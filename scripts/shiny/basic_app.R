library(shiny)
library(DBI)
library(RSQLite)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(leafpop)

# setwd(paste0(getwd(),"/scripts/shiny"))
table_choices = c("abundanceCapture", "anglerInfo", "anglingCapBasok", "anglingDerby", 
                  "fishCatch", "fishCaught", "fishingDetails", "iceData", "nestRaw", 
                  "recapture", "scale500", "scaleColNameKey", "scaleRawTable", 
                  "springMarking", "surveyAnswers", "surveyData", "surveyQuestions", 
                  "tagData", "weatherDetails")

ui <- page_sidebar(
  sidebar = sidebar(
    pickerInput(
      inputId = "table_name",
      label = "IM A LABEL",
      choices = c(table_choices),
      multiple = F,
      options = pickerOptions(
        liveSearch = TRUE
      )
    )
  ),
  card(
    tabsetPanel(
      tabPanel("Interactive Table", DT::DTOutput("queried_table")),
      tabPanel("Interactive Map", leafletOutput("my_leaf"))
    )
  )
)

server <- function(input, output, session) {
  my_db = DBI::dbConnect(RSQLite::SQLite(),"www/CultusData.sqlite")
  all_tables = DBI::dbListTables(my_db)
  # nest_dat = DBI::dbGetQuery(my_db, statement = "select * from nestRaw")
  
  bc = bcmaps::bc_bound() |> sf::st_transform(4326)
  
  # If we are wanting to respond to user inputs, we use inputs 
  # in the UI and then track their values here. Since they can
  # change, we need to code certain objects in our server script
  # to be "reactive"
  table_chosen = reactive({
    input$table_name
  })
  
  table_queried = reactive({
    DBI::dbGetQuery(my_db,
                    paste0("select * from ",table_chosen()))
  })
  
  output$queried_table = DT::renderDT({
    table_queried()
  })
  
  output$my_leaf = renderLeaflet({
    
    # if(input$table_name == "nestRaw") browser()

    l = leaflet() |> 
      addTiles() |> 
      addPolygons(
        data = bc,
        color = 'black',
        weight = 2,
        fillColor = 'transparent'
      )
    
    # IF the table we've chosen to look at has some lat/long column, let's make a map!
    if("lat" %in% names(table_queried()) | "easting" %in% names(table_queried())){
      if("easting" %in% names(table_queried())){
        leafdat = table_queried() |> 
          dplyr::filter(!is.na(easting)) |> 
          sf::st_as_sf(coords = c("easting","northing"), crs = 32610) |> 
          sf::st_transform(4326)
        
        # Let's say we want to show a table of various columns for each point
        # upon mouse click. We can use the package leafpop to do so!
        leaf_table = leafdat |> 
          sf::st_drop_geometry() |> 
          dplyr::select(date, guarding, depth, diameter) |> 
          leafpop::popupTable()
      }
      
      l = l |> 
        addMarkers(
          data = leafdat,
          # Popup is for mouse clicks
          popup = lapply(leaf_table, htmltools::HTML),
          # label is for mouse-overs i.e. hovering
          label = ~date
        )
    }
    l
  })
}

shinyApp(ui, server)

