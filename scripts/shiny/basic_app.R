library(shiny)
library(DBI)
library(RSQLite)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(leafpop)
library(shinymanager)
library(bcmaps)

creds <- data.frame(read.table("www/creds.txt", sep = ",", header = T))

credentials <- data.frame(
  user = creds$user,
  password = sapply(creds$password, scrypt::hashPassword),
  is_hashed_password = TRUE,
  stringsAsFactors = FALSE
)

my_theme <- bslib::bs_theme(
  bootswatch = 'lumen',
  heading_font = 'Merriweather,serif',
  base_font = 'Source Sans Pro',
  "sidebar-bg" = '#ADD8E7'
)

table_choices <- c(
  'Creel - All', 'Raw Scale Aging', 'Tag Data', 'Spring Marking',
  'Nest Data', 'Angling Basok', 'Angling Derby', 'Abundance Data', 'Scale 500',
  'Creel - Survey Responses', 'Creel - Angler', 'Creel - Detailed', 'Sweltzer Creek',
  'Creel - survey data', 'Creel - Angler Info', 'Creel - fish Catch',
  'Creel - fishing Details', 'Creel - ice data', 'Creel - survey Answers',
  'Creel - Survey Questions', 'Creel - Weather details'
)
table_choices <- sort(table_choices)

# Add a logout button container to the UI
ui <- page_sidebar(
  title = tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
    # Left: App Title
    tags$span("Cultus Lake - Data", style = "font-size: 20px; font-weight: bold;"),
    # Right: Logout Button
    div(
      uiOutput("logout_ui"),
      style = "margin-left: auto;"  # Pushes the logout button to the far right
    )
  ),
  sidebar = sidebar(
    pickerInput(
      inputId = "table_name",
      label = "Select table",
      selected = 'Tag Data',
      choices = c(table_choices),
      multiple = FALSE,
      options = pickerOptions(liveSearch = TRUE)
    ),
    uiOutput('date_filter'),
    uiOutput("dynamicFilter"),
    bslib::layout_column_wrap(
      width = 1,
      downloadButton(
        "download_xlsx",
        "Download file (.xlsx)",
        style = "color: #ffffff; background-color: #27ae22; border-color: #277c24;"
      )
    )
  ),
  theme = my_theme,
  card(
    tabsetPanel(
      tabPanel("Interactive Table",
               div(style = "overflow-x: scroll; overflow-y: scroll;", DT::DTOutput("queried_table"))),
      tabPanel("Metadata",
               tabsetPanel(
                 tabPanel("Creel - All",
                          img(src = "Creel - All.png", width = 1200, height = 600, alt = "Creel - All")
                 ),
                 tabPanel("Creel - Detailed",
                          img(src = "Creel - Detailed.png", width = 1200, height = 600, alt = "Creel - Detailed")
                 ),
                 tabPanel("Creel - Angler Info",
                          img(src = "Creel - Angler Info.png", width = 1000, height = 600, alt = "Creel - Angler Info")
                 ),
                 tabPanel("Creel - Survey Response",
                          img(src = "Creel - Survey Response.png", width = 1000, height = 600, alt = "Creel - Survey Response")
                 )
               )
      ),
      tabPanel("Contact",
               card(
                 a(
                   actionButton(inputId = "email1", label = "Contact Admin",
                                icon = icon("envelope", lib = "font-awesome")),
                   href = "mailto:john.phelan@gov.bc.ca"
                 )
               )
      )
    )
  ),
)

# Secure the app using shinymanager
ui <- secure_app(ui)


server <- function(input, output, session) {
  
  
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  observe({
    print(input$shinymanager_where)
  })
  observeEvent(input$shinymanager_where, { 
    if(input$shinymanager_where == 'application'){
  
  
    
  # if(!stringr::str_detect(getwd(),'/www/')){
  #   setwd(paste0(getwd(),'/www/'))
  # }
  # if(!stringr::str_detect(getwd(),'scripts/shiny/')){
  #   setwd(paste0(getwd(),'/scripts/shiny/www/'))
  # }
  
      
      
  my_db = DBI::dbConnect(RSQLite::SQLite(),"www/CultusData.sqlite")

  database_tbl_names = DBI::dbListTables(my_db)
  # nest_dat = DBI::dbGetQuery(my_db, statement = "select * from nestRaw")
  
  database_tbl_names = c("abundanceCapture", "anglerInfo", "anglingCapBasok", "anglingDerby",
                    "fishCatch", "fishCaught", "fishingDetails", "iceData", "nestRaw",
                    "recapture", "scale500", "scaleColNameKey", "scaleRawTable",
                    "springMarking", "surveyAnswers", "surveyData", "surveyQuestions", "SweltzerCreek",
                    "tagData", "weatherDetails", "fishCaught")
  
  queries_tbl = readxl::read_excel('www/table_name_to_query_lookup_tbl.xlsx')
  
  #Depending on the human-readable table name chosen by the user, 
  # inform the SQL query that is performed.
  
  initial_query = reactive({
    # Find all columns in the queries_tbl that have an X for the table name that 
    # has been chosen.
    selected_query_tbl = queries_tbl[queries_tbl$TableName == input$table_name,]
    
    list_of_queries = selected_query_tbl |> 
      tidyr::pivot_longer(cols = -TableName) |> 
      dplyr::filter(value == 'X') |> 
      dplyr::mutate(query = paste0('select * from ',name)) |> 
      dplyr::pull(query)
    
    list_of_queries
  })
  
  # Loop over the list of queries, combining tables afterwards
  
  queried_tbl = reactive({
    merged_data = purrr::map(initial_query(), ~ {
      DBI::dbGetQuery(my_db,.x) |> 
        dplyr::mutate(dplyr::across(dplyr::everything(), \(x) as.character(x)))
    }) |> 
      purrr::reduce(dplyr::full_join)
    
    # Ensure that the time and date columns are in the proper format!
    # if('time' %in% names(merged_data)){
    #   merged_data = merged_data |> 
    #     dplyr::mutate(time = dplyr::case_when(
    #       stringr::str_detect(time, ':[0-9]{2}$') ~ lubridate::ymd_hms(time),
    #       T ~ lubridate::ymd(time)
    #     ))
    # }
    if('date' %in% names(merged_data)){
      merged_data$date = lubridate::ymd(merged_data$date)
    }
    if ("date" %in% colnames(merged_data)) {
      # Reorder: make "date" the first column, followed by all other columns
      merged_data <- merged_data[, c("date", setdiff(colnames(merged_data), "date"))]
    }
    merged_data
  })
  
  # We may want an additional set of filters based on WHAT columns our table has...
  # To do this, we could dynamically render new UI pieces based on our server script.
  # output$conditional_time_filter = renderUI({
  #   if(!'time' %in% names(queried_tbl())) return(NULL)
  #   if('time' %in% names(queried_tbl())){
  #     shiny::sliderInput(
  #       inputId = 'tbl_time_filter',
  #       label = 'Time',
  #       min = min(queried_tbl()$time, na.rm=T),
  #       max = max(queried_tbl()$time, na.rm=T),
  #       value = c(min(queried_tbl()$time, na.rm=T),max(queried_tbl()$time, na.rm=T))
  #     )
  #   }
  # })
  
  output$date_filter = renderUI({
    if (!'date' %in% names(queried_tbl())) return(NULL)
    
    if ('date' %in% names(queried_tbl())) {
      shiny::sliderInput(
        inputId = 'tbl_date_filter',
        label = 'Date',
        min = as.Date(min(queried_tbl()$date, na.rm = TRUE)),  # Ensure Date type
        max = as.Date(max(queried_tbl()$date, na.rm = TRUE)),  # Ensure Date type
        value = c(as.Date(min(queried_tbl()$date, na.rm = TRUE)),
                  as.Date(max(queried_tbl()$date, na.rm = TRUE))),
        timeFormat = "%Y-%m-%d"  # Format the date display
      )
    }
  })
  
  output$ageFilter = renderUI({
    
    if (!'AgeClass' %in% names(queried_tbl())) return(NULL)
    
    if ('AgeClass' %in% names(queried_tbl())) {
      shiny::selectInput(
        inputId = 'age_class_filter',
        label = 'Select Age Class:',
        choices = sort(unique(queried_tbl()$AgeClass)), 
        selected = NULL, 
        multiple = TRUE 
      )
    }
  })
  
  # output$dynamicFilter = renderUI({
  #   # Get the current table
  #   table_data <- queried_tbl()
  #   
  #   # Return NULL if the table is empty
  #   if (nrow(table_data) == 0) return(NULL)
  #   
  #   # Find columns with 10 or fewer non-NA unique values
  #   cols_to_filter <- names(table_data)[sapply(table_data, function(col) {
  #     unique_vals <- unique(col[!is.na(col)]) # Exclude NA values
  #     length(unique_vals) > 0 && length(unique_vals) <= 10
  #   })]
  #   
  #   # If no columns qualify, return NULL
  #   if (length(cols_to_filter) == 0) return(NULL)
  #   
  #   # Create dropdowns for each qualifying column
  #   tagList(
  #     lapply(cols_to_filter, function(col) {
  #       selectizeInput(
  #         inputId = paste0(col, "_filter"),
  #         label = paste("Filter by", col),
  #         choices = NULL, # Choices will be populated dynamically
  #         selected = NULL,
  #         multiple = TRUE
  #       )
  #     })
  #   )
  # })
  

  output$dynamicFilter = renderUI({
    # Get the current table
    table_data <- queried_tbl()
    
    # Return NULL if the table is empty
    if (nrow(table_data) == 0) return(NULL)
    
    # Find columns with 10 or fewer non-NA unique values
    cols_to_filter <- names(table_data)[sapply(table_data, function(col) {
      unique_vals <- unique(col[!is.na(col)]) # Exclude NA values
      length(unique_vals) > 0 && length(unique_vals) <= 10
    })]
    
    # If no columns qualify, return NULL
    if (length(cols_to_filter) == 0) return(NULL)
    
    # Create dropdowns for each qualifying column
    accordion(id = "my_acc", accordion_panel( "Filter by: ",
    tagList(
      lapply(cols_to_filter, function(col) {
        selectizeInput(
          inputId = paste0(col, "_filter"),
          label = paste("Filter by", col),
          choices = NULL, # Choices will be populated dynamically
          selected = NULL,
          multiple = TRUE
        )
      })
    )
    )
    )
  })

  
  observe({
    # Get the current table
    table_data <- queried_tbl()
    
    # Return if the table is empty
    if (nrow(table_data) == 0) return()
    
    # Find columns with 10 or fewer non-NA unique values
    filter_columns <- names(table_data)[sapply(table_data, function(col) {
      unique_vals <- unique(col[!is.na(col)]) # Exclude NA values
      length(unique_vals) > 0 && length(unique_vals) <= 10
    })]
    
    # Populate each dropdown with valid unique values
    for (col in filter_columns) {
      updateSelectizeInput(
        session,
        inputId = paste0(col, "_filter"),
        choices = sort(unique(table_data[[col]][!is.na(table_data[[col]])])), # Exclude NA values
        server = TRUE
      )
    }
  })
  
  # output$conditional_age_class = renderUI({
  #   if(!'AgeClass' %in% names(queried_tbl())) return(NULL)
  #   if('AgeClass' %in% names(queried_tbl())){
  #     shinyWidgets::pickerInput(
  #       inputID = 'ageClass',
  #       label = 'Age group',
  #       multiple = TRUE,
  #       choices = (unique(queried_tbl$ageClass))
  #     )}
  # })
  
  final_table = reactive({
    output = queried_tbl()
    
    if (nrow(output) == 0) return(output)
    
    # Apply date filter only if the date column exists
    if (!is.null(input$tbl_date_filter) && "date" %in% names(output)) {
      output = output |> 
        dplyr::filter(
          date >= as.Date(input$tbl_date_filter[1]),
          date <= as.Date(input$tbl_date_filter[2])
        )
    }
    if (!is.null(input$age_class_filter) && "AgeClass" %in% names(output)) {
      output = output |> 
        dplyr::filter(AgeClass %in% input$age_class_filter) # Match selected values
    }
    
    filter_columns <- names(output)[sapply(output, function(col) {
      unique_vals <- unique(col[!is.na(col)]) # Exclude NA values
      length(unique_vals) > 0 && length(unique_vals) <= 10
    })]
    
    # Apply filters based on the inputs
    for (col in filter_columns) {
      filter_input <- input[[paste0(col, "_filter")]]
      if (!is.null(filter_input) && length(filter_input) > 0) {
        output <- output |> 
          dplyr::filter(.data[[col]] %in% filter_input)
      }
    }
    
    output
  })
  
 
  
  
  
  # names(testytest) = stringr::str_extract(initial_query,'[a-zA-Z]+$')
  # dplyr::full_join(testytest$surveyQuestions,testytest$surveyAnswers)
  # purrr::reduce(dplyr::full_join)
  
  
  
  # If we are wanting to respond to user inputs, we use inputs 
  # in the UI and then track their values here. Since they can
  # change, we need to code certain objects in our server script
  # to be "reactive"
  table_chosen = reactive({
    input$table_name
  })
  
  # table_queried = reactive({
  #   DBI::dbGetQuery(my_db,
  #                   paste0("select * from ",table_chosen()))
  # })
  
  output$queried_table = DT::renderDT({
    final_table()
  })
  
  output$my_leaf = renderLeaflet({
    
    # if(input$table_name == "nestRaw") browser()
    bc = bcmaps::bc_bound() |> sf::st_transform(4326)
    l = leaflet() |> 
      addTiles() |> 
      addPolygons(
        data = bc,
        color = 'black',
        weight = 2,
        fillColor = 'transparent'
      )
    
    # IF the table we've chosen to look at has some lat/long column, let's make a map!
    if("lat" %in% names(final_table()) | "easting" %in% names(final_table()) | "latitude" %in% names(final_table())){
      if("easting" %in% names(final_table())){
        
        leafdat = final_table() |> 
          dplyr::filter(!is.na(easting)) |> 
          sf::st_as_sf(coords = c("easting","northing"), crs = 32610) |> 
          sf::st_transform(4326)
        
        # Let's say we want to show a table of various columns for each point
        # upon mouse click. We can use the package leafpop to do so!
        leaf_table = leafdat |> 
          sf::st_drop_geometry() |> 
          #dplyr::select(date, guarding, depth, diameter) |> 
          leafpop::popupTable()
      }else{
        leafdat = final_table() |> 
          dplyr::filter(!is.na(latitude)) |> 
          sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)
        
        # Let's say we want to show a table of various columns for each point
        # upon mouse click. We can use the package leafpop to do so!
        leaf_table = leafdat |> 
          sf::st_drop_geometry() |> 
          #dplyr::select(date, guarding, depth, diameter) |> 
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
  
  # Inform what the action buttons actually do!
  output$download_xlsx = downloadHandler(
    filename = function() {
      paste0('Cultus_Lake_',stringr::str_replace_all(input$table_name,' ','_'),'_Query.xlsx')
    },
    content = function(file) {
      openxlsx::write.xlsx(final_table(), file)
    }
  )
  
  observeEvent(input$emailButton, {
    email_address <- "john.phelan@gov.bc.ca"
    url <- paste0("mailto:", email_address)
    
    # Ensure correct behavior by using system commands to open the URL
    if (.Platform$OS.type == "windows") {
      shell.exec(url)
    } else {
      system(paste("xdg-open", shQuote(url)))
    }
  })
  
  # Render logout button with custom style
  output$logout_ui <- renderUI({
    actionButton(
      inputId = "logout",
      label = "Logout",
      style = "background-color: #CC5500; color: white; border-color: #6A0000;",
      class = "btn"
    )
  })
  
  # Logout logic
  observeEvent(input$logout, {
    user_logged_in(FALSE)  # Reset login state
    session$reload()
  })
  
    } 
  })
}

shinyApp(ui, server)
