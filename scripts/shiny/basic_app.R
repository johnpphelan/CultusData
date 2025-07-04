library(shiny)
library(DBI)
library(RSQLite)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(leafpop)
library(shinymanager)
library(bcmaps)
library(htmltools)
library(plotly)

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

creelMetadataBox <- function(title, keys, fields) {
  HTML(sprintf(
    "<div class='container' style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);'>
      <h4 style='color: #34495e; font-weight: bold; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>%s</h4>
      <p><strong>Keys:</strong> <span style='color: #3498db;'>%s</span></p>
      <p><strong>Contents:</strong></p>
      <ul style='list-style-type: none; padding-left: 10px;'>%s</ul>
    </div>",
    title,
    keys,
    paste(sprintf(
      "<li><strong title='%s' style='cursor: help;'>%s</strong></li>",
      names(fields),
      unname(fields)
    ), collapse = "\n")
  ))
}

#Not needed - but this is for keeping variables within the main project environment
source("scripts/ui.R", local = T)

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

      
  my_db = DBI::dbConnect(RSQLite::SQLite(),"www/CultusData.sqlite")

  database_tbl_names = DBI::dbListTables(my_db)
  
  
  ######################################################################################################
  
  
  source("scripts/server_reactives.R", local = T)
  source("scripts/server_renders.R", local = T)
  source("scripts/server_observe.R", local = T)
  source("scripts/server_downloads.R", local = T)

  
  
  
  
  
    } 
  })
}

shinyApp(ui, server)
