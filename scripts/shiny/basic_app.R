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

table_choices <- c("Scale aging", "Acoustic tag data", "Electrofishing", "Nest data", "Angling Basok", "Angling derby", 
                   "Recapture data", "Scale 500", "Sweltzer creek", "Creel - Fish Details", 
                   "Creel - Fishing Results", "Creel - Fisher Survey", "Creel - Shifts", "Creel - Weather Conditions", 
                    "High reward tags", "High reward tags - claimed","Creel - Survey Responses",
                   "Creel - Survey Questions", "Temperature - Shore", "Temperature - CLASS")
table_choices<-sort(table_choices)

creel_tables<-c("Creel - Fish Details","Creel - Fisher Survey", "Creel - Fishing Results","Creel - Shifts",
                "Creel - Survey Responses","Creel - Weather Conditions", "Creel - Survey Questions")
creel_tables<-sort(creel_tables)

table_col_search<-c("NA","pitTagNo", "transmitter", "receiver", "stationName", "receiver_name", "location", "acousticTagNo", "length", "weight", "tagID", "ScaleBookNo")
table_col_search <- sort(table_col_search)


template_list<-c("Abundance estiamte" = "", "angling Basok and Buck", "Angling Derby", "High Reward Tags", "High Reward Tags Claimed",
                 "Nest Data", "scale 500", "Scale Aging", "Spring Marking", "Sweltzer Creek", "Tag Data", "Scale Column Name Key")

# Add a logout button container to the UI
ui <- page_sidebar(
  
  # add a css here and make the styling percentage based
  # 
  includeCSS("www/style.css"),
  
  title = tags$div(
    class = "page-title-bar",
    tags$span("Cultus Lake - Data", class = "title-text"),
    div(
      uiOutput("logout_ui"),
      class = "logout-button"
    )
  ),
  
  
  sidebar = sidebar(
    div(
    div(
      pickerInput(
        inputId = "table_name",
        label = "Select table",
        selected = 'Nest data',
        choices = c(table_choices),
        multiple = FALSE,
        options = pickerOptions(liveSearch = TRUE)
      ),
      class = "picker-input"
    ),
    div(
      uiOutput('date_filter'),
      class = "date-filter"    
    ),
    uiOutput("dynamicFilter"),
    
    )
  ),
  theme = my_theme,
  
  tags$script(HTML('
  $(document).ready(function(){
    var tooltipTriggerList = [].slice.call(document.querySelectorAll("[data-bs-toggle=\'tooltip\']"));
    var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
      return new bootstrap.Tooltip(tooltipTriggerEl);
    });
  });
')),
  
  
  card(
    tabsetPanel(
        
      tabPanel("Selected Table",
               downloadButton(
                 class = "download-button",
                 "download_xlsx",
                 "Download file (.xlsx)"),
               div(class = "main-table",
                   DT::DTOutput("queried_table")
               )
      ),
      
      # New Tab for displaying the column names from the found tables
      tabPanel("Search By Column",
               div(
                 class = "column-search-header",
                 p("This finds the selected column name across")
               ),
               downloadButton(
                 class = "download-button",
                 "download_filtered_tables",
                 "Download Tables (.zip)"
               ),
               pickerInput(
                 inputId = "column_name",
                 label = "Select column to search",
                 selected = "NA",
                 choices = c(table_col_search),
                 multiple = FALSE,
                 options = pickerOptions((liveSearch = TRUE))
               ),
               
               uiOutput("columns_in_selected_tables")
               
               
      ),
  
      # How will this look?
      # We should have a drop down menu in the panel, where we can choose the creel tables
      # There are multiple choices
      tabPanel("Creel - Table Joins",
               
               # gets the names of the tables
               
               selectizeInput(
                 inputId = "Creel_selections",
                 label = "Select Creel tables to join",
                 choices = creel_tables,
                 multiple = TRUE,
                 options =list(create = TRUE)
               ),
               downloadButton(
                 class = "download-button",
                 "download_creel_tables",
                 "Download Creel Tables (.xlsx)"
               ),
               #we need to join the tables in the server, then display them!
               div(class = "main-table", DT::DTOutput("creel_joined_tables")),
               
               uiOutput("selected_creel_tables")
      ),
      
      tabPanel("Metadata",
               div(
                 class = "metadata-header",
                 tagList(
                   p("Each tab contains a list and descriptor for each column in the current tables."),
                   p("The unique key(s) for each table is listed."),
                   p("Hover over the column names to find a description for that specific column.")
                 )
               ),
               tabsetPanel(
                 tabPanel("Creel - Shifts",
                          creelMetadataBox(
                            title = "Creel - Shifts",
                            keys = "date, survey number",
                            fields = c(
                              "Date of the survey" = "Date",
                              "Unique ID for the survey" = "Survey number",
                              "Person conducting the survey" = "Surveyor",
                              "Time the shift started" = "Shift start time",
                              "Time the shift ended" = "Shift end time",
                              "Total hours worked during the shift" = "Hours worked"
                            )
                          )
                 ),
                 tabPanel("Creel - Fish Details",
                          creelMetadataBox(
                            title = "Creel - Fish Details",
                            keys = "date, survey number, Fish ID",
                            fields = c(
                              "Unique ID for the record" = "Survey number",
                              "Date of record" = "Date",
                              "Time of record" = "Time",
                              "Fish number caught on the current date" = "Fish No",
                              "Species of the fish caught" = "Spp",
                              "Length of fish in mm" = "length",
                              "Weight of fish in g" = "weight",
                              "Pit tag number, if present" = "PitTagNo",
                              "Notes/comments from operator" = "notes",
                              "Unique fish ID, independent of date" = "fishID",
                              "Acoustic tag number, if present" = "acousticTagNo"
                            )
                          )
                 ),
                 
                 tabPanel("Creel - Fish Results",
                          creelMetadataBox(
                            title = "Creel - Fish Results",
                            keys = "date, survey number",
                            fields = c(
                              "Unique ID for the record" = "Survey number",
                              "Date of record" = "Date",
                              "Time of record" = "Time",
                              "Total number of fish caught (may be released)" = "totCaught",
                              "Total number of fish retained" = "totRetained",
                              "Total number of small mouth bass caught" = "noSMBc",
                              "Total number of sockeye salmon(?) caught" = "NoKOc",
                              "Total number of cutthroat trout caught" = "NoCTc",
                              "Total number of rainbow trout caught" = "NoRBc",
                              "Total number of bull trout caught" = "NoBTc",
                              "Total number of lake trout caught" = "NoLTc",
                              "Total number of other species caught" = "NoOtherSppc",
                              "Total number of small mouth bass retained" = "noSMBr",
                              "Total number of sockeye salmon(?) retained" = "NoKOr",
                              "Total number of cutthroat trout retained" = "NoCTr",
                              "Total number of rainbow trout retained" = "NoRBr",
                              "Total number of bull trout retained" = "NoBTr",
                              "Total number of lake trout retained" = "NoLTr",
                              "Total number of other species retained" = "NoOtherSppr",
                              "Why did the angler release any of the fish caught" = "releaseReason",
                              "Hours fished per person" = "personHours",
                              "Number of anglers in the group" = "noAnglers",
                              "Number of rods being used" = "noRods",
                              "Where were the anglers fishing (dock or boat etc)" = "vessl",
                              "Preferred species for angling" = "preferredSpp",
                              "Where was the survey conducted" = "site"
                            )
                          )
                 ),
                 
                 tabPanel("Creel - Fisher Survey",
                          creelMetadataBox(
                            title = "Creel - Fisher Survey",
                            keys = "date, survey number, anglerID",
                            fields = c(
                              "Date of the survey" = "Date",
                              "Unique ID for the survey" = "Survey number",
                              "Time of survey" = "time",
                              "Reported gender" = "gender",
                              "Age category of participant" = "AgeClass",
                              "Type of fishing license held" = "licensePeriod",
                              "Resident of Canada?" = "residency",
                              "Where the participant is from" = "cityProvienceCountry",
                              "Post code" = "Post Code",
                              "Other information provided" = "notes",
                              "Unique ID for angler" = "AnglerID"
                            )
                          )
                 ),
               )
      ),
      
      
      tabPanel("Download template forms",
               div(
                 class = "under-construction",
                 p("Under construction!"),
               ),
               card(
                 selectizeInput(
                   inputId = "template_selection",
                   label = "Select template to download",
                   choices = template_list,
                   multiple = TRUE,
                   options =list(create = TRUE)
                 ),
               ),
                 downloadButton(
                   class = "download-button",
                   "download_template",
                   "Download template (.csv)"
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
                    "tagData", "weatherDetails", "fishCaught", "highRewardTags", "highRewardTagsClaimed")
  
  queries_tbl = readxl::read_excel('www/table_name_to_query_lookup_tbl.xlsx')
  
  #Depending on the human-readable table name chosen by the user, 
  # inform the SQL query that is performed.
  
  ######################################################################################################
  
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
      purrr::reduce(dplyr::left_join)
    
    
    if('date' %in% names(merged_data)){
      merged_data$date = lubridate::ymd(merged_data$date)
    }
    if ("date" %in% colnames(merged_data)) {
      # Reorder: make "date" the first column, followed by all other columns
      merged_data <- merged_data[, c("date", setdiff(colnames(merged_data), "date"))]
    }
    merged_data
  })
  
  
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
    
    
    curr_table = queries_tbl[queries_tbl$TableName == input$table_name,]
    
    if (curr_table$TableName == "Scale aging") {

      output <- tryCatch({
        output |> 
          dplyr::filter(!detailedNames %in% c("start_time", "end_time")) |>  
          dplyr::select(-ageType) |>  
          tidyr::pivot_wider(names_from = detailedNames, values_from = result)
      },
      error = function(e) {
        message("Pivoting failed: ", e$message)
        return(output)  # Return original data if an error occurs
      })
    }

    output
  })
  
  
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
  
  
  ################################################################
  
  # Reactive to get tables containing the selected column
  tables_with_column <- reactive({
    selected_column <- input$column_name
    tables <- dbListTables(my_db)
    tables_with_column <- c()
    
    for (table in tables) {
      column_info <- dbGetQuery(my_db, paste0("PRAGMA table_info(", table, ")"))
      
      if (selected_column %in% column_info$name) {
        tables_with_column <- c(tables_with_column, table)
      }
    }
    
    return(tables_with_column)  # Return the names of tables with the selected column
  })
  
  
  
  # Render the column names in the "Column Names" tab
  output$columns_in_selected_tables <- renderUI({
    selected_column <- input$column_name
    tables <- tables_with_column()  # Get the table names with the selected column
    
    if (length(tables) > 0) {
      bslib::card(
        bslib::card_body(
          tagList(
            tags$h4("Tables with Selected Variable"),
            tags$p(
              style = "margin-bottom: 15px;",
              paste0("The following tables include the column '", selected_column, "':")
            ),
            div(
              style = "padding-left: 10px;",
              HTML(paste0("• ", tables, collapse = "<br>"))
            )
          )
        ),
        style = "background-color: #eaeaea; border: 1px solid #0a0d13; padding: 20px; border-radius: 8px;"
      )
    } else {
      bslib::card(
        bslib::card_body(
          tags$p(
            style = "color: #ff0000; font-weight: bold;",
            "No tables found with the selected column."
          )
        ),
        style = "background-color: #fce4e4; border: 1px solid #f5c6cb; padding: 20px; border-radius: 8px;"
      )
    }
  })
  
  
  
  # Handle downloading of tables as a zip file
  output$download_filtered_tables <- downloadHandler(
    filename = function() {
      paste0("tables_with_column_", input$column_name, ".zip")
    },
    content = function(file) {
      # Write tables to a zip file
      
      tmp_dir <- paste0(tempdir(), Sys.Date())
      dir.create(tmp_dir)
      #zip_file <- file.path(tmp_dir, "tables_with_column.zip")
      
      files <- c()
      tables <- tables_with_column()
      
      for (table in tables) {
        table_data <- dbGetQuery(my_db, paste0("SELECT * FROM ", table))
        table_file <- file.path(tmp_dir, paste0(table, ".csv"))
        
        write.csv(table_data, table_file, row.names = FALSE)
        if (file.info(table_file)$size == 0) {
          stop(paste("Error: File", table_file, "is empty"))
        }
        files <- c(files, table_file)
      }
      
      zip::zip(zipfile = file, files = dir(tmp_dir), root = tmp_dir)
      
      #file.copy(zip_file, file)
    }
  )
 
  ##################################################################################
  
  # Reactive to get tables containing the selected column
  selected_table_creel <- reactive({
    req(input$Creel_selections)
    
    # Load queries and filter by selected tables
    selected_query_tbl <- queries_tbl %>%
      dplyr::filter(TableName %in% input$Creel_selections)
    
    # Create SQL queries for the selected tables
    list_of_queries <- selected_query_tbl %>%
      tidyr::pivot_longer(cols = -TableName) %>%
      dplyr::filter(value == 'X') %>%
      dplyr::mutate(query = paste0('SELECT * FROM ', name)) %>%
      dplyr::pull(query)
    
    # Ensure it's a named list
    list_of_queries <- setNames(as.list(list_of_queries), selected_query_tbl$TableName)
    
    list_of_queries
  })
  
  
  # Reactive to get tables containing the selected column
  creel_joins <- reactive({
    table_queries <- selected_table_creel()
    
    # Fetch data from database
    table_data <- purrr::map(table_queries, ~ DBI::dbGetQuery(my_db, .x))
    table_data <- as.list(table_data)
    
    # Debugging: Print which tables are fetched
    print(names(table_data))
    
    # Check if creelSurveyQuestions and creelSurveyAnswers are selected
    has_questions <- "Creel - Survey Questions" %in% names(table_data)
    has_answers <- "Creel - Survey Answers" %in% names(table_data)
    
    # Handle creelSurveyQuestions & creelSurveyAnswers
    if (has_questions & has_answers) {
      # Properly join on questionID
      questions_answers <- dplyr::inner_join(
        table_data$`Creel - Survey Questions`,
        table_data$`Creel - Survey Answers`,
        by = "questionID"
      )
      table_data$questions_answers <- questions_answers
      
      # Remove original tables after joining
      table_data <- table_data[!(names(table_data) %in% c("creelSurveyQuestions", "creelSurveyAnswers"))]
    } else if (has_questions) {
      # If only creelSurveyQuestions is selected, keep it
      table_data$questions_answers <- table_data$`Creel - Survey Questions`
      table_data <- table_data[!(names(table_data) %in% c("creelSurveyQuestions"))]
    } else if (has_answers) {
      # If only creelSurveyAnswers is selected, keep it
      table_data$questions_answers <- table_data$`Creel - Survey Answers`
      table_data <- table_data[!(names(table_data) %in% c("creelSurveyAnswers"))]
    }
    
    # Join remaining tables on date and surveyNumber
    remaining_tables <- table_data[names(table_data) != "questions_answers"]
    valid_tables <- list()
    for (tbl_name in names(remaining_tables)) {
      df <- remaining_tables[[tbl_name]]
      if (!is.data.frame(df) || nrow(df) == 0) next
      if (all(c("date", "surveyNumber") %in% names(df))) {
        valid_tables[[tbl_name]] <- df
      }
    }
    
    # Perform the join
    if (length(valid_tables) == 1) {
      creel_joined_data <- valid_tables[[1]]
    } else if (length(valid_tables) > 1) {
      creel_joined_data <- Reduce(function(df1, df2) {
        dplyr::full_join(df1, df2, by = c("date", "surveyNumber"), suffix = c(".x", ".y"))
      }, valid_tables)
    } else {
      creel_joined_data <- NULL
    }
    
    # If questions_answers exists, merge it properly
    if ("questions_answers" %in% names(table_data)) {
      if (is.null(creel_joined_data)) {
        creel_joined_data <- table_data$questions_answers
      } else {
        # Properly join, don't just append!
        creel_joined_data <- dplyr::full_join(creel_joined_data, table_data$questions_answers, by = "questionID")
      }
    }
    
    #if questions_answers is in the list of table names, then we need to drop "questionID"
    # and then we need to pivot wider, where question becomes column names and the values in these
    # columns comes from answers
  
    # the rows should then be mered on surveyNumber and date
    if ("questions_answers" %in% names(table_data) & "date" %in% names(creel_joined_data)) {
      creel_joined_data <- creel_joined_data |> 
        dplyr::select(-questionID) |> 
        tidyr::pivot_wider(names_from = question, values_from = answer) |> 
        dplyr::group_by(date, surveyNumber) |> 
        dplyr::summarise(across(everything(), ~ dplyr::first(unlist(.))), .groups = "drop")
    }
    
    
    # Handle empty case
    if (is.null(creel_joined_data)) {
      creel_joined_data <- data.frame(Message = "No Data Available")
    }
    return(creel_joined_data)
  })
  
  
  
  output$creel_joined_tables <- DT::renderDT({
    
    creel_data_dynamic<-creel_joins()
    
    # Render DataTable
    DT::datatable(creel_data_dynamic, options = list(scrollX = TRUE, scrollY = TRUE))
    
  })

  # Handle downloading of tables as a zip file
  output$download_creel_tables <- downloadHandler(
    filename = function() {
      paste0('Cultus_Lake_Creel_Survey_Joins.xlsx')
    },
    content = function(file) {
      openxlsx::write.xlsx(creel_joins(), file)
    }
  )
  
  
  
  
  ###################################################################################
  output$my_leaf = renderLeaflet({
    
    
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
    session$reload()
    user_logged_in(FALSE)  # Reset login state
  })
  
    } 
  })
}

shinyApp(ui, server)
