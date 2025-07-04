
# the database tables names, and the queries excel table
database_tbl_names = c("abundanceCapture", "anglerInfo", "anglingCapBasok", "anglingDerby",
                       "fishCatch", "fishCaught", "fishingDetails", "iceData", "nestRaw",
                       "recapture", "scale500", "scaleColNameKey", "scaleRawTable",
                       "springMarking", "surveyAnswers", "surveyData", "surveyQuestions", "SweltzerCreek",
                       "tagData", "weatherDetails", "fishCaught", "highRewardTags", "highRewardTagsClaimed")

queries_tbl = readxl::read_excel('www/table_name_to_query_lookup_tbl.xlsx')




#Get the inital table - match the names and return the list of queries - the tables being looked for
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

# this is getting the filtering options for the sidebar
# it is also pivoting a table for some reason
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

plot_inputs <- reactiveValues(x = NULL, y = NULL)

observe({
  df <- final_table()
  if (nrow(df) == 0) return()
  
  selectable_cols <- names(df)[sapply(df, function(col) {
    # Skip if it's a Date
    if (inherits(col, "Date")) return(TRUE)  # Dates are allowed
    
    # Try numeric conversion
    suppressWarnings({
      numeric_col <- as.numeric(as.character(col))
    })
    
    # Use numeric if it's valid (not all NAs) and not identical to original character
    is_valid_numeric <- !all(is.na(numeric_col)) &&
      !inherits(col, "Date") &&
      is.numeric(numeric_col)
    
    is_valid_numeric || is.numeric(col) || is.factor(col) || is.character(col)
  })]
  
  # Only change selection if inputs are null (first time setup)
  current_x <- isolate(input$plot_x)
  current_y <- isolate(input$plot_y)
  
  updatePickerInput(
    session, "plot_x",
    choices = selectable_cols,
    selected = if (is.null(current_x)) {
      if ("date" %in% selectable_cols) "date" else selectable_cols[1]
    } else current_x
  )
  
  updatePickerInput(
    session, "plot_y",
    choices = selectable_cols,
    selected = if (is.null(current_y)) {
      setdiff(selectable_cols, current_x)[1]
    } else current_y
  )
})


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


