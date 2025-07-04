
# gets the columns from the current table for filtering
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

observeEvent(input$plot_x, {
  plot_inputs$x <- input$plot_x
})

observeEvent(input$plot_y, {
  plot_inputs$y <- input$plot_y
})

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

# Logout logic
observeEvent(input$logout, {
  session$reload()
  user_logged_in(FALSE)  # Reset login state
})