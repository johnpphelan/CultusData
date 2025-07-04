
#For the date filter
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

# #Age filter?
# output$ageFilter = renderUI({
#   
#   if (!'AgeClass' %in% names(queried_tbl())) return(NULL)
#   
#   if ('AgeClass' %in% names(queried_tbl())) {
#     shiny::selectInput(
#       inputId = 'age_class_filter',
#       label = 'Select Age Class:',
#       choices = sort(unique(queried_tbl()$AgeClass)), 
#       selected = NULL, 
#       multiple = TRUE 
#     )
#   }
# })

# all the other filters
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

output$queried_table = DT::renderDT({
  
  final_table()
})

## Is the column a numeric? If it is, use it, if it's not, then skip it
safe_numeric <- function(x) {
  
  if (inherits(x, "Date")) return(x)
  
  
  num_x <- suppressWarnings(as.numeric(as.character(x)))
  
  # If all non-NA values converted, and we have at least one non-NA result, accept it
  if (!all(is.na(num_x)) && sum(!is.na(num_x)) >= length(na.omit(x)) * 0.8) {
    return(num_x)
  } else {
    return(x)  # Keep as-is
  }
}


output$main_plot <- plotly::renderPlotly({
  df <- final_table()
  req(nrow(df) > 0, plot_inputs$x, plot_inputs$y)
  
  x_raw <- df[[plot_inputs$x]]
  y_raw <- df[[plot_inputs$y]]
  
  x <- safe_numeric(x_raw)
  y <- safe_numeric(y_raw)
  
  plot_df <- data.frame(x = x, y = y)
  
  p <- ggplot(plot_df, aes(x = x, y = y)) +
    {
      switch(input$plot_type,
             scatter = geom_point(),
             line = geom_line(),
             boxplot = geom_boxplot())
    } +
    labs(x = plot_inputs$x, y = plot_inputs$y) +
    theme_minimal()
  
  ggplotly(p)
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

output$creel_joined_tables <- DT::renderDT({
  
  creel_data_dynamic<-creel_joins()
  creel_data_dynamic <- creel_data_dynamic |> 
    select(where(~ !all(is.na(.))))
  # Render DataTable
  DT::datatable(creel_data_dynamic, options = list(scrollX = TRUE, scrollY = TRUE))
  
})

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

# Render logout button with custom style
output$logout_ui <- renderUI({
  actionButton(
    inputId = "logout",
    label = "Logout",
    style = "background-color: #CC5500; color: white; border-color: #6A0000;",
    class = "btn"
  )
})
