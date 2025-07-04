

# get the first table - use the look up excel to find the correct table name in the database and 
#return the table
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