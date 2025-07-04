

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

# Handle downloading of tables as a zip file
output$download_creel_tables <- downloadHandler(
  filename = function() {
    paste0('Cultus_Lake_Creel_Survey_Joins.xlsx')
  },
  content = function(file) {
    openxlsx::write.xlsx(creel_joins(), file)
  }
)

# Inform what the action buttons actually do!
output$download_xlsx = downloadHandler(
  filename = function() {
    paste0('Cultus_Lake_',stringr::str_replace_all(input$table_name,' ','_'),'_Query.xlsx')
  },
  content = function(file) {
    openxlsx::write.xlsx(final_table(), file)
  }
)