library(DBI)
library(RSQLite)
library(tidyverse)

# Define database path
db_filepath <- "scripts/shiny/www/CultusData.sqlite"

# Function to extract column names and write CSV
extract_and_write_template <- function(con, table_name, output_dir = "output/templates/") {
  query <- paste0("PRAGMA table_info(", table_name, ");")
  result <- dbSendQuery(con, query)
  column_info <- fetch(result, n = -1)
  dbClearResult(result)
  
  # Reshape and blank out first row
  column_info <- column_info |> 
    pivot_wider(names_from = name, values_from = type) |> 
    slice(1) |> 
    mutate(across(everything(), ~NA))
  
  # Select only the column name placeholders (skip first 4 cols)
  column_names <- column_info[, 5:ncol(column_info)]
  
  # Write to CSV
  write.csv(column_names, file = file.path(output_dir, paste0(table_name, ".csv")), row.names = FALSE)
}

# Tables you want to export templates for
table_names <- c(
  "SweltzerCreek", "abundanceCapture", "anglingCapBasok", "anglingDerby", 
  "nestRaw", "recapture", "scale500", "scaleRawTable", 
  "scaleColNameKey", "springMarking", "tagData", 
  "highRewardTags", "highRewardTagsClaimed"
)

# Connect to database
con <- dbConnect(RSQLite::SQLite(), db_filepath)

# Loop through and generate templates
walk(table_names, ~extract_and_write_template(con, .x))

# Disconnect
dbDisconnect(con)