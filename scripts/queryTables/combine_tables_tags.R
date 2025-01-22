library(sqldf)
library(dplyr)
#source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath)

print(dbListObjects(con))

query <- "SELECT * FROM fishCaught"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

tables <- dbListTables(con)
tables_with_column <- c()
table_list<-list()
i = 1
for (table in tables) {
  column_info <- dbGetQuery(con, paste0("PRAGMA table_info(", table, ")"))
  
  if ("pitTagNo" %in% column_info$name) {
    tables_with_column <- c(tables_with_column, table)
    dbExecute(con = con, paste0("SELECT * FROM ",table))
    result <- dbSendQuery(conn = con, paste0("SELECT * FROM ",table))
    df<-fetch(result, -1)
    table_list[[i]]<-df
    dbClearResult(result)
    i=i+1
  }
}

names(table_list)<-tables_with_column

if (length(tables_with_column) > 0) {
  cat("Tables containing the column 'PitTagNo':\n")
  print(tables_with_column)
} else {
  cat("No tables found with the column 'PitTagNo'.\n")
}


