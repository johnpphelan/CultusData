library(data.table)
library(dplyr)
library(stringr)
library(reshape2)
source("scripts/utils/fix_col_names_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)


tableNames<-c("surveyData", "anglerInfo", "fishCatch", "fishingDetails", "weatherDetails", "surveyAnswers", "surveyQuestions", "iceData")

creelTable<-tableNames |> 
              data.frame() |> 
              mutate(creelTableID = row_number(),
                     col_names = NA)
            
for (i in 1:nrow(creelTable)){
  Name<-creelTable[i,]$tableNames
  query <- paste0("PRAGMA table_info(",Name,");")
  result <- dbSendQuery(con, query)
  column_names <- fetch(result, n = -1)
  nm<-column_names$name
  col_nm<-paste(nm,collapse = " ")
  creelTable$col_names[i]<-col_nm
  dbClearResult(result)
}

col_types<-get_col_types(creelTable)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("col_name", "creelTableID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS creelTables (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (creelTableID))")

dbExecute(con, sql)
dbWriteTable(conn = con, "creelTables", creelTable, row.names = F, append = T)
dbDisconnect(con)
