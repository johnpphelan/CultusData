library(sqldf)
library(dplyr)
source("scripts/get_data/get_angling_derby.R")
source("scripts/utils/col_types_f.R")

angling_derby_data

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)


angling_derby_data<- angling_derby_data  |> 
  mutate(date = as.character(date), start_time = as.character(start_time),
         end_time = as.character(end_time)) |> 
  mutate(anglingID = row_number())

names(angling_derby_data) <- gsub("_", "", names(angling_derby_data))

sur_col_types <- get_col_types(angling_derby_data)


sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("anglingID")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS anglingDerby (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (anglingID))")

dbExecute(con, sql)
DBI::dbListTables(con)
dbWriteTable(conn = con, "anglingDerby", angling_derby_data, row.names = F, append = T)

query <- "SELECT * FROM anglingDerby"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)
