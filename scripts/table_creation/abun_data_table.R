library(sqldf)
library(dplyr)
source("scripts/get_data/get_abun_data.R")
source("scripts/utils/col_types_f.R")

names(capture_raw)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

captureTable<-capture_raw
names(captureTable)<-remove_special_chars(names(captureTable))
names(captureTable)<-gsub("_","",names(captureTable)) 

captureTable<- captureTable |> 
  mutate(captureID = row_number(), date = as.character(date))

col_types<-get_col_types(captureTable)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("captureID", "date", "GPSPoint", "Pittag", "AcousticTag", "ScaleBookNo", "ScaleNos") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS abundanceCapture (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (captureID))")
dbExecute(con, sql)
dbWriteTable(conn = con, "abundanceCapture", captureTable, row.names = F, append = T)
dbListTables(con)

###################################################################################################

#rm(col_types, sur_col_types_sql, sql)

recapTable<-recapture_raw
recapTable
names(recapTable)<-gsub("_", "",names(recapTable))
names(recapTable)

recapTable<-recapTable |> 
  mutate(recapID = row_number(), date = as.character(date)) |> 
  rename(length = Lengthmm, weight = Weightg, startTime = starttime,
         endTime = endtime, pitTagNo = PITTagNo)

col_types<-get_col_types(recapTable)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("recapID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS recapture (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (recapID))")

dbExecute(con, sql)
dbWriteTable(conn = con, "recapture", recapTable, row.names = F, append = T)

query <- "SELECT * FROM recapture"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)
dbDisconnect(con)

