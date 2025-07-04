library(sqldf)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(readxl)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)


lan_folder = "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"
lan_file<- "2025 projects/Fishing Derby/2025-Fishing-Derby-Data_updated.xlsx"

derby<-read_excel(lan_folder |>  paste0(lan_file))

smallmouth<-derby[,1:7]
names(smallmouth)<-smallmouth[1,]
smallmouth<-smallmouth[-1,]

query <- "SELECT * FROM anglingDerby"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)

smallmouth$date<-"2025-06-15"
smallmouth <- smallmouth |> 
  dplyr::rename(length = `Length (mm)`, weight = `Weight (g)`, pitTagNo = `PIT Tag ID`, 
                acousticTagNo = `Acoustic Tag ID`, otolithNo = `Otolith/Scale ID`, sex = Sex, floyTag = `Floy Tag`)

max_id<-df |> 
  dplyr::pull(anglingID) |> 
  max(na.rm = TRUE)

smallmouth <- smallmouth |>
  dplyr::mutate(anglingID = row_number()+max_id) |> 
  dplyr::mutate(across(everything(), as.character))

dbExecute(con, "ALTER TABLE anglingDerby ADD COLUMN otolithNo TEXT")

dbExecute(con, "ALTER TABLE anglingDerby ADD COLUMN sex TEXT")

dbExecute(con, "ALTER TABLE anglingDerby ADD COLUMN floyTag TEXT")

dbWriteTable(con, "anglingDerby", smallmouth, append = TRUE, row.names = FALSE)


#########################################################################################

pumpkinseed<- derby[,13:14]



names(pumpkinseed)<-pumpkinseed[1,]
pumpkinseed<-pumpkinseed[-1,]

pumpkinseed$date = "2025-06-15"

pumpkinseed <- pumpkinseed |> 
  dplyr::rename(length = `Length (mm)`, weight = `Weight (g)`)

pumpkinseed <- pumpkinseed |>
  dplyr::mutate(anglingID = row_number()) |> 
  dplyr::mutate(across(everything(), as.character))

source("scripts/utils/col_types_f.R")

sur_col_types <- get_col_types(pumpkinseed)

sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("anglingID")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS anglingDerbyPumpkinseed (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (anglingID))")

dbExecute(con, sql)
DBI::dbListTables(con)
dbWriteTable(conn = con, "anglingDerbyPumpkinseed", pumpkinseed, row.names = F, append = T)

query <- "SELECT * FROM anglingDerbyPumpkinseed"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)




dbDisconnect(con)


