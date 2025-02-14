library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
source("scripts/utils/fix_col_names_f.R")

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


file_list<-list.files(path =paste0(lan_folder,"2024 projects/Electroshocking/"),
                      pattern = "*.xlsx", full.names = T)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)
query <- "SELECT * FROM recapture"
result <- dbGetQuery(con, query)


recapture24<-read_excel(path = file_list, sheet = 1, col_names = TRUE)

recapture24<- recapture24 |> filter(!is.na(PITtag))

recap<-recapture24 |> mutate(date = as.character(date)) |> 
  rename(AcousticTagNo = AcousticTag, pitTagNo = PITtag, comments = Comments, startTime = `Start Time`, endTime = `End Time`) 

recap<-recap |> 
  select(AcousticTagNo, pitTagNo, date, startTime, endTime, comments)

query <- "SELECT MAX(recapID) FROM recapture"
result <- dbGetQuery(con, query)

initCount<-result$`MAX(recapID)`
recap <- recap |> 
  mutate(recapID = row_number() + initCount)

col_types<-get_col_types(recap)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("recapID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

recap<-recap |> mutate(date = as.character(date), startTime = as.character(startTime), endTime = as.character(endTime),
                comments = as.character(comments), AcousticTagNo = as.numeric(AcousticTagNo), pitTagNo = as.numeric(pitTagNo))

dbAppendTable(con, "recapture", recap)

dbDisconnect(con)
