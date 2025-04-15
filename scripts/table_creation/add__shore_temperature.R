library(tidyverse)
library(DBI)
library(RSQLite)
library(readxl)

source("scripts/utils/col_types_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

shore_temps<-read_xlsx(paste0(lan_folder, "2024 Projects/Temperature/CultusShoreTemps(SMB).xlsx"))

location<-as.character(colnames(shore_temps[1]))

colnames(shore_temps)<-shore_temps[1,]
shore_temps<-shore_temps[-1,]

shore_temps = shore_temps |> 
  mutate(Date = as.Date(as.numeric(Date), origin="1899-12-30")) |> 
  rename(date = Date, temp = `Temp (oC)`, timeTaken = `Time Taken (2400hrs)`, depth_m = `Depth Taken (m)`, 
         adultBassObs = `Adult Bass Observed`, nestsObserved = `Nests Observed`, visibility = Visibility, 
         comments = Comments) |> 
  mutate(across(everything(), as.character))
  
shore_temps["location"] <- location

col_types<-get_col_types(shore_temps)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("date, timeTaken") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS shoreTemps (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (date, timeTaken))")
dbExecute(con, sql)
dbWriteTable(conn = con, "shoreTemps", shore_temps, row.names = F, append = T)
dbListTables(con)

dbDisconnect(con)
