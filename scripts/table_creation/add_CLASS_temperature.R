library(tidyverse)
library(DBI)
library(RSQLite)
library(readxl)
library(tidyr)


source("scripts/utils/col_types_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

class_temps<-read_xlsx(paste0(lan_folder, "2024 Projects/Temperature/CLASSTempSummary2024.xlsx"))


str(class_temps)
class_temps<- class_temps |> 
  rename(date = Date, site = `Site Name`, time = Time, coordinates = Coordinates,
         airTemp = `Air Temp(C)`, depthAtLocation = `Depth at Location`, sampleDepth = `Sample Depth(m)`, 
         temperature = `Temperature (C)`) |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         time = as.POSIXct(time, format = "%H:%M:%S")) |> 
  separate(coordinates, into = c("lat", "lon"), sep = "W", remove = FALSE) |> 
  mutate(lat = gsub("N", "", lat, ignore.case = TRUE)) |> 
  mutate(lon = gsub("W", "", lon, ignore.case = TRUE)) |>
  mutate(across(everything(), as.character))

col_types<-get_col_types(class_temps)

 class_temps<- class_temps |> 
  mutate(tempID = row_number())

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("tempID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS classTemps (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (tempID))")
dbExecute(con, sql)
dbWriteTable(conn = con, "classTemps", class_temps, row.names = F, append = T)
dbListTables(con)

dbDisconnect(con)
