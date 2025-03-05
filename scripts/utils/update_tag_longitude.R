library(tidyverse)
library(sqldf)
library(readxl)
library(sf)

db_filepath = "scripts/shiny/www/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath)

query <- "SELECT * FROM tagData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
tags<-fetch(result, -1)
dbClearResult(result)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

receiver_locations<-read_excel(paste0(lan_folder,"2023 projects/acoustic telemetry/2023 receiver locations and deployment information.xlsx"))


receiver_locations <- receiver_locations |> 
  mutate(across(everything(), as.character))


tags_high <-tags |> 
  mutate(receiver_name = str_extract(receiver, "(?<=-).*")) |> 
  filter(longitude < -123) |> 
  mutate(across(everything(), as.character))

tags_ok <-tags |> 
  mutate(receiver_name = str_extract(receiver, "(?<=-).*")) |> 
  filter(longitude > -123)


tags_fixed<- tags_high |> 
  left_join(receiver_locations, by = c("receiver_name" = "receiver label number")) |> # Join with location data
  mutate(latitude = coalesce(latitude.y, latitude.x),  # Replace latitude if match found
         longitude = coalesce(longitude.y, longitude.x)) |>  # Replace longitude if match found
  select(-latitude.x, -longitude.x, -latitude.y, -longitude.y,
         -`receiver name`, -`deployment date`, -`location description`, -`depth of float at deployment (cm)`) # Remove redundant columns



tags_fixed <- tags_fixed |> 
  mutate(
    # Extract degrees, minutes, and direction for longitude
    lon_deg = as.numeric(str_extract(longitude, "^\\d+")),   
    lon_min = as.numeric(str_extract(longitude, "\\d+\\.\\d+")), 
    lon_dir = str_extract(longitude, "[EW]"),      
    
    # Convert longitude to decimal degrees
    longitude_dd = lon_deg + (lon_min / 60), 
    longitude_dd = ifelse(lon_dir == "W", -longitude_dd, longitude_dd), 
    
    # Extract degrees, minutes, and direction for latitude
    lat_deg = as.numeric(str_extract(latitude, "^\\d+")),   
    lat_min = as.numeric(str_extract(latitude, "\\d+\\.\\d+")), 
    lat_dir = str_extract(latitude, "[NS]"),      
    
    # Convert latitude to decimal degrees
    latitude_dd = lat_deg + (lat_min / 60), 
    latitude_dd = ifelse(lat_dir == "S", -latitude_dd, latitude_dd)
  ) |> 
  select(-lon_deg, -lon_min, -lon_dir, -lat_deg, -lat_min, -lat_dir, - longitude, - latitude) |>  # Remove intermediate columns
  rename(longitude = longitude_dd, latitude = latitude_dd) |> 
  mutate(across(everything(), as.character))

tags_ok <- tags_ok |> 
  mutate(across(everything(), as.character))


tags_combined<-rbind(tags_ok, tags_fixed)

tags_combined <- tags_combined |> 
  mutate(longitude = as.numeric(longitude)) |> 
  mutate(longitude = if_else(longitude > 0, -longitude, longitude)) |> 
  mutate(longitude = as.character(longitude))


delete_tags<- "DROP TABLE IF EXISTS tagData"
dbExecute(con, delete_tags)


sur_col_types <- get_col_types(tags_combined)

sur_col_types$sqlite_type[sur_col_types$col_name == "time"] <- "TEXT"
sur_col_types$sqlite_type[sur_col_types$col_name == "date"] <- "TEXT"
sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("tagIDEvent")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS tagData (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (tagIDEvent))")

dbExecute(con, sql)
DBI::dbListTables(con)

dbWriteTable(conn = con, "tagData", tags_combined, row.names = F, append = T)


dbDisconnect(con)



