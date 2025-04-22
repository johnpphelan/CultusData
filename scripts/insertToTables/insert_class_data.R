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

class_temps<-read_xlsx(paste0(lan_folder, "2025 Projects/Temperature/CLASS Field Sheet 2025.xlsx"))

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

class_temps<- class_temps |> 
  mutate(tempID = row_number())



# writing a temporary table to hold the data before adding to the main table
dbWriteTable(con, "temp_classTemps", class_temps, temporary = TRUE, overwrite = TRUE)

#get the names of the columns
colnames_db <- dbListFields(con, "classTemps")
#collapsing the column names with commas, for use later
columns_str <- paste(colnames_db, collapse = ", ")
# this create the statment to join the temp table and the main table together,
# provided there is no conflict. If the same date/time combination already exists,
# then the data will not be uploaded.
upsert_query <- glue::glue("
  INSERT OR IGNORE INTO classTemps ({columns_str})
  SELECT {columns_str}
  FROM temp_classTemps
")
#do it
dbExecute(con, upsert_query)
#drop the temp table
dbExecute(con, "DROP TABLE IF EXISTS temp_classTemps")
# close connection
dbDisconnect(con)



