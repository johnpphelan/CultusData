library(tidyverse)
library(DBI)
library(RSQLite)
library(readxl)

source("scripts/utils/col_types_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

# lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"
# 
# shore_temps<-read_xlsx(paste0(lan_folder, "2025 Projects/Temperature/CultusShoreTemps(SMB).xlsx"))

shore_temps<-read_xlsx(paste0("data/CultusShoreTemps(SMB).xlsx"))

location<-as.character(colnames(shore_temps[1]))

colnames(shore_temps)<-shore_temps[1,]
shore_temps<-shore_temps[-1,]

shore_temps = shore_temps |> 
  mutate(Date = as.Date(as.numeric(Date), origin="1899-12-30")) |> 
  rename(date = Date, temp = `Temp (oC)`, timeTaken = `Time Taken (2400hrs)`, depth_m = `Depth Taken (m)`, 
         adultBassObs = `Adult Bass Observed`, nestsObserved = `Nests Observed`, visibility = Visibility, 
         comments = Comments) |> 
  mutate(across(everything(), as.character))
  

## if the time taken is missing, we have to fill it in as 12:01, so that it can be easily identified later
shore_temps<-shore_temps |> 
  mutate(timeTaken = ifelse(is.na(timeTaken), "12:01", timeTaken))

shore_temps["location"] <- location

# writing a temporary table to hold the data before adding to the main table
dbWriteTable(con, "temp_shoreTemps", shore_temps, temporary = TRUE, overwrite = TRUE)

#get the names of the columns
colnames_db <- dbListFields(con, "shoreTemps")
#collapsing the column names with commas, for use later
columns_str <- paste(colnames_db, collapse = ", ")
# this create the statment to join the temp table and the main table together,
# provided there is no conflict. If the same date/time combination already exists,
# then the data will not be uploaded.
upsert_query <- glue::glue("
  INSERT OR IGNORE INTO shoreTemps ({columns_str})
  SELECT {columns_str}
  FROM temp_shoreTemps
")
#do it
dbExecute(con, upsert_query)
#drop the temp table
dbExecute(con, "DROP TABLE IF EXISTS temp_shoreTemps")
# close connection
dbDisconnect(con)

