library(sqldf)
library(dplyr)
library(stringr)
source("scripts/get_data/get_acoustic_telem.R")
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")


db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

names_fix(names(tag_data23))
names(tag_data23)<-remove_special_chars(names(tag_data23))

tag_data23 <- tag_data23 |> 
  mutate(tagIDEvent = row_number()) |> 
  rename(date = DateUTC, time = TimeUTC, dateLocal = DateLocal, timeLocal = TimeLocal, receiver = Receiver, transmitter = Transmitter, 
         transmitterName = TransmitterName, transmitterSerial = TransmitterSerial, sensorValue = SensorValue,
         sensorUnit = SensorUnit, stationName = StationName, latitude = Latitude, longitude = Longitude, 
         transmitterType = TransmitterType, sensorPrecision = SensorPrecision)

tag_data23 <- tag_data23 |> 
  as.data.frame() |> 
  mutate( date = as.character(date), time = as.character(time), dateLocal = as.character(dateLocal))


sur_col_types <- get_col_types(tag_data23)

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


#subset(tag_data23, duplicated(time))


dbWriteTable(conn = con, "tagData", tag_data23, row.names = F, append = T)


dbDisconnect(con)
