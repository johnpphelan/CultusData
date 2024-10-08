library(sqldf)
library(dplyr)
library(tidyr)
library(openxlsx)
source("scripts/get_data/get_angling_recapture.R")
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

## time field is a mess - we should look at changing this

angling_data <- angling_data |> 
  mutate(anglingCapID = row_number(),
         Survey_Date = as.character(Survey_Date),
         Time = format(convertToDateTime(Time), format = "%H:%m"))
names(angling_data) <- gsub("_", "", names(angling_data))

angling_data <- angling_data |> 
  rename(date = SurveyDate, surveyor = SurveyorName, time = Time, weather = Weather,
         location = Location, fishNo = FishNo, length = Lengthmm, weight = Weightg,
         pitTagNo = PITtagNo, acousticTagNo = AcoustictagNo)

col_types<-get_col_types(angling_data)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("anglingCapID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS anglingCapBasok (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (anglingCapID))")
dbExecute(con, sql)
dbWriteTable(conn = con, "anglingCapBasok", angling_data, row.names = F, append = T)
dbListTables(con)
dbDisconnect(con)
