library(data.table)
library(sqldf)
library(dplyr)
library(stringr)
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

parse_utc <- function(x) {
  parse_date_time(
    str_trim(na_if(na_if(as.character(x), ""), "NA")),
    orders = c(
      "Ymd HMS", "Ymd HM", "Ymd",
      "mdY HMS", "mdY HM", "mdY"
    ),
    tz = "UTC"
  )
}

lan_folder = "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/DATA FILES Cultus SMB/Acoustic telemetry/"

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

receiver_data = read.csv(paste0(lan_folder,"all_receiver_data.csv"))

## only add the last 2 years
receiver_data <- receiver_data |> 
  mutate(time = parse_utc(time)) |> 
  filter(time > "2025-01-01")

receiver_data <- receiver_data |> 
  mutate(tagIDEvent = row_number()) 


sur_col_types <- get_col_types(receiver_data)




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


dbWriteTable(conn = con, "tagData", receiver_data, row.names = F, append = T)


dbDisconnect(con)



