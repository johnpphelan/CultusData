library(sqldf)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
source("scripts/utils/col_types_f.R")
db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)


tagsSold<-read.csv("data/Acoustic_tags/TransmitterSalesValues.csv")

tagsSold = tagsSold |>
  mutate(across(everything(), ~as.character(.)))

col_types<-get_col_types(tagsSold)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("date, transmitter") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS tagEvent (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (date, transmitter))")
dbExecute(con, sql)
dbWriteTable(conn = con, "dateTagSold", tagsSold, row.names = F, append = T)
dbListTables(con)

dbDisconnect(con)
