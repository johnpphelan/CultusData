library(sqldf)
library(dplyr)
library(tidyr)
source("scripts/get_data/get_spring_marking.R")
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

names(spring_marking)

#### check the contents of spring marking - does this table need foreign keys?

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)



spring_marking <- spring_marking |> 
  mutate(springMarkID = row_number()) |> 
  rename(GPSPoint = GPS_Point, startTime = Start_Time, endTime = End_Time, secondsElectricity = Seconds_Electricity,
         pitTagNo = Pit_tag, acousticTagNo = Acoustic_Tag, maturity = Maturity, method = Method, recap = Recap, 
         mort = Mort, scaleBookNo = Scale_Book_No, scaleNos = Scale_Nos, comments = Comments)



col_types<-get_col_types(spring_marking)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("springMarkID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS springMarking (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (springMarkID))")
dbExecute(con, sql)
dbWriteTable(conn = con, "springMarking", spring_marking, row.names = F, append = T)
dbListTables(con)
dbDisconnect(con)
