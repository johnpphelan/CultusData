library(sqldf)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
source("scripts/utils/col_types_f.R")
db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

tagEvent<-read_xlsx(paste0(lan_folder, "2023 Projects/acoustic telemetry/20231101-SMB acoustic tag removal log.xlsx"))

names(tagEvent)

tagEvent = tagEvent |> 
  rename(dateTagged = `date tagged`, acousticTagNo = `Acoustic Tag`, tagModel = `Tag model`, dateRemoved = `date removed`,
         pitTagNo = `Pit tag`, maturity = Maturity, removalMethod = `removal method`)

tagEvent = tagEvent |>
  mutate(across(everything(), ~as.character(.)))

col_types<-get_col_types(tagEvent)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("dateTagged, acousticTagNo") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS tagEvent (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (dateTagged, acousticTagNo))")
dbExecute(con, sql)
dbWriteTable(conn = con, "tagEvent", tagEvent, row.names = F, append = T)
dbListTables(con)

dbDisconnect(con)

         