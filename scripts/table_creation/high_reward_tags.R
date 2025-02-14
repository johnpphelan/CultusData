library(sqldf)
library(dplyr)
library(lubridate)
library(stringi)
library(readxl)
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2024 projects/"

db_filepath = "output/CultusData.sqlite"
con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

f_name<-paste0(lan_folder,"High-Reward Tags/High_Reward_Tag_Dataset_2024.xlsx")
high_tags<-read_excel(path = f_name, col_names = TRUE, skip = 2)

high_tags <- high_tags |> 
  rename(date = Date, tagColor = `Tag Colour`, tagCode = `Tag Code`, fl_mm = `FL(mm)`,
         recap = `Recap (Y/N)`, notes = Notes) |> 
  mutate(date = str_trim(date)) |> 
  mutate(date = mdy(date), date = as.character(date)) |> 
  fill(notes) |> 
  mutate(tagID = row_number())

sur_col_types <- get_col_types(high_tags)

sur_col_types

sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("date", "tagID")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))


sql = paste0("CREATE TABLE IF NOT EXISTS highRewardTags (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (\ntagID\n))")


dbExecute(con, sql)
dbWriteTable(conn = con, "highRewardTags", high_tags, row.names = F, append = T)
query <- "SELECT * FROM highRewardTags"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

DBI::dbListTables(con)
