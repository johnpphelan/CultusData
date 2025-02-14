library(sqldf)
library(dplyr)
library(lubridate)
library(stringi)
library(readxl)
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2024 projects/High-Reward tags/"

db_filepath = "output/CultusData.sqlite"
con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)


high_tags_claimed<-read_excel(path = "./data/High reward tags.xlsx", col_names = TRUE)

high_tags_claimed_reduced<-high_tags_claimed |> 
  mutate(rewardID = row_number()) |> 
  rename(year = Year, date = `Date reported`, name = Name, tagID = `Tag #`,
         harvested = `Harvested?`, sentMoney = `sent for $$`, address = Address, 
         phoneNumber = `Phone number`, email = `Email address`, length = `Length mm`, 
         notes = Notes) |> 
  mutate(date = as.character(date)) |> 
  select(date, tagID, harvested, length, notes, rewardID)

write.csv(high_tags_claimed_reduced, "./output/high_tags_claimed_reduced.csv", row.names = F)

sur_col_types <- get_col_types(high_tags_claimed_reduced)

sur_col_types

sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("date", "tagID", "rewardID")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))


sql = paste0("CREATE TABLE IF NOT EXISTS highRewardTagsClaimed (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (\nrewardID\n))")

dbExecute(con, sql)
dbWriteTable(conn = con, "highRewardTagsClaimed", high_tags_claimed_reduced, row.names = F, append = T)
query <- "SELECT * FROM highRewardTagsClaimed"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

DBI::dbListTables(con)

