library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
source("scripts/utils/fix_col_names_f.R")

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


file_list<-list.files(path =paste0(lan_folder,"2024 projects/Electroshocking/"),
                      pattern = "*.xlsx", full.names = T)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

abundanceData<-read_excel(path = file_list, sheet = 1, col_names = TRUE)

names(abundanceData)<-gsub(" ", "", names(abundanceData))
names(abundanceData)<-gsub("#", "No", names(abundanceData))
names(abundanceData)<-gsub("'", "", names(abundanceData))

abundancedata<- abundanceData |> 
  select(date, GPSstart, GPSfinish, SecondsElectricity, PITtag, AcousticTag, length, weight,
         sex, Maturity, Method, Recap, Mort, ScaleBookNo, ScaleNos, Comments, StartTime, EndTime,
         WaterTemp, Stomach, Euthanized, PassNo) |> 
  mutate(date = as.character(date)) |> 
  rename( passNo = PassNo, startTime = StartTime, endTime = EndTime, secondsElectricity = SecondsElectricity,
          pitTagNo = PITtag, acousticTagNo = AcousticTag, euthanized = Euthanized, waterTemp = WaterTemp,
         maturity = Maturity, method = Method, recap = Recap, mort = Mort, scacleBookNo = ScaleBookNo,
         scaleNo = ScaleNos, comments = Comments, stomach = Stomach)

query <- "SELECT MAX(captureID) FROM abundanceCapture"
result <- dbGetQuery(con, query)

initCount<-result$`MAX(captureID)`
abundancedata <- abundancedata |> 
  mutate(captureID = row_number() + initCount)


col_types<-get_col_types(abundancedata)

#add the new columns
dbExecute(con, "ALTER TABLE abundanceCapture ADD COLUMN GPSstart TEXT")
dbExecute(con, "ALTER TABLE abundanceCapture ADD COLUMN GPSfinish TEXT")
dbExecute(con, "ALTER TABLE abundanceCapture ADD COLUMN waterTemp REAL")
dbExecute(con, "ALTER TABLE abundanceCapture ADD COLUMN stomach TEXT")
dbExecute(con, "ALTER TABLE abundanceCapture ADD COLUMN euthanized TEXT")
dbExecute(con, "ALTER TABLE abundanceCapture ADD COLUMN passNo REAL")

dbAppendTable(con, "abundanceCapture", abundancedata)
dbDisconnect(con)
