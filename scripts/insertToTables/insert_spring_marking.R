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

spring_marking<-read_excel(path = file_list, sheet = 1, col_names = TRUE)

names(spring_marking)<-gsub(" ", "", names(spring_marking))
names(spring_marking)<-gsub("#", "No", names(spring_marking))
names(spring_marking)<-gsub("'", "", names(spring_marking))

spring_marking<- spring_marking |> 
  select(date, GPSstart, GPSfinish, SecondsElectricity, PITtag, AcousticTag, length, weight,
         sex, Maturity, Method, Recap, Mort, ScaleBookNo, ScaleNos, Comments, StartTime, EndTime,
         WaterTemp, Stomach, Euthanized, PassNo) |> 
  mutate(date = as.character(date)) |> 
  rename(PitTagNo = PITtag, secondsElectricity = SecondsElectricity, acousticTagNo = AcousticTag,
         maturity = Maturity, method = Method, recap = Recap, mort = Mort, scaleBookNo = ScaleBookNo,
         scaleNos = ScaleNos, comments = Comments, startTime = StartTime, endTime = EndTime)

query <- "SELECT MAX(springMarkID) FROM springMarking"
result <- dbGetQuery(con, query)

initCount<-result$`MAX(springMarkID)`
spring_marking <- spring_marking |> 
  mutate(springMarkID = row_number() + initCount)


col_types<-get_col_types(spring_marking)

#add the new columns
dbExecute(con, "ALTER TABLE springMarking ADD COLUMN GPSstart TEXT")
dbExecute(con, "ALTER TABLE springMarking ADD COLUMN GPSfinish TEXT")
dbExecute(con, "ALTER TABLE springMarking ADD COLUMN WaterTemp REAL")
dbExecute(con, "ALTER TABLE springMarking ADD COLUMN Stomach TEXT")
dbExecute(con, "ALTER TABLE springMarking ADD COLUMN Euthanized TEXT")
dbExecute(con, "ALTER TABLE springMarking ADD COLUMN PassNo REAL")

dbAppendTable(con, "springMarking", spring_marking)
