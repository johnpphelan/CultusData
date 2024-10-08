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


file_list<-list.files(path =paste0(lan_folder,"2024 projects/Creel Surveys/Creel Survey forms 2024 (backup)/"),
                      pattern = "*.xlsx", full.names = T)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

f_name<-file_list[1]
f_name

creelData<-read_excel(path = f_name, sheet = 1, col_names = TRUE)
names(creelData)[names(creelData) == "# SMB c...15"]<- "# SMB c"
names(creelData)[names(creelData) == "# SMB c...23"]<- "# SMB r"

namesFix<-names(creelData)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(creelData)<-namesFix


sheets<-excel_sheets(f_name)
sheetOI<-grep("Demographic", sheets, value = TRUE)
demograhpy<-lapply(sheetOI, read_excel, path = f_name)

sheetOI<-grep("Fish", sheets, value = TRUE)
fishdata<-lapply(sheetOI, read_excel, path = f_name)

sheetOI<-grep("ICE", sheets, value = TRUE)
ICE<-data.frame(lapply(sheetOI, read_excel, path = f_name))




survDT<- creelData |> 
  select(Survey_No, Date, Time, Surveyor, Shift) |> 
  mutate(Date = as.Date(Date, origin = "1899-12-30")) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, surveyor = Surveyor, shift = Shift) |> 
  mutate(date = as.character(date), time = as.character(time), shift = as.character(shift)) 

survDT  |> 
  mutate(surveyNumber = NA)



recentID<-dbGetQuery(conn = con, "SELECT surveyNumber FROM surveyData")
dbAppendTable(con, "surveyData", survDT) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

#############################################################################################################


catchDF<- creelData |> 
  select(Survey_No, Date, Time, Total_Fish_Caught, Total_Retained, ends_with("_c"), ends_with("_r"),
         Release_Reason) |>
  mutate(Date = as.Date(Date, origin = "1899-12-30")) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, totFishCaught = Total_Fish_Caught,
         totRetained = Total_Retained, releaseReason = Release_Reason) |>
  rename_with(~ str_replace_all(.x, "_", ""), everything()) |> 
  mutate(date = as.character(date), time = as.character(time)) 

dbAppendTable(con, "fishCatch", catchDF) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

##########################################################################

fishingdetails<- creelData |> 
  select(Survey_No, Date, Time, No_Anglers, No_Rods, Total_Person_Hr_Fished, Vessel,
         Preferred_Catch_Spp, Site) |> 
  mutate(Date = as.Date(Date, origin = "1899-12-30")) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, numberAnglers = No_Anglers, numberRods = No_Rods, 
         personHoursFished = Total_Person_Hr_Fished, vessel = Vessel, prefferedSpp = Preferred_Catch_Spp,
         site = Site) |>   
  mutate(date = as.character(date), time = as.character(time))

dbAppendTable(con, "fishingDetails", fishingdetails) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

#####################################################################

weatherTable<-creelData |> 
  select(Survey_No, Time, Site, Air__temperature, Cloud_Cover, Wind, Precip) |> 
  rename(surveyNumber = Survey_No, time = Time, site = Site, meanAirTemp= Air__temperature,
         cloudCover = Cloud_Cover, wind = Wind, precip = Precip) |> 
  mutate(time = as.character(time), wind = paste0(day(wind),"-",month(wind)))

weatherTable

dbAppendTable(con, "weatherDetails", weatherTable) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work


############################################################################

questionTable<- creelData |> 
  colnames()

questionTable <- questionTable |> 
  as.data.frame() |> 
  slice(32:43) |> 
  rename(question = questionTable)

query <- "SELECT * FROM surveyQuestions"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

miss<- questionTable |> 
  filter(!question %in% df$question)

adding<- miss |> 
  mutate(questionID = max(df$questionID) + row_number())

dbAppendTable(con, "surveyQuestions", adding) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

#####################################################################################

query <- "SELECT * FROM surveyQuestions"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

answersTable<- creelData |> 
  select(c(Survey_No, Time, contains(df$question)))


answersLong <- answersTable |>
  pivot_longer(cols = -c(Survey_No, Time), names_to = "Question", values_to = "Answer") |> 
  left_join(df, by = c("Question" = "question")) |> 
  mutate(Question = questionID) |> 
  select(-questionID) |> 
  rename( surveyNumber = Survey_No, time = Time, question = Question, answer = Answer)  |> 
  mutate(time = as.character(time))



dbAppendTable(con, "surveyAnswers", answersLong) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work


#####################################################################################################





query <- "SELECT * FROM surveyAnswers"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)




#################################################################



ICE2<- ICE |> 
  rename(date = Date, time = Time, noAnglingBoats = X..angling.boats, noBoatAnglers = X..boat.anglers, 
         noShoreAnglers = X..shore.anglers, noDockAnglers=X..dock.anglers, airTemp = Air.temperature,
         cloudCover = Cloud.cover, wind = Wind, precip = Precipitation) |> 
  mutate(weather = paste0("Air temperature: ", airTemp,", cloud cover: ", cloudCover, ", wind: ", wind,", precipiation: ", precip)) |> 
  select(date, time, noAnglingBoats, noBoatAnglers, noShoreAnglers, noDockAnglers, weather)


dbAppendTable(con, "iceData", ICE2) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work





