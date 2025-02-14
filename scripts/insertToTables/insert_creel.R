library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
source("scripts/utils/fix_col_names_f.R")



lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2024 projects/Creel Surveys/"


# file_list<-list.files(path =paste0(lan_folder,"2024 projects/Creel Surveys/Creel Survey forms 2024 (backup)/"),
#                       pattern = "*.xlsx", full.names = T)
# 
# db_filepath = "output/CultusData.sqlite"
# 
# con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)
# 
# DBI::dbListTables(con)
# 
# f_name<-file_list[1]
# f_name
db_filepath = "output/CultusData.sqlite"
con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

f_name<-paste0(lan_folder,"Cultus Lake Creel Form 2024_Data_Working2.xlsx")
creelData<-read_excel(path = paste0(lan_folder,"Cultus Lake Creel Form 2024_Data_Working2.xlsx"), sheet = 2, col_names = TRUE)
names(creelData)[names(creelData) == "# SMB c...15"]<- "# SMB c"
names(creelData)[names(creelData) == "# SMB c...23"]<- "# SMB r"

namesFix<-names(creelData)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(creelData)<-namesFix


sheets<-excel_sheets(paste0(lan_folder,"Cultus Lake Creel Form 2024_Data_Working2.xlsx"))
sheetOI<-grep("Demographic", sheets, value = TRUE)
demography<-lapply(sheetOI, read_excel, path = f_name)

sheetOI<-grep("Fish", sheets, value = TRUE)
fishdata<-lapply(sheetOI, read_excel, path = f_name)

sheetOI<-grep("ICE", sheets, value = TRUE)
ICE<-data.frame(lapply(sheetOI, read_excel, path = f_name))



#################################################################

#########


creelData<-creelData |>
  mutate(Time = as_datetime(as.numeric(Time) * 86400, origin = "1899-12-30")) |>  # Convert Excel time to datetime
  group_by(Time) |>
  mutate(
    Time = if_else(row_number() > 1, Time + minutes(5) * (row_number() - 1), Time) # Add 5 minutes to duplicates
  ) |>
  ungroup()
  


survDT<- creelData |> 
  select(Survey_No, Date, Time, Surveyor, Shift) |> 
  mutate(Date = as.Date(Date, origin = "1899-12-30")) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, surveyor = Surveyor, shift = Shift) |> 
  mutate(date = as.character(date), time = as.character(time), shift = as.character(shift)) 

survDT<- survDT |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))

survDT<- survDT |>
  mutate(
    shift = str_remove(shift, "^\\d+: "),
    shift_end = sub(".*-", "", shift),
    shift_start = sub("-.*", "", shift),
    shift_start = if_else(str_detect(shift_start, "^\\d{1,2}:\\d{2}$"), paste0(shift_start, ":00"), shift_start),
    shift_end = if_else(str_detect(shift_end, "^\\d{1,2}:\\d{2}$"), paste0(shift_end, ":00"), shift_end),
    shift_start_time = hms::parse_hms(shift_start),
    shift_end_time = hms::parse_hms(shift_end),
    hours_worked = as.numeric(difftime(shift_end_time, shift_start_time, units = "hours"))
  ) |> 
  mutate(shift_start_time = as.character(paste(date, shift_start_time)),
         shift_end_time = as.character(paste(date, shift_end_time))) |> 
  select(surveyNumber, date, time, surveyor, shift_start_time, shift_end_time, hours_worked)

survDT <- survDT |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))



#recentID<-dbGetQuery(conn = con, "SELECT surveyNumber FROM surveyData")
dbAppendTable(con, "creelShifts", survDT) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

#############################################################################################################


catchDF<- creelData |> 
  select(Survey_No, Date, Time, Total_Fish_Caught, Total_Retained, ends_with("_c"), ends_with("_r"),
         Release_Reason) |>
  mutate(Date = as.Date(Date, origin = "1899-12-30")) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, totFishCaught = Total_Fish_Caught,
         totRetained = Total_Retained, releaseReason = Release_Reason) |>
  rename_with(~ str_replace_all(.x, "_", ""), everything()) |> 
  mutate(date = as.character(date), time = as.character(time)) 

catchDF<- catchDF |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))


dbAppendTable(con, "creelFishResults", catchDF) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

##########################################################################

# fishingdetails<- creelData |> 
#   select(Survey_No, Date, Time, No_Anglers, No_Rods, Total_Person_Hr_Fished, Vessel,
#          Preferred_Catch_Spp, Site) |> 
#   mutate(Date = as.Date(Date, origin = "1899-12-30")) |> 
#   rename(surveyNumber = Survey_No, date = Date, time = Time, numberAnglers = No_Anglers, numberRods = No_Rods, 
#          personHoursFished = Total_Person_Hr_Fished, vessel = Vessel, prefferedSpp = Preferred_Catch_Spp,
#          site = Site) |>   
#   mutate(date = as.character(date), time = as.character(time))
# 
# fishingdetails<- fishingdetails |> 
#   mutate(date = as.Date(date)) |> 
#   arrange(date) |> 
#   mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
#   group_by(date_group) |> 
#   mutate(surveyNumber = row_number()) |>  
#   ungroup() |> 
#   select(-date_group) |> 
#   mutate(date = as.character(date))
# 
# dbAppendTable(con, "fishingDetails", fishingdetails) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

#####################################################################

weatherTable<-creelData |> 
  select(Survey_No, Date, Time, Site, Air__temperature, Cloud_Cover, Wind, Precip) |> 
  rename(surveyNumber = Survey_No, time = Time, site = Site, meanAirTemp= Air__temperature,
         cloudCover = Cloud_Cover, wind = Wind, precip = Precip)



weatherTable<- weatherTable |> 
  mutate(Date = as.Date(Date)) |> 
  arrange(Date) |> 
  mutate(date_group = cumsum(Date != lag(Date, default = first(Date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(Date = as.character(Date))

dbAppendTable(con, "creelWeather", weatherTable) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work


############################################################################

questionTable<- creelData |> 
  colnames()

questionTable <- questionTable |> 
  as.data.frame() |> 
  slice(32:43) |> 
  rename(question = questionTable)

query <- "SELECT * FROM creelSurveyQuestions"
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

dbAppendTable(con, "creelSurveyQuestions", adding) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

#####################################################################################

query <- "SELECT * FROM creelSurveyQuestions"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

answersTable<- creelData |> 
  select(c(Survey_No, Time,Date, contains(df$question)))


answersLong <- answersTable |>
  pivot_longer(cols = -c(Survey_No, Time, Date), names_to = "Question", values_to = "Answer") |> 
  left_join(df, by = c("Question" = "question")) |> 
  mutate(Question = questionID) |> 
  select(-questionID) |> 
  rename( surveyNumber = Survey_No, time = Time, questionID = Question, answer = Answer)  |> 
  mutate(time = as.character(time))

answersLong<- answersLong |> 
  mutate(time = as.character(time),
         Date = as.character(Date))

answersLong<- answersLong |>
  rename(date = Date) |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))


dbAppendTable(con, "creelSurveyAnswers", answersLong) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work


#####################################################################################################





query <- "SELECT * FROM creelSurveyAnswers"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)




#################################################################


# 
# ICE2<- ICE |> 
#   rename(date = Date, time = Time, noAnglingBoats = X..angling.boats, noBoatAnglers = X..boat.anglers, 
#          noShoreAnglers = X..shore.anglers, noDockAnglers=X..dock.anglers, airTemp = Air.temperature,
#          cloudCover = Cloud.cover, wind = Wind, precip = Precipitation) |> 
#   mutate(weather = paste0("Air temperature: ", airTemp,", cloud cover: ", cloudCover, ", wind: ", wind,", precipiation: ", precip)) |> 
#   select(date, time, noAnglingBoats, noBoatAnglers, noShoreAnglers, noDockAnglers, weather)
# 
# 
# dbAppendTable(con, "iceData", ICE2) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work



fishResults<- creelData |> 
  select(Survey_No, Date, Time, Total_Fish_Caught, Total_Retained, ends_with("_c"), ends_with("_r"),
         Release_Reason, Total_Person_Hr_Fished, No_Anglers, No_Rods, Vessel, Preferred_Catch_Spp, Site) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, totFishCaught = Total_Fish_Caught,
         totRetained = Total_Retained, releaseReason = Release_Reason, personHours = Total_Person_Hr_Fished,
         noAnglers = No_Anglers, noRods = No_Rods, vessl = Vessel, preferredSpp = Preferred_Catch_Spp, site = Site ) |>
  rename_with(~ str_replace_all(.x, "_", ""), everything()) |> 
  mutate(date = as.character(date), time = as.character(time)) 

fishResults <- fishResults |>
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))

dbAppendTable(con, "creelFishResults", fishResults)

##########################################################################

demo<-demography[[1]]

demo<- demo |> 
  rename(surveyNumber = `Survey #`, dateTime = `Date & Time`, gender = Gender, ageClass = `Age Class`, 
         licensePeriod = `License Period`, residency = Residency, cityProvinceCountry = `City, Prov, Country`, 
         postCode = `Postal Code (first 3)`, notes = Notes)

#anglerData<- 
anglerData<- demo |> 
  mutate(date = as.Date(dateTime), time = as_datetime(dateTime)) |> 
  mutate(date = as.Date(time)) |> 
  mutate(date = as.character(date), time = as.character(time)) |> 
  mutate(anglerID = row_number()) |> 
  select(-dateTime)




anglerData<- anglerData |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))

query <- "SELECT * FROM creelFisherDemography"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)


anglerData<- anglerData |> 
  mutate(anglerID = max(df$anglerID) + row_number())


dbAppendTable(con, "creelFisherDemography", anglerData)

#################################################################################################


################### Add the fish data to creelFishDetails
query <- "SELECT * FROM creelFishDetails"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

fd<-fishdata[[1]]

fishd <-fd |> 
  rename(surveyNumber = `Survey #`, dateTime = `Date & Time`, fishNo = `Fish #`,
         length = `Length, mm`, weight = `Weight, g`, pitTagNo = `Pit Tag #`, acousticTagNo = `Acoustic tag #`, 
         notes = Notes)

fishd <- fishd |>
  mutate(date = as.Date(dateTime), time = as_datetime(dateTime)) |> 
  mutate(date = as.Date(time)) |> 
  mutate(date = as.character(date), time = as.character(time)) |> 
  select(-dateTime)

fishd<- fishd |> 
  mutate(fishID = max(df$fishID) + row_number())

#add the new columns
dbExecute(con, "ALTER TABLE creelFishDetails ADD COLUMN acousticTagNo TEXT")

dbAppendTable(con, "creelFishDetails", fishd)

dbDisconnect(con)
