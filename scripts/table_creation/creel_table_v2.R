library(sqldf)
library(dplyr)
source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")
names(fish_edit)[names(fish_edit) == "...8"]<-"Weather"



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

survDT<- main_page |> 
  select(Survey_No, Date, Time, Surveyor, Shift) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, surveyor = Surveyor, shift = Shift) |> 
  mutate(date = as.character(date), time = as.character(time), shift = as.character(shift)) 

sur_col_types <- get_col_types(survDT)

sur_col_types


sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber, time")  ~ "KEY",
    col_name %in% c("date") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS surveyData (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (\nsurveyNumber\n, \ntime\n))")


dbExecute(con, sql)
DBI::dbListTables(con)

dropall<-"DELETE FROM surveyData"
result <- dbSendQuery(conn = con, dropall)
dbWriteTable(conn = con, "surveyData", survDT, row.names = F, append = T)

query <- "SELECT * FROM surveyData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

#####################################################################################################

anglerData<- demography_edit |> 
  select(Survey, Date_Time, Gender, Age_Class, License_Period, Residency,
         City_Prov_Country, Postal_Code_first_3, Notes) |> 
  rename(surveyNumber = Survey, time= Date_Time, gender=Gender, AgeClass = Age_Class,
         licensePeriod = License_Period, residency = Residency, cityProvinceCountry = City_Prov_Country,
         postCode = Postal_Code_first_3, notes = Notes) |> 
  mutate(date = as.Date(time)) |> 
  mutate(date = as.character(date), time = as.character(time)) |> 
  mutate(anglerID = row_number())


col_types<-get_col_types(anglerData)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("anglerID","surveyNumber", "time", "date") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sur_col_types_sql

sql = paste0("CREATE TABLE IF NOT EXISTS anglerInfo (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (anglerID),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES surveyData (surveyNumber, time))")
# 
# drop_table_sql <- paste0("DROP TABLE IF EXISTS anglerInfo;")
# 
# # Execute the SQL to drop the table
# dbExecute(con, drop_table_sql)

dbExecute(con, sql)
dbWriteTable(conn = con, "anglerInfo", anglerData, row.names = F, append = T)
query <- "SELECT * FROM anglerInfo"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

DBI::dbListTables(con)

####################################################################################################


catchDF<-main_page |> 
  select(Survey_No, Date, Time, Total_Fish_Caught, Total_Retained, ends_with("_c"), ends_with("_r"),
         Release_Reason) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, totFishCaught = Total_Fish_Caught,
         totRetained = Total_Retained, releaseReason = Release_Reason) |>
  rename_with(~ str_replace_all(.x, "_", ""), everything()) |> 
  mutate(date = as.character(date), time = as.character(time)) 


sur_col_types <- get_col_types(catchDF)

sur_col_types


sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber, time")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS fishCatch (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyNumber, time),
             \nFOREIGN KEY (surveyNumber, time) REFERENCES surveyData (surveyNumber, time),
             \nFOREIGN KEY (surveyNumber, time) REFERENCES anglerInfo (surveyNumber, time)
             )")
dbExecute(con, sql)
DBI::dbListTables(con)



dbWriteTable(conn = con, "fishCatch", catchDF, row.names = F, append = T)

query <- "SELECT * FROM fishCatch"
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
rowtoadd<-df[1,]
rowtoadd$date<-"2024-01-01"

DBI::dbListTables(con)
##sucess! the foreign key means there must be a corresponding date added to surveyData 
## before the fishCatch data can be updated.
#dbWriteTable(con = con, name = "fishCatch", value = rowtoadd, append = T, row.names = F)

######################################################################################################


fishingDetails<- main_page |> 
  select(Survey_No, Date, Time, No_Anglers, No_Rods, Total_Person_Hr_Fished, Vessel,
         Preferred_Catch_Spp, Site) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, numberAnglers = No_Anglers, numberRods = No_Rods, 
         personHoursFished = Total_Person_Hr_Fished, vessel = Vessel, prefferedSpp = Preferred_Catch_Spp,
         site = Site) |>   
  mutate(date = as.character(date), time = as.character(time))

col_types<-get_col_types(fishingDetails)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("date", "surveyNumber", "time", "site") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS fishingDetails (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyNumber, time),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES fishCatch (surveyNumber, time),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES anglerInfo (surveyNumber, time)
              )")
dbExecute(con, sql)
dbWriteTable(conn = con, "fishingDetails", fishingDetails, row.names = F, append = T)
query <- "SELECT * FROM fishingDetails"
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)
#######################################################################################################

weatherTable<-main_page |> 
  select(Survey_No, Time, Site, Mean_Air_Temperature, Cloud_Cover, Wind, Precip) |> 
  rename(surveyNumber = Survey_No, time = Time, site = Site, meanAirTemp= Mean_Air_Temperature,
         cloudCover = Cloud_Cover, wind = Wind, precip = Precip) |> 
  mutate(time = as.character(time)) 

col_types<-get_col_types(weatherTable)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber", "time", "site") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))


sql = paste0("CREATE TABLE IF NOT EXISTS weatherDetails (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyNumber, time, site),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES surveyData (surveyNumber, time))")
dbExecute(con, sql)
dbWriteTable(conn = con, "weatherDetails", weatherTable, row.names = F, append = T)
dbListTables(con)
#####################################################################################################

library(tidyr)
questionTables<-main_page |> 
  colnames()

questionTables <- questionTables |> 
  as.data.frame() |> 
  slice(34:45) |> 
  mutate(questionID = row_number()) |> 
  rename(question = questionTables)

answersTable<- main_page |> 
  select(c(Survey_No, Time, contains(questionTables$question)))

answersLong <- answersTable %>%
  pivot_longer(cols = -c(Survey_No, Time), names_to = "Question", values_to = "Answer") |> 
  left_join(questionTables, by = c("Question" = "question")) |> 
  mutate(Question = questionID) |> 
  select(-questionID) |> 
  rename( surveyNumber = Survey_No, time = Time, questionID = Question, answer = Answer) |> 
  mutate(time = as.character(time))


col_types<-get_col_types(answersLong)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber", "time" , "questionID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))




sql = paste0("CREATE TABLE IF NOT EXISTS surveyAnswers (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyNumber, time, questionID),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES anglerInfo (surveyNumber, time),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES surveyData (surveyNumber, time)
             )")

dbExecute(con, sql)
dbWriteTable(conn = con, "surveyAnswers", answersLong, row.names = F, append = T)

col_types<-get_col_types(questionTables)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("question", "questionID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS surveyQuestions (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (questionID),
              \nFOREIGN KEY (questionID) REFERENCES surveyAnswers (question))")

dbExecute(con, sql)
dbWriteTable(conn = con, "surveyQuestions", questionTables, row.names = F, append = T)

query <- "SELECT * FROM surveyQuestions"
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)
#######################################################################################################

  
names(ICE)<-names_fix(names(ICE))
names(ICE)<-gsub("_", "", names(ICE))
ICE<-ICE |> 
  rename(date = Date, time = Time, noAnglingBoats = Noanglingboats, noBoatAnglers = Noboatanglers,
         noShoreAnglers = Noshoreanglers, noDockAnglers = Nodockanglers, comments = Comments,
         weather = Weather) |> 
  mutate(date = as.character(date), time = as.character(time)) |> 
  mutate(iceID = row_number())

sur_col_types <- get_col_types(ICE)

sur_col_types


sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("iceID")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS iceData (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY ( \niceID\n))")


dbExecute(con, sql)
DBI::dbListTables(con)

dbWriteTable(conn = con, "iceData", ICE, row.names = F, append = T)

query <- "SELECT * FROM iceData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

#############################################################################################

fishcaught<- fish_edit |> 
  rename(surveyNumber = Survey_No, time = Date_Time, fishNo = Fish_No, length = Length_mm, weight = Weight_g, 
         pitTagNo = PIT_tag_No, notes = Notes) |> 
  mutate(date = format(as.Date(time), "%Y-%m-%d")) |> 
  mutate(date = as.character(date), time = as.character(time),
         fishID = row_number())

sur_col_types <- get_col_types(fishcaught)

sur_col_types


sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNo, time, fishID")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS fishCaught (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY ( \n fishID \n))")


dbExecute(con, sql)
DBI::dbListTables(con)

dbWriteTable(conn = con, "fishCaught", fishcaught, row.names = F, append = T)

query <- "SELECT * FROM fishCaught"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)



dbDisconnect(con)
