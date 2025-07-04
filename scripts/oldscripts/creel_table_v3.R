library(sqldf)
library(dplyr)
library(lubridate)
library(tidyr)
source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")
names(fish_edit)[names(fish_edit) == "...8"]<-"Weather"



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

############################################

# Shifts

shifts <- main_page |> 
  select(Survey_No, Date, Time, Surveyor, Shift) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, surveyor = Surveyor, shift = Shift) |> 
  mutate(date = as.character(date), time = as.character(time), shift = as.character(shift)) 

#overwrite the survey_No column as per Amalis - this makes sure that the survey_No starts at 1 every day
shifts <- shifts |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))

shifts <- shifts |> 
  mutate(time = ymd_hms(time)) |> 
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time))

shifts <- shifts |> 
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

sur_col_types <- get_col_types(shifts)

sur_col_types


sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber")  ~ "KEY",
    col_name %in% c("date") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))


sql = paste0("CREATE TABLE IF NOT EXISTS creelShifts (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (\nsurveyNumber\n, \ndate\n))")

dbExecute(con, sql)
#DBI::dbListTables(con)

dbWriteTable(conn = con, "creelShifts", shifts, row.names = F, append = T)

# query <- "SELECT * FROM creelShifts"
# dbExecute(con = con, query)
# #querydelete<-"DROP TABLE surveyData"
# result <- dbSendQuery(conn = con, query)
# df<-fetch(result, -1)
# df
# dbClearResult(result)

#####################################################################################

# Weather

weatherTable<-main_page |> 
  select(Survey_No, Date,  Time, Site, Mean_Air_Temperature, Cloud_Cover, Wind, Precip) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, site = Site, meanAirTemp= Mean_Air_Temperature,
         cloudCover = Cloud_Cover, wind = Wind, precip = Precip) |> 
  mutate(time = as.character(time)) 

weatherTable<- weatherTable |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))

weatherTable <- weatherTable |> 
  mutate(time = ymd_hms(time)) |> 
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time))


col_types<-get_col_types(weatherTable)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber", "time", "site") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))


sql = paste0("CREATE TABLE IF NOT EXISTS creelWeather (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyNumber, date),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES surveyData (surveyNumber, time))")
dbExecute(con, sql)
dbWriteTable(conn = con, "creelWeather", weatherTable, row.names = F, append = T)
dbListTables(con)

#####################################################################################

# Fishing results

 
catchDF<-main_page |> 
  select(Survey_No, Date, Time, Total_Fish_Caught, Total_Retained, ends_with("_c"), ends_with("_r"),
         Release_Reason, Total_Person_Hr_Fished, No_Anglers, No_Rods, Vessel, Preferred_Catch_Spp, Site) |> 
  rename(surveyNumber = Survey_No, date = Date, time = Time, totFishCaught = Total_Fish_Caught,
         totRetained = Total_Retained, releaseReason = Release_Reason, personHours = Total_Person_Hr_Fished,
         noAnglers = No_Anglers, noRods = No_Rods, vessl = Vessel, preferredSpp = Preferred_Catch_Spp, site = Site ) |>
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

catchDF <- catchDF |> 
  mutate(time = ymd_hms(time)) |> 
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time))


sur_col_types <- get_col_types(catchDF)

sur_col_types


sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber, date")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS creelFishResults (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyNumber, date),
             \nFOREIGN KEY (surveyNumber, time) REFERENCES surveyData (surveyNumber, time),
             \nFOREIGN KEY (surveyNumber, time) REFERENCES anglerInfo (surveyNumber, time)
             )")
dbExecute(con, sql)
DBI::dbListTables(con)



dbWriteTable(conn = con, "creelFishResults", catchDF, row.names = F, append = T)

query <- "SELECT * FROM creelFishResults"
#querydelete<-"DROP TABLE creelFishResults"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
rowtoadd<-df[1,]
rowtoadd$date<-"2024-01-01"

DBI::dbListTables(con)
##sucess! the foreign key means there must be a corresponding date added to surveyData 
## before the fishCatch date can be updated.
#dbWriteTable(con = con, name = "fishCatch", value = rowtoadd, append = T, row.names = F)

#############################################################################

# Survey Questions and Answers


questionTables<-main_page |> 
  colnames()

questionTables <- questionTables |> 
  as.data.frame() |> 
  slice(34:45) |> 
  mutate(questionID = row_number()) |> 
  rename(question = questionTables)

answersTable<- main_page |> 
  select(c(Survey_No, Date, Time, contains(questionTables$question)))

answersLong <- answersTable %>%
  pivot_longer(cols = -c(Survey_No, Time, Date), names_to = "Question", values_to = "Answer") |> 
  left_join(questionTables, by = c("Question" = "question")) |> 
  mutate(Question = questionID) |> 
  select(-questionID) |> 
  rename( surveyNumber = Survey_No, date = Date, time = Time, questionID = Question, answer = Answer) |> 
  mutate(time = as.character(time))

answersLong<- answersLong |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  # group_by(date_group) |> 
  # mutate(surveyNumber = row_number()) |>  
  # ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))

answersLong <- answersLong |> 
  mutate(time = ymd_hms(time)) |> 
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time))

answersLong <- answersLong |> 
  mutate(date = as.character(date))

col_types<-get_col_types(answersLong)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber", "time", "questionID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))




sql = paste0("CREATE TABLE IF NOT EXISTS creelSurveyAnswers (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyNumber, time, questionID),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES anglerInfo (surveyNumber, time),
              \nFOREIGN KEY (surveyNumber, time) REFERENCES surveyData (surveyNumber, time)
             )")



dbExecute(con, sql)
dbWriteTable(conn = con, "creelSurveyAnswers", answersLong, row.names = F, append = T)

col_types<-get_col_types(questionTables)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("question", "questionID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))



sql = paste0("CREATE TABLE IF NOT EXISTS creelSurveyQuestions (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (questionID),
              \nFOREIGN KEY (questionID) REFERENCES surveyAnswers (question))")

dbExecute(con, sql)
dbWriteTable(conn = con, "creelSurveyQuestions", questionTables, row.names = F, append = T)

query <- "SELECT * FROM creelSurveyQuestions"
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

#############################################################################

# Demography

anglerData<- demography_edit |> 
  select(Survey, Date_Time, Gender, Age_Class, License_Period, Residency,
         City_Prov_Country, Postal_Code_first_3, Notes) |> 
  rename(surveyNumber = Survey, time = Date_Time, gender=Gender, AgeClass = Age_Class,
         licensePeriod = License_Period, residency = Residency, cityProvinceCountry = City_Prov_Country,
         postCode = Postal_Code_first_3, notes = Notes) |> 
  mutate(date = as.Date(time)) |> 
  mutate(date = as.character(date), time = as.character(time)) |> 
  mutate(anglerID = row_number())




anglerData<- anglerData |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))

# anglerData <- anglerData |> 
#   mutate(time = ymd_hms(time)) |> 
#   group_by(time) |> 
#   mutate(
#     time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
#   ) |> 
#   ungroup() |> 
#   mutate(time = as.character(time))

anglerData <- anglerData |> 
  mutate(
    time = if_else(
      nchar(time) == 10,  # If time only contains a date (YYYY-MM-DD)
      paste(time, "12:00:00"),  # Append "12:00:00" to the date
      time  # Keep original time if already complete
    )
  ) |> 
  mutate(time = ymd_hms(time)) |>  # Convert to datetime
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time))  # Convert back to character if needed


col_types<-get_col_types(anglerData)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("anglerID","surveyNumber", "time", "date") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sur_col_types_sql

sql = paste0("CREATE TABLE IF NOT EXISTS creelFisherDemography (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (anglerID),
              \nFOREIGN KEY (surveyNumber, date) REFERENCES surveyData (surveyNumber, date))")
# 
# drop_table_sql <- paste0("DROP TABLE IF EXISTS anglerInfo;")
# 
# # Execute the SQL to drop the table
# dbExecute(con, drop_table_sql)

dbExecute(con, sql)
dbWriteTable(conn = con, "creelFisherDemography", anglerData, row.names = F, append = T)
query <- "SELECT * FROM creelFisherDemography"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

##################################

# Fish measurements

fishcaught<- fish_edit |> 
  rename(surveyNumber = Survey_No, time = Date_Time, fishNo = Fish_No, length = Length_mm, weight = Weight_g, 
         pitTagNo = PIT_tag_No, notes = Notes) |> 
  mutate(date = format(as.Date(time), "%Y-%m-%d")) |> 
  mutate(date = as.character(date), time = as.character(time),
         fishID = row_number())

fishcaught<- fishcaught |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))

fishcaught <- fishcaught |> 
  mutate(time = ymd_hms(time)) |> 
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time))


sur_col_types <- get_col_types(fishcaught)




sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber, time, fishID")  ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sur_col_types_sql$a <- ifelse(
  grepl("surveyNumber|fishID", sur_col_types_sql$a), 
  sur_col_types_sql$a, 
  sub(" .*", " TEXT", sur_col_types_sql$a)
)

sql = paste0("CREATE TABLE IF NOT EXISTS creelFishDetails (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY ( \n fishID \n))")


dbExecute(con, sql)
DBI::dbListTables(con)

dbWriteTable(conn = con, "creelFishDetails", fishcaught, row.names = F, append = T)

query <- "SELECT * FROM creelFishDetails"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)



dbDisconnect(con)


