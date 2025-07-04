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

################################################################################################# 
# 1. Main page

main<-main_page[,1:33]

main <- main |> 
  dplyr::rename(
    surveyNumber = Survey_No,
    date = Date,
    time = Time,
    shift = Shift,
    site = Site,
    meanAirTemperature = Mean_Air_Temperature,
    cloudCover = Cloud_Cover,
    wind = Wind,
    precip = Precip,
    noAnglers = No_Anglers,
    noRods = No_Rods,
    totalPersonHrFished = Total_Person_Hr_Fished,
    totalFishCaught = Total_Fish_Caught,
    noPikeminnowCaught = No_pikeminnow_caught,
    noSMBc = No_SMB_c,
    noKOc = No_KO_c,
    noCTc = No_CT_c,
    noRBc = No_RB_c,
    noBTc = No_BT_c,
    noLTc = No_LT_c,
    noOtherSppC = No_Other_Spp_c,
    totalRetained = Total_Retained,
    noPikeminnowR = No_pikeminnow_r,
    noSMBr = No_SMB_r,
    noKOr = No_KO_r,
    noCTr = No_CT_r,
    noRBr = No_RB_r,
    noBTr = No_BT_r,
    noLTr = No_LT_r,
    noOtherSppR = No_Other_Spp_r
  )

main_t<-main |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date)) |> 
  mutate(time = ymd_hms(time)) |> 
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time)) |> 
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
         shift_end_time = as.character(paste(date, shift_end_time)))


main<-main_t |> 
  mutate(across(everything(), as.character))

sur_col_types <- get_col_types(main)

sur_col_types


sur_col_types_sql <- sur_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber")  ~ "KEY",
    col_name %in% c("date") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))


sql = paste0("CREATE TABLE IF NOT EXISTS creelMain (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (\nsurveyNumber\n, \ndate\n))")

dbExecute(con, sql)
#DBI::dbListTables(con)

dbWriteTable(conn = con, "creelMain", main, row.names = F, append = T)


#################################

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



####################################################################################
# 2. Fish edit page
fish_edit <- fish_edit |> 
  dplyr::rename(
    surveyNumber = Survey_No,
    dateTime = Date_Time,
    fishNo = Fish_No,
    spp = Spp,
    length = Length_mm,
    weight = Weight_g,
    pitTagNo = PIT_tag_No
  ) |>
  dplyr::mutate(across(everything(), as.character))

fish_edit = fish_edit |> 
  dplyr::mutate(date = as.Date(dateTime))

fish_edit <- fish_edit |>
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date)) |> 
  mutate(time = ymd_hms(dateTime)) |> 
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time))

fish_edit <- fish_edit |>
  dplyr::mutate(across(everything(), as.character)) |> 
  dplyr::select(-dateTime)

fish_col_types <- get_col_types(fish_edit)
fish_col_types_sql <- fish_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("fishNo") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))
sql = paste0("CREATE TABLE IF NOT EXISTS creelFish (
       ",paste0(fish_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (fishNo)
             )")
dbExecute(con, sql)
dbWriteTable(conn = con, "creelFish", fish_edit, row.names = F, append = T)

########################################################################################
# 3. Demography edit page

demo <- demography_edit |> 
  dplyr::rename(surveyNumber = Survey,
                dateTime = Date_Time,
                gender = Gender,
                ageClass = Age_Class,
                licensePeroid = License_Period,
                residency = Residency,
                cityProvCountry = City_Prov_Country,
                postCode = Postal_Code_first_3,
                notes = Notes) |> 
  dplyr::mutate(date = as.Date(dateTime), time =format(lubridate::ymd_hms(dateTime), "%H:%M:%S")) |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date)) |> 
  mutate(time = ymd_hms(dateTime)) |> 
  group_by(time) |> 
  mutate(
    time = if_else(row_number() > 1, time + minutes(5) * (row_number() - 1), time)
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time)) |> 
  dplyr::mutate(across(everything(), as.character)) |> 
  dplyr::select(-dateTime)

demo <- demo |> 
  dplyr::mutate(anglerID = row_number())


col_types<-get_col_types(demo)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("anglerID","surveyNumber", "time", "date") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sur_col_types_sql

sql = paste0("CREATE TABLE IF NOT EXISTS creelFisherDemography (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (anglerID))")
# 
# drop_table_sql <- paste0("DROP TABLE IF EXISTS anglerInfo;")
# 
# # Execute the SQL to drop the table
# dbExecute(con, drop_table_sql)

dbExecute(con, sql)
dbWriteTable(conn = con, "creelFisherDemography", demo, row.names = F, append = T)
query <- "SELECT * FROM creelFisherDemography"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)  
######################################################################################
# 1. ICE

ice <-ICE |> 
  dplyr::rename(
    date = Date, time = Time, noAnglingBoats = No_angling_boats, noBoatAnglers = No_boat_anglers,
    noShoreAnglers = No_shore_anglers, nodocAnglers = No_dock_anglers, comments = Comments, 
    weather = Weather
  ) |> 
  dplyr::mutate(date = as.Date(date), time =format(lubridate::ymd_hms(time), "%H:%M:%S")) |> 
  mutate(date = as.Date(date)) |> 
  arrange(date) |> 
  mutate(date_group = cumsum(date != lag(date, default = first(date)))) |> 
  group_by(date_group) |> 
  mutate(surveyNumber = row_number()) |>  
  ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date)) |> 
  dplyr::mutate(across(everything(), as.character))

  

col_types<-get_col_types(ice)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber", "time", "date") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sur_col_types_sql

sql = paste0("CREATE TABLE IF NOT EXISTS creelICE (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (date, surveyNumber))")
# 
# drop_table_sql <- paste0("DROP TABLE IF EXISTS anglerInfo;")
# 
# # Execute the SQL to drop the table
# dbExecute(con, drop_table_sql)

dbExecute(con, sql)
dbWriteTable(conn = con, "creelICE", ice, row.names = F, append = T)
query <- "SELECT * FROM creelICE"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)  
