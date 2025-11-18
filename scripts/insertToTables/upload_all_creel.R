library(tidyverse)
library(sqldf)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

main_page<-read_xlsx("./output/Creel_Survey_Cleaned.xlsx", sheet = "Main Data", col_names = T)
demo<-read_xlsx("./output/Creel_Survey_Cleaned.xlsx", sheet = "Demographic Data", col_names = T)
fish<-read_xlsx("./output/Creel_Survey_Cleaned.xlsx", sheet = "Fish Data", col_names = T)
ICE<-read_xlsx("./output/Creel_Survey_Cleaned.xlsx", sheet = "ICE", col_names = T)

#### Delete the previous instances of the creel data from the database
print(dbListObjects(con))


droptable<-"DROP TABLE IF EXISTS creel_main"
dbExecute(con, droptable)
droptable<-"DROP TABLE IF EXISTS creelMain"
dbExecute(con, droptable)
droptable<-"DROP TABLE IF EXISTS creelSurveyQuestions"
dbExecute(con, droptable)
droptable<-"DROP TABLE IF EXISTS creelSurveyAnswers"
dbExecute(con, droptable)
droptable<-"DROP TABLE IF EXISTS creelShifts"
dbExecute(con, droptable)
droptable<-"DROP TABLE IF EXISTS creelICE"
dbExecute(con, droptable)
droptable<-"DROP TABLE IF EXISTS creelFish"
dbExecute(con, droptable)
droptable<-"DROP TABLE IF EXISTS creelFisherDemography"
dbExecute(con, droptable)


print(dbListObjects(con))


offset_time <- function(df) {
  if ("time" %in% names(df)) {
    df <- df |>
      mutate(time = hms::as_hms(time)) |>
      group_by(time) |>
      mutate(
        time = hms::as_hms(as.numeric(time) + 300 * (row_number() - 1))  # +5 min per duplicate
      ) |>
      ungroup() |>
      mutate(time = as.character(time))
  }
  return(df)
}

### Add the main table back in there - 
# main_page<-main_page |> 
#   select(c(-x2))

main<-main_page[,1:29]


main <- offset_time(main)

main$date<-as.Date(main$date)

main<-main |> 
  mutate(across(everything(), as.character)) |> 
  rename(surveyNumber = survey_number)

main<- main |> 
  mutate(surveyNumber )

unique(main)

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

##########################################

questionTables<-main_page |> 
  colnames()

questionTables <- questionTables |> 
  as.data.frame() |> 
  slice(30:ncol(main_page)) |> 
  mutate(questionID = row_number()) |> 
  rename(question = questionTables)

answersTable<- main_page |> 
  rename(surveyNumber = survey_number) |> 
  select(c(surveyNumber, date, time, contains(questionTables$question)))

answersLong <- answersTable %>%
  pivot_longer(cols = -c(surveyNumber, time, date), names_to = "Question", values_to = "Answer") |> 
  left_join(questionTables, by = c("Question" = "question")) |> 
  mutate(Question = questionID) |> 
  select(-questionID) |> 
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
  mutate(date = as.character(date))

answersLong <- offset_time(answersLong)

# answersLong = answersLong|> 
#   rename(surveyNumber = survey_number)



col_types<-get_col_types(answersLong)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("surveyNumber", "time", "date", "questionID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))




sql = paste0("CREATE TABLE IF NOT EXISTS creelSurveyAnswers (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (surveyNumber, time, Question, date),
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

###########################################################

fish_edit <- fish |>
  dplyr::mutate(across(everything(), as.character)) |> 
  mutate(fish_number = row_number()) 

fish_edit <- fish_edit |> 
  rename(surveyNumber = survey_number)

fish_edit  <- offset_time(fish_edit)



fish_edit$date<-as.Date(fish_edit$date)

fish_edit<- fish_edit |> 
  mutate(across(everything(), as.character))

fish_col_types <- get_col_types(fish_edit)

fish_col_types_sql <- fish_col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("fish_number") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS creelFish (
       ",paste0(fish_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (fish_number)
             )")
dbExecute(con, sql)
dbWriteTable(conn = con, "creelFish", fish_edit, row.names = F, append = T)

#########################################################################################

demo <- demo |> 
  dplyr::mutate(anglerID = row_number())

demo <- demo |> 
  rename(surveyNumber = survey_number)

demo<- offset_time(demo)
demo$date<-as.Date(demo$date)

demo<- demo |> 
  mutate(across(everything(), as.character))

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

############################################################################################

ICE = ICE |> 
  rename(surveyNumber = survey_number)

ICE$date = as.Date(ICE$date)

ICE = ICE |> 
  mutate(across(everything(), as.character))


ICE <- offset_time(ICE)
col_types<-get_col_types(ICE)

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
dbWriteTable(conn = con, "creelICE", ICE, row.names = F, append = T)
query <- "SELECT * FROM creelICE"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)  

dbDisconnect(con)
