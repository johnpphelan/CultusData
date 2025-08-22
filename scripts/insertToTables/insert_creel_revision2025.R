library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
source("scripts/utils/fix_col_names_f.R")

lan_folder = "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2025 projects/Creel Surveys/"


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

the_files = list.files("//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2025 projects/Creel Surveys/Archived Creel Forms/",
                       pattern = "*.xlsx", recursive = T, full.names = TRUE)

sheet_names = readxl::excel_sheets(the_files[1])

files_read_l = the_files |> 
  lapply(
    \(x) {
      all_sheets = readxl::excel_sheets(x)
      all_sheets |> 
        purrr::map( ~ {
          readxl::read_excel(x, sheet = .x, col_types = 'text')
        }) |> 
        purrr::set_names(all_sheets)
    }
  )



files_read_f = list_flatten(files_read_l)

sheets_combineds<-sheet_names |> 
  map( ~ {
    files_read_f[which(names(files_read_f) == .x)] |> 
      dplyr::bind_rows()
  })
names(sheets_combineds)<-sheet_names

main<-sheets_combineds$`Main Data`
fish_data<-sheets_combineds$`Fish Data`
demo<-sheets_combineds$`Demographic Data`
ICE<-sheets_combineds$`ICE`



####################################################################
# 1. Insert main

main_data<-main[1:32,] |> 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
         datetime = as.POSIXct(as.numeric(Time), origin = "1899-12-30", tz = "UTC"),
         Date = as.Date(datetime),
         Time = hms::as_hms(format(datetime, "%H:%M:%S"))) |>
  select(-datetime)


query <- "SELECT * FROM creelMain"
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

main_data |> 
  rename(surveyNumber = `Survey #`, surveyor = Surveyor, date = Date, time = Time,
         shift = Shift, site = Site)




main<-main |> 
  dplyr::rename(
    surveyNumber = Survey_No,
    date = Date,
    time = Time,
    shift = Shift,
    site = Site,
    meanAirTemperature = Air__temperature,
    cloudCover = Cloud_Cover,
    wind = Wind,
    precip = Precip,
    noAnglers = No_Anglers,
    noRods = No_Rods,
    totalPersonHrFished = Total_Person_Hr_Fished,
    totalFishCaught = Total_Fish_Caught,
    noSMBc = No_SMB_c16,
    noKOc = No_KO_c,
    noCTc = No_CT_c,
    noRBc = No_RB_c,
    noBTc = No_BT_c,
    noLTc = No_LT_c,
    noOtherSppC = No_Other_Spp_c,
    totalRetained = Total_Retained,
    noSMBr = No_SMB_c24,
    noKOr = No_KO_r,
    noCTr = No_CT_r,
    noRBr = No_RB_r,
    noBTr = No_BT_r,
    noLTr = No_LT_r,
    noOtherSppR = No_Other_Spp_r
  ) |>
  dplyr::mutate(across(everything(), as.character))

main<-main |> 
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


main<-main |> 
  mutate(across(everything(), as.character))

main_t <- main |> 
  select(-Date_2)

dbAppendTable(con, "creelMain", main_t)

###########################################################################################

questionTable<- creelData |> 
  colnames()

questionTable <- questionTable |> 
  as.data.frame() |> 
  slice(33:43) |> 
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
  select(c(Survey_No, Time, Date, contains(df$question)))


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
  # group_by(date_group) |> 
  # mutate(surveyNumber = row_number()) |>  
  # ungroup() |> 
  select(-date_group) |> 
  mutate(date = as.character(date))


dbAppendTable(con, "creelSurveyAnswers", answersLong) ## adds these - if the entry (surveyNumber and time) already exists, then it won't work

###########################################################################################

# 2. Insert demography

demo<-demography[[1]]

query <- "SELECT * FROM creelFisherDemography"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
dbClearResult(result)


demo<-demo |> 
  dplyr::rename(surveyNumber = `Survey #`,
                dateTime = `Date & Time`,
                gender = Gender,
                ageClass = `Age Class`,
                licensePeroid = `License Period`,
                residency = Residency,
                cityProvCountry = `City, Prov, Country`,
                postCode = `Postal Code (first 3)`,
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

max_ID<-df$anglerID |> 
  max(na.rm = TRUE)

demo <- demo |> 
  dplyr::mutate(anglerID = row_number()+max_ID)

dbAppendTable(con, "creelFisherDemography", demo)

###########################################################################################

# 3. Insert fish data

fishData <- fishdata[[1]]

query <- "SELECT * FROM creelFish"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
dbClearResult(result)

fishData<-fishData |> 
  dplyr::rename(surveyNumber = `Survey #`,
                time = `Date & Time`,
                date = `Date & Time`,
                spp = Spp,
                fishNo = `Fish #`,
                pitTagNo = `Pit Tag #`,
                acousticTagNo = `Acoustic tag #`,
                length = `Length, mm`,
                weight = `Weight, g`,
                notes = Notes) |> 
  dplyr::mutate(date = as.Date(date), time =format(lubridate::ymd_hms(date), "%H:%M:%S")) |> 
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
  dplyr::mutate(across(everything(), as.character))

maxfishid<-df$fishNo |> 
  max(na.rm = TRUE) |> 
  as.numeric()
fishData <- fishData |>
  dplyr::mutate(fishNo = row_number() + maxfishid)

dbExecute(con, "ALTER TABLE creelFish ADD COLUMN acousticTagNo TEXT")
dbAppendTable(con, "creelFish", fishData)
###########################################################################################

# 4. Insert ice data

query <- "SELECT * FROM creelICE"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
dbClearResult(result)
maxsurvey<-df$surveyNumber |> 
  max(na.rm = TRUE) |> 
  as.numeric()

ICE <- ICE |> 
  dplyr::rename(
    date = Date,
    time = Time,
    noAnglingBoats = X..angling.boats,
    noBoatAnglers = X..boat.anglers,
    noShoreAnglers = X..shore.anglers,
    nodocAnglers = X..dock.anglers
  ) |> 
  dplyr::mutate(
    weather = paste0(
      "Air Temp: ", Air.temperature,
      " Cloud: ", Cloud.cover,
      " Wind: ", Wind,
      " Precip: ", Precipitation
    )
  )

ICE <- ICE |>
  dplyr::mutate(across(everything(), as.character)) |> 
  dplyr::select(-Air.temperature, -Cloud.cover, -Wind, -Precipitation)

ICE <- ICE |> 
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

dbAppendTable(con, "creelICE", ICE)
