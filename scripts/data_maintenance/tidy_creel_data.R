library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(hms)
library(purrr)
library(skimr)
library(knitr)
library(kableExtra)
library(dplyr)
library(plotly)
library(tidyr)
library(htmltools)
library(lubridate)
library(plotly)
library(patchwork)
library(stringr)

library(sqldf)
library(DBI)

source("scripts/utils/fix_col_names_f.R")

lan_folder <- "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

the_files <- c(list.files(c(
  # No raw files in 2023, we could add these in later
  #"//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2023 projects/Creel Surveys/Archived Creel Forms/",
  "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2024 projects/Creel Surveys/Creel Survey forms 2024 (backup)/",
  "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2025 projects/Creel Surveys/Archived Creel Forms/"
),
pattern = "*.xlsx", recursive = TRUE, full.names = TRUE),
"//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2023 projects/Creel Surveys/2023 SMB creel_final not edits.xlsx"
)



the_files <- the_files[!grepl("~$", the_files)] # Remove temp files

sheet_names <- readxl::excel_sheets(the_files[1])

files_read_l <- the_files |> 
  lapply(function(x) {
    all_sheets <- readxl::excel_sheets(x)
    all_sheets |> 
      purrr::map(~ {
        # read everything as text
        dat_text <- readxl::read_excel(x, sheet = .x, col_types = "text")
        
        # rename special columns as you had before
        names(dat_text)[names(dat_text) == "# SMB c...15"] <- "# SMB c"
        names(dat_text)[names(dat_text) == "# SMB c...23"] <- "# SMB r"
        
        # clean names
        janitor::clean_names(dat_text)
      }) |> purrr::set_names(all_sheets)
  })

files_read_f <- list_flatten(files_read_l)



sheets_combined <- sheet_names |> 
  map(~ {
    files_read_f[which(names(files_read_f) == .x)] |> 
      bind_rows()
  })

names(sheets_combined) <- sheet_names

#################################################################

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

#################################################################

main <- sheets_combined$`Main Data`

# Remove duplicate survey_number/date/time combos and fill missing values
main <- main |>
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) |>
  group_by(survey_number, date, time) |>
  fill(everything(), .direction = "downup") |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(date = as.character(date)) |>
  arrange(date) |>
  group_by(as.character(date)) |>
  mutate(survey_number = row_number()) |>
  ungroup() |>
  select(-`as.character(date)`) |>
  mutate(
    date = as.character(date),
    time = hms::as_hms(as.numeric(time) %% 1 * 86400)
  )

# Adjust times to prevent overlap
main <- main |>
  group_by(time) |>
  mutate(
    time = if_else(
      row_number() > 1,
      hms::as_hms(as.numeric(time) + 5 * 60 * (row_number() - 1)),
      time
    )
  ) |>
  ungroup() |>
  mutate(time = as.character(time))

# Process shift start/end and calculate hours worked
main <- main |>
  mutate(
    shift = str_remove(shift, "^\\d+: "),
    shift_end = sub(".*-", "", shift),
    shift_start = sub("-.*", "", shift),
    shift_start = if_else(str_detect(shift_start, "^\\d{1,2}:\\d{2}$"), paste0(shift_start, ":00"), shift_start),
    shift_end = if_else(str_detect(shift_end, "^\\d{1,2}:\\d{2}$"), paste0(shift_end, ":00"), shift_end),
    shift_start_time = hms::parse_hms(shift_start),
    shift_end_time = hms::parse_hms(shift_end),
    hours_worked = as.numeric(difftime(shift_end_time, shift_start_time, units = "hours")),
    shift_start_time = paste(date, shift_start_time),
    shift_end_time = paste(date, shift_end_time)
  ) |>
  relocate(shift_end:hours_worked, .before = everything())

# Convert Excel serial dates in "wind" to M-D format where applicable
main <- main |>
  mutate(
    wind = case_when(
      suppressWarnings(!is.na(as.numeric(wind))) ~ {
        date_val <- as.Date(as.numeric(wind), origin = "1899-12-30")
        paste0(month(date_val), "-", day(date_val))
      },
      TRUE ~ wind
    )
  )

# Reorder and consolidate temperature and wind columns
main <- main |>
  select(
    survey_number:total_fish_caught,
    number_smb_c:number_other_spp_c,
    number_pikeminnow_caught,
    total_retained,
    number_smb_r:number_other_spp_r,
    number_pikeminnow_r,
    everything()
  ) |>
  mutate(
    air_temp = coalesce(air_temp, air_temperature, mean_air_temperature),
    wind = coalesce(wind, x44)
  ) |>
  select(-c(air_temperature, x44))

# -------------------------------
# Prepare final main page
# -------------------------------

main_page <- main[, 1:37] |>
  rename(
    fishing_method = vessel,
    number_pikeminnow_c = number_pikeminnow_caught
  ) |>
  filter(!is.na(date)) |>
  relocate(shift_end:hours_worked, .after = everything())

# Standardize column names: title case temporarily for readability renaming
main_page <- main_page |>
  rename_with(~ str_replace_all(., "_", " ")) |>
  rename_with(str_to_title) |>
  rename(
    `Number SMB Caught` = `Number Smb C`,
    `Number KO Caught` = `Number Ko C`,
    `Number Cutthroat Trout Caught` = `Number Ct C`,
    `Number Rainbow Trout Caught` = `Number Rb C`,
    `Number Bull Trout Caught` = `Number Bt C`,
    `Number Lake Trout Caught` = `Number Lt C`,
    `Number SMB Retained` = `Number Smb R`,
    `Number KO Retained` = `Number Ko R`,
    `Number Cutthroat Trout Retained` = `Number Ct R`,
    `Number Rainbow Trout Retained` = `Number Rb R`,
    `Number Bull Trout Retained` = `Number Bt R`,
    `Number Lake Trout Retained` = `Number Lt R`
  ) |>
  rename(survey_number = `Survey Number`)

# Final rename: back to lowercase_with_underscores
main_page <- main_page |>
  rename_with(~ gsub(" ", "_", tolower(.))) |>
  rename(
    number_pikeminnow_caught = number_pikeminnow_c,
    number_other_spp_caught = number_other_spp_c,
    number_pikeminnow_retained = number_pikeminnow_r,
    number_other_spp_retained = number_other_spp_r
  )

# -------------------------------
# Add shift type classification
# -------------------------------

main_page <- main_page |>
  mutate(
    shift_start = as_hms(shift_start),
    shift_end = as_hms(shift_end),
    shift_type = case_when(
      hour(shift_start) >= 6 & hour(shift_start) < 12 ~ "Morning",
      hour(shift_start) >= 12 & hour(shift_start) < 18 ~ "Afternoon",
      hour(shift_start) >= 18 & hour(shift_start) < 24 ~ "Evening",
      TRUE ~ "Other"
    ),
    shift_type = factor(shift_type, levels = c("Morning", "Afternoon", "Evening", "Other"))
  )


# --- Ensure all columns are character for consistent import ---
main_page <- main_page |>
  mutate(across(everything(), as.character))

# --- Define SQLite column types ---
get_col_types <- function(df) {
  tibble(
    col_name = names(df),
    sqlite_type = sapply(df, function(x) {
      if (all(grepl("^\\d+$", x[!is.na(x)]))) {
        "INTEGER"
      } else if (all(suppressWarnings(!is.na(as.numeric(x[!is.na(x)]))))) {
        "REAL"
      } else {
        "TEXT"
      }
    })
  )
}

# --- Get column types ---
sur_col_types <- get_col_types(main_page)

# --- Mark key columns (survey_number + date) ---
sur_col_types_sql <- sur_col_types |>
  mutate(
    key_status = case_when(
      col_name %in% c("survey_number", "date") ~ "KEY",
      TRUE ~ ""
    ),
    sql_def = paste0(col_name, " ", toupper(sqlite_type), " ", key_status)
  )

# --- Build CREATE TABLE statement ---
sql <- paste0(
  "CREATE TABLE IF NOT EXISTS creelMain (\n",
  paste0(sur_col_types_sql$sql_def, collapse = ",\n"),
  ",\nPRIMARY KEY (survey_number, date)\n)"
)

# --- Create table if not exists ---
dbExecute(con, sql)

# --- Write data to table ---
dbWriteTable(conn = con, name = "creel_main", value = main_page, append = TRUE, row.names = FALSE)

# --- Verify ---
dbListTables(con)
dbListFields(con, "creel_main")



###################################################################

questionTables<-main |> 
  colnames()

questionTables <- questionTables |> 
  as.data.frame() |> 
  slice(38:length(questionTables)) |> 
  mutate(questionID = row_number()) |> 
  rename(question = questionTables)

answersTable<- main |> 
  select(c(survey_number, date, time, contains(questionTables$question)))

answersLong <- answersTable %>%
  pivot_longer(cols = -c(survey_number, time, date), names_to = "Question", values_to = "Answer") |> 
  left_join(questionTables, by = c("Question" = "question")) |> 
  mutate(Question = questionID) |> 
  select(-questionID) |> 
  rename(surveyNumber = survey_number, questionID = Question, answer = Answer) |> 
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


###########################################################################

demo_data <- sheets_combined$`Demographic Data`

demo_data <- demo_data |> 
  # normalize age class
  mutate(age_class = factor(age_class, levels = c("<16", "16-65", ">65"))) |> 
  # normalize city/province/country text
  mutate(
    city_prov_country = str_to_upper(str_trim(city_prov_country)),
    city_prov_country = str_replace_all(city_prov_country, "^BC$|^BC\\.$", "BC, CANADA"),
    city_prov_country = case_when(
      city_prov_country %in% c("ON", "ONTARIO") ~ "ONTARIO, CANADA",
      city_prov_country %in% c("AB") ~ "ALBERTA, CANADA",
      city_prov_country %in% c("SASK", "SASKATCHEWAN") ~ "SASKATCHEWAN, CANADA",
      city_prov_country %in% c("FL", "FLORIDA") ~ "FLORIDA, USA",
      city_prov_country == "CALIFORNIA" ~ "CALIFORNIA, USA",
      city_prov_country == "UKRAINE" ~ "UKRAINE",
      city_prov_country == "THAILAND" ~ "THAILAND",
      TRUE ~ city_prov_country
    ),
    city_prov_country = case_when(
      city_prov_country %in% c("VANCOUVER", "METRO VANCOUVER", "NORTH VAN", "NORTH VANCOUVER") ~ "VANCOUVER, BC, CANADA",
      city_prov_country == "SURREY" ~ "SURREY, BC, CANADA",
      city_prov_country == "BURNABY" ~ "BURNABY, BC, CANADA",
      city_prov_country == "CHILLIWACK" ~ "CHILLIWACK, BC, CANADA",
      city_prov_country == "LANGLEY" ~ "LANGLEY, BC, CANADA",
      city_prov_country == "ABBOTSFORD" ~ "ABBOTSFORD, BC, CANADA",
      city_prov_country == "WHITE ROCK" ~ "WHITE ROCK, BC, CANADA",
      city_prov_country == "MAPLE RIDGE" ~ "MAPLE RIDGE, BC, CANADA",
      city_prov_country == "CULTUS LAKE" ~ "CULTUS LAKE, BC, CANADA",
      city_prov_country == "MISSION" ~ "MISSION, BC, CANADA",
      city_prov_country == "PORT MOODY" ~ "PORT MOODY, BC, CANADA",
      city_prov_country == "COQUITLAM" ~ "COQUITLAM, BC, CANADA",
      city_prov_country == "VICTORIA" ~ "VICTORIA, BC, CANADA",
      TRUE ~ city_prov_country
    ),
    region_group = case_when(
      str_detect(city_prov_country, "BC, CANADA") ~ "BC",
      str_detect(city_prov_country, "CANADA") ~ "Other Canada",
      TRUE ~ "International"
    ),
    city_prov_country = str_to_title(city_prov_country),
    city_prov_country = str_replace_all(city_prov_country, "Bc", "BC")
  )

# ---------------------------------------
# 2. Normalize and rename for database
# ---------------------------------------
demo <- demo_data |>
  # Normalize column names: lowercase + underscores
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ tolower(.)) |>
  
  # Ensure correct column names
  mutate(survey = coalesce(survey, survey_number)) |>
  select(-any_of("survey_number")) |>
  rename(
    survey_number = survey,
    datetime = date_time,
    gender = gender,
    age_class = age_class,
    license_period = license_period,
    residency = residency,
    city_prov_country = city_prov_country,
    postal_code = postal_code_first_3,
    notes = notes
  ) |>
  mutate(datetime = as.numeric(datetime)) |> 
  # --- Convert Excel numeric datetime to POSIXct ---
  mutate(datetime_parsed = as.Date(as.numeric(datetime), origin = "1899-12-30")) |>
  mutate(time = ymd_hms(as.numeric(datetime), origin = "1899-12-30")) |> 
  # --- Derive date & time ---
  mutate(
    date = as.Date(datetime_parsed),
    time = format(datetime_parsed, "%H:%M:%S")
  ) |>
  
  arrange(date, datetime_parsed) |>
  
  # --- Reset survey_number per date ---
  group_by(date) |>
  mutate(survey_number = row_number()) |>
  ungroup() |>
  
  # --- Final cleaning ---
  mutate(
    angler_id = as.character(row_number()),
    datetime = as.character(datetime_parsed),
    date = as.character(date),
    time = as.character(time)
  ) |>
  select(-datetime_parsed) |>
  mutate(across(everything(), as.character))
# ---------------------------------------
# 3. Upload to SQLite
# ---------------------------------------
get_col_types <- function(df) {
  tibble(
    col_name = names(df),
    sqlite_type = sapply(df, function(x) {
      if (all(grepl("^\\d+$", x[!is.na(x)]))) "INTEGER"
      else if (all(suppressWarnings(!is.na(as.numeric(x[!is.na(x)]))))) "REAL"
      else "TEXT"
    })
  )
}

con <- dbConnect(SQLite(), "creel_survey.sqlite")

col_types <- get_col_types(demo)

col_defs <- col_types |>
  mutate(
    key_status = case_when(col_name == "angler_id" ~ "KEY", TRUE ~ ""),
    sql_def = paste0(col_name, " ", sqlite_type, " ", key_status)
  )

sql <- paste0(
  "CREATE TABLE IF NOT EXISTS creel_fisher_demography (\n",
  paste0(col_defs$sql_def, collapse = ",\n"),
  ",\nPRIMARY KEY (angler_id)\n)"
)

dbExecute(con, sql)

dbWriteTable(conn = con, name = "creel_fisher_demography", value = demo, append = TRUE, row.names = FALSE)

# Verify
dbListTables(con)
dbListFields(con, "creel_fisher_demography")


###################################################################

fish <- sheets_combined$`Fish Data` %>%
  mutate(temp = coalesce(pit_tag_number, p_it_tag_number)) %>%
  # Remove the old columns
  select(-pit_tag_number, -p_it_tag_number) %>%
  # Rename the temp column to the standardized name
  rename(pit_tag_number = temp) |>
  filter(!is.na(date_time)) %>%
  mutate(
    date = as.Date(as.numeric(date_time), origin = "1899-12-30"),
    time = hms::as_hms(as.numeric(date_time) %% 1 * 86400)
  ) %>%
  select(-date_time) |> 
  mutate(spp = case_when(spp %in% c("Lrg Scl Suckr", "Lrg Scale Suckr") ~ "Large Scale Sucker",
                         spp %in% c("Pumpkin sedd", "PS") ~ "Pumpkinseed",
                         spp %in% c("RT") ~ "Rainbow Trout",
                         spp %in% c("SMB") ~ "Smallmouth Bass",
                         TRUE ~ spp)) |> 
  mutate(
    length_mm = as.numeric(length_mm),
    weight_g = as.numeric(weight_g)
  )

###################################################################################

ICE<- sheets_combined$ICE

ICE |>
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) |>
  mutate(
    air_temperature = coalesce(air_temp, air_temperature),
    precipitation = coalesce(precip, precipitation)
  ) |>
  select(-air_temp, -air_temperature, -precip, -precipitation) |>
  mutate(across(everything(), as.character))



