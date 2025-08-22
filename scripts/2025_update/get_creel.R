library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
library(purrr)
library(janitor)

source("scripts/utils/fix_col_names_f.R")

lan_folder = "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2025 projects/Creel Surveys/"

the_files = c(list.files(c(
  # No raw files in 2023, we could add these in later
  #"//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2023 projects/Creel Surveys/Archived Creel Forms/",
  "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2024 projects/Creel Surveys/Creel Survey forms 2024 (backup)/",
  "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2025 projects/Creel Surveys/Archived Creel Forms/"
),
pattern = "*.xlsx", recursive = T, full.names = TRUE),
"//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2023 projects/Creel Surveys/2023 SMB creel_final not edits.xlsx"
)

the_files <- the_files[!grepl("~$", the_files)] # Remove temporary files

sheet_names = readxl::excel_sheets(the_files[1])

files_read_l <- the_files |> 
  #looping over the file names
  lapply(
    \(x) { # for each file
      # get the sheet name
      all_sheets <- readxl::excel_sheets(x)
      all_sheets |> # for each sheet
        purrr::map(~ { # The tilde means "for each .x (sheet name)"
          # for the file x, read the sheet .x
          dat <- readxl::read_excel(x, sheet = .x, col_types = 'text') 
          cat(paste0("File processed: ", x,", Sheet:", .x, "\n"))
          # Fix specific column names before cleaning names
          names(dat)[names(dat) == "# SMB c...15"] <- "# SMB c"
          names(dat)[names(dat) == "# SMB c...23"] <- "# SMB r"
          
          # Optional: clean remaining names (if you want lower_case formatting)
          dat <- janitor::clean_names(dat)
          
          return(dat)
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


# combine the columns by name

main <- main |> 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) |> 
  arrange(date) |> 
  group_by(as.character(date)) |> 
  mutate(survey_number = row_number()) |>  
  ungroup() |> 
  select(-`as.character(date)`) |> 
  mutate(
    date = as.character(date),
    time = hms::as_hms(as.numeric(time) %% 1 * 86400)
  ) |>
  group_by(time) |> 
  mutate(
    time = if_else(
      row_number() > 1,
      hms::as_hms(as.numeric(time) + 5 * 60 * (row_number() - 1)),
      time
    )
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
    hours_worked = as.numeric(difftime(shift_end_time, shift_start_time, units = "hours")),
    shift_start_time = paste(date, shift_start_time),
    shift_end_time = paste(date, shift_end_time)
  )

main <- main |> 
  relocate(shift_end:hours_worked, .before = everything())

#fix wind data
main<- main |>
  mutate(
    wind = case_when(
      suppressWarnings(!is.na(as.numeric(wind))) ~ {
        # Convert to Date, then use lubridate for flexible formatting
        date_val <- as.Date(as.numeric(wind), origin = "1899-12-30")
        paste0(month(date_val), "-", day(date_val))
      },
      TRUE ~ wind
    )
  )

main <- main |>
  select(
    survey_number:total_fish_caught,
    number_smb_c:number_other_spp_c,
    number_pikeminnow_caught,
    total_retained,
    number_smb_r:number_other_spp_r,
    number_pikeminnow_r,
    everything()
  )


main<-main |> 
  dplyr::mutate(air_temp = coalesce(air_temp, air_temperature, mean_air_temperature),
                surveyor = coalesce(surveyor, x2),
                wind = coalesce(wind, x44),
  ) |> 
  dplyr::select(-c(air_temperature, x2, x44))

main_page<-main[,1:35]

main_page <- main_page |> 
  rename(fishing_method = vessel,
         number_pikeminnow_c = number_pikeminnow_caught)

main_page <- main_page |> 
  filter(!is.na(date))

main_page<- main_page |> 
  relocate(shift_end:hours_worked, .after = everything())





sum(as.numeric(main_page$total_fish_caught), na.rm = T) 
counted <- main_page |>
  mutate(year = year(as.Date(date))) |>  # ensure date is a proper Date object
  group_by(year) |> 
  summarise(across(
    .cols = c(matches("^number.*[cr]$"), any_of(c("total_fish_caught", "total_fish_retained"))),
    .fns = ~ sum(as.numeric(.x), na.rm = TRUE),
    .names = "sum_{.col}"
  )) |> 
  as_tibble()

# write.csv(counted, file = "output/counts_survey2024.csv", row.names = FALSE)

sum_numbers_in_text <- function(x) {
  nums <- str_extract_all(x, "\\d+")         # extract all numbers
  sapply(nums, function(n) sum(as.numeric(n)))  # sum per element
}

main_t_totals_long <- counted |>
  pivot_longer(
    cols = -year,                
    names_to = "metric",
    values_to = "total"
  )

ggplot(main_t_totals_long, aes(x = metric, y = total, fill = factor(year))) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    x = "Metric",
    y = "Total Count",
    fill = "Year",
    title = "Total Counts by Metric and Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Get all columns that end in 'r' (for retained fish)
retained_cols <- grep("^number.*_r$", names(main_page), value = TRUE)

# Convert relevant columns to numeric and calculate sums
retained_component_total <- main_page |>
  summarise(across(all_of(retained_cols), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
  rowSums()

reported_total_retained <- sum(as.numeric(main_page$total_retained), na.rm = TRUE)

# Compare
retained_component_total
reported_total_retained
difference <- retained_component_total - reported_total_retained
difference

# Get all columns that end in '_c' (for caught fish)
caught_cols <- grep("^number.*_c$", names(main_page), value = TRUE)

# Convert relevant columns to numeric and calculate sums
caught_component_total <- main_page |>
  summarise(across(all_of(caught_cols), ~ sum(as.numeric(.x), na.rm = TRUE))) |>
  rowSums()

reported_total_caught <- sum(as.numeric(main_page$total_fish_caught), na.rm = TRUE)

# Compare
caught_component_total
reported_total_caught
difference_caught <- caught_component_total - reported_total_caught
difference_caught

# write.csv(main_t_totals, file = "output/counts_survey2024.csv", row.names = FALSE)



### 2. Questions and Answers

questionTables<-main|> 
  colnames()

questionTables <- questionTables |> 
  as.data.frame() |> 
  slice(38:54) |> 
  mutate(questionID = row_number()) |> 
  rename(question = questionTables)


answersTable<- main |> 
  select(c(survey_number, date, time, any_of(questionTables$question)))

answersLong <- answersTable |> 
  pivot_longer(cols = -c(survey_number, time, date), names_to = "Question", values_to = "Answer") |> 
  left_join(questionTables, by = c("Question" = "question")) |> 
  mutate(Question = questionID) |> 
  select(-questionID) |> 
  rename( surveyNumber = survey_number, date = date, time = time, questionID = Question, answer = Answer) |> 
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


### 3 . Demography

demo<- sheets_combined$`Demographic Data`
demo<-demo |> 
  mutate(date = as.Date(as.numeric(date_time), origin = "1899-12-30")) |> 
  arrange(date) |> 
  group_by(as.character(date)) |> 
  mutate(survey_number = row_number()) |> 
  ungroup() |> 
  select(-`as.character(date)`) |> 
  mutate(
    date = as.character(date),
    time = hms::as_hms(as.numeric(date_time) %% 1 * 86400)
  ) |>
  group_by(time) |> 
  mutate(
    time = if_else(
      row_number() > 1,
      hms::as_hms(as.numeric(time) + 5 * 60 * (row_number() - 1)),
      time
    )
  ) |> 
  ungroup() |> 
  mutate(time = as.character(time)) |> 
  select(-c(date_time, survey)) |> 
  relocate(date, time, .after = 1)
