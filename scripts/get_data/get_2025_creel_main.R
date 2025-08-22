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
source("scripts/utils/fix_col_names_f.R")
# Define file paths
lan_folder <- "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


the_files <- list.files(c(
  "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2025 projects/Creel Surveys/Archived Creel Forms/"),
pattern = "*.xlsx", recursive = TRUE, full.names = TRUE)


the_files <- the_files[!grepl("~$", the_files)] # Remove temp files

sheet_names <- readxl::excel_sheets(the_files[1])

files_read_l <- the_files |> 
  lapply(function(x) {
    all_sheets <- readxl::excel_sheets(x)
    all_sheets |> 
      purrr::map(~ {
        # read everything as text
        dat_text <- readxl::read_excel(x, sheet = .x, col_types = "text")
        
        # read again with guessing
        dat_guess <- readxl::read_excel(x, sheet = .x)
        
        # if surveyor exists in the guessed version, replace it
        if ("Surveyor" %in% names(dat_guess)) {
          dat_text$Surveyor <- dat_guess$Surveyor
        }
        
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
    sheet_data <- files_read_f[which(names(files_read_f) == .x)] |>
      map(function(df) {
        if ("surveyor" %in% names(df)) {
          df$surveyor <- as.character(df$surveyor)
        }
        df
      })
    bind_rows(sheet_data)
  })

names(sheets_combined) <- sheet_names


main <- sheets_combined$`Main Data`


main |>
  filter(!is.na(date)) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  filter(duplicated(select(., survey_number, date, time)))


main <- main |> 
  filter(!is.na(date)) |> 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) |> 
  group_by(survey_number, date, time) |>
  fill(everything(), .direction = "downup") |>  # fill missing values in group
  slice_head(n = 1) |>                          # keep just the first row
  ungroup() |> 
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


saveRDS(main, "output/2025_main_data.rds")


