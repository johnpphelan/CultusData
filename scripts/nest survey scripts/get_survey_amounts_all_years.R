library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


data_2023 <- read.xlsx(paste0(lan_folder,"2023 projects/nest surveys/2023 nest survey data raw.xlsx"), sheet = "raw data") |> 
  janitor::clean_names()
data_2024 <- read.xlsx(paste0(lan_folder,"2024 projects/Nest Surveys/2024_Nest_Survey_Data_Compiled-Nicole_Kaminski.xlsx"), sheet = "SMB Nests") |> 
  janitor::clean_names()
data_2025 <- read.xlsx(paste0(lan_folder,"2025 projects/Nest Destruction/2025 Cultus Lake SMB Nest Destruction Surveys.xlsx"), sheet = "Survey Data") |> 
  janitor::clean_names()



data_2023 = data_2023 |> 
  dplyr::rename(northing = northinh, male_presence = presence_of_guarding_male_y_n,
                life_stage_on_nest = life_stage_on_nest_egg_alevin_or_fry,
                adjacent_structure = adjacent_structure_e_g_dock_or_mouring_buoy_na,
                activity_completed = activity_completed_observation_vs_nest_destruction,
                full_destruction = nest_fully_destroyed_y_n_partially) |> 
  dplyr::mutate(
    date = as.Date(date, origin = "1899-12-30"),
    approximate_diameter_of_nest_m = stringr::str_extract(approximate_diameter_of_nest_m, "^[^-]+"),
    life_stage_on_nest = dplyr::case_when(
      life_stage_on_nest == "alvein" ~ "alevin",
      TRUE ~ life_stage_on_nest
    ),
    survey_number = dplyr::row_number()
  )

data_2023 = data_2023 |>
  dplyr::mutate(
    time_in = dplyr::if_else(
      is.na(time_in),
      NA_character_,
      format(hms::as_hms(time_in * 86400), "%H:%M")
    ),
    time_out = dplyr::if_else(
      is.na(time_out),
      NA_character_,
      format(hms::as_hms(time_out * 86400), "%H:%M")
    )
  )
data_2023 = data_2023 |>
  dplyr::mutate(
    time_in  = format(hms::as_hms(time_in),  "%H:%M"),
    time_out = format(hms::as_hms(time_out), "%H:%M")
  )

data_2024 = data_2024 |> 
  dplyr::rename(approximate_diameter_of_nest_m = depth, male_presence = gaurding_male,
                life_stage_on_nest = life_stage, adjacent_structure = adj_structure,
                activity_completed = nests_destroyed, comments = x18) |> 
  dplyr::select(-x20) |> 
  mutate(date = as.Date(date, origin = "1899-12-30"))

data_2024 = data_2024 |>
  dplyr::mutate(
    time_in = dplyr::if_else(
      is.na(time_in),
      NA_character_,
      format(hms::as_hms(time_in * 86400), "%H:%M")
    ),
    time_out = dplyr::if_else(
      is.na(time_out),
      NA_character_,
      format(hms::as_hms(time_out * 86400), "%H:%M")
    )
  )
data_2024 = data_2024 |>
  dplyr::mutate(
    time_in  = format(hms::as_hms(time_in),  "%H:%M"),
    time_out = format(hms::as_hms(time_out), "%H:%M")
  )



  

data_2025 = data_2025 |> 
  rename(location = location_name, male_presence = guarding_male_y_n,
         activity_completed = activity_completed_by_surveyor,
         full_destruction = nest_fully_destroyed, ais = other_invasive_species_present) |> 
  mutate(date = as.Date(date, origin = "1899-12-30"),
         start_time = as.POSIXct(start_time * 86400, origin = "1899-12-30", tz = "UTC"))

data_2025 = data_2025 |> 
  rename(time_in = start_time, time_out = end_time)

data_2025 = data_2025 |>
  dplyr::mutate(
    
    # Fix time_in (POSIXct with Excel origin)
    time_in = dplyr::if_else(
      is.na(time_in),
      NA_character_,
      format(as.POSIXct(time_in), "%H:%M")
    ),
    
    # Fix time_out (Excel fractional day)
    time_out = dplyr::if_else(
      is.na(time_out),
      NA_character_,
      format(hms::as_hms(time_out * 86400), "%H:%M")
    )
    
  )
data_2025 = data_2025 |>
  dplyr::mutate(
    time_out = format(hms::as_hms(time_out), "%H:%M")
  )

data_2023 |> 
  group_by(date) |> 
  summarise(n())

data_2024 |> 
  group_by(date) |> 
  summarise(n())

data_2025 |> 
  group_by(date) |> 
  summarise(n())

dplyr::n_distinct(data_2023$date)
dplyr::n_distinct(data_2024$date)
dplyr::n_distinct(data_2025$date)


data_2023 = data_2023 |> 
  mutate(full_destruction = case_when(
    full_destruction == "Y" ~ "Yes",
    full_destruction == "N" ~ "No",
    full_destruction == "P" ~ "Partial",
    full_destruction == "partial" ~ "Partial",
    full_destruction == "yes" ~ "Yes",
    full_destruction == "yes " ~ "Yes",
    full_destruction == "no" ~ "No",
    full_destruction == "prev." ~ "Previous Nest",
    full_destruction == "N (prev Y)" ~ "Previous Nest",
    full_destruction == "n/a" ~ NA,
    TRUE ~ full_destruction
  ))

data_2024= data_2024 |> 
  rename(full_destruction = activity_completed) |> 
  mutate(full_destruction = case_when(
    full_destruction == "Y" ~ "Yes",
    full_destruction == "N" ~ "No",
    full_destruction == "P" ~ "Partial",
    full_destruction == "partial" ~ "Partial",
    full_destruction == "yes" ~ "Yes",
    full_destruction == "yes " ~ "Yes",
    full_destruction == "no" ~ "No",
    full_destruction == "prev." ~ "Previous Nest",
    full_destruction == "N (prev Y)" ~ "Previous Nest",
    full_destruction == "n/a" ~ NA,
    TRUE ~ full_destruction
  ))

data_2025 = data_2025 |> 
  mutate(full_destruction = case_when(
    full_destruction == "Y" ~ "Yes",
    full_destruction == "N" ~ "No",
    full_destruction == "P" ~ "Partial",
    full_destruction == "partial" ~ "Partial",
    full_destruction == "yes" ~ "Yes",
    full_destruction == "yes " ~ "Yes",
    full_destruction == "no" ~ "No",
    full_destruction == "prev." ~ "Previous Nest",
    full_destruction == "N (prev Y)" ~ "Previous Nest",
    full_destruction == "n/a" ~ NA,
    TRUE ~ full_destruction
  ))

effort_summary_2023 = data_2023 |>
  dplyr::filter(!is.na(time_in), !is.na(time_out)) |>
  dplyr::group_by(
    date,
    time_in,
    time_out,
    full_destruction
  ) |>
  dplyr::summarise(
    n_entries = dplyr::n(),
    .groups = "drop"
  )

effort_summary_2024 = data_2024 |>
  dplyr::filter(!is.na(time_in), !is.na(time_out)) |>
  dplyr::group_by(
    date,
    time_in,
    time_out,
    full_destruction
  ) |>
  dplyr::summarise(
    n_entries = dplyr::n(),
    .groups = "drop"
  )

effort_summary_2025 = data_2025 |>
  dplyr::filter(!is.na(time_in), !is.na(time_out)) |>
  dplyr::group_by(
    date,
    time_in,
    time_out,
    full_destruction
  ) |>
  dplyr::summarise(
    n_entries = dplyr::n(),
    .groups = "drop"
  )


effort_total = bind_rows(effort_summary_2023, effort_summary_2024, effort_summary_2025)

# effort_total = effort_total |> 
#   mutate(full_destruction = case_when(
#     full_destruction == "Y" ~ "Yes",
#     full_destruction == "N" ~ "No",
#     full_destruction == "P" ~ "Partial",
#     full_destruction == "partial" ~ "Partial",
#     full_destruction == "yes" ~ "Yes",
#     full_destruction == "yes " ~ "Yes",
#     full_destruction == "no" ~ "No",
#     full_destruction == "prev." ~ "Previous Nest",
#     full_destruction == "N (prev Y)" ~ "Previous Nest",
#     full_destruction == "n/a" ~ NA,
#     TRUE ~ full_destruction
#   ))

write.csv(effort_total, "./output/total_effort_nest_destruction.csv")
