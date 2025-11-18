# clean_creel_excel.R
# ---------------------------------------------
# Purpose: Read raw Creel Survey Excel files, clean, fix column names, dates, times, and save as new Excel file
# ---------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(hms)
library(readxl)
library(janitor)
library(purrr)
library(writexl)
library(stringr)

# -----------------------------
# Configuration
# -----------------------------
lan_folder <- "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

# Gather all relevant files
the_files <- c(
  list.files(file.path(lan_folder, "2024 projects/Creel Surveys/Creel Survey forms 2024 (backup)/"), 
             pattern = "*.xlsx", recursive = TRUE, full.names = TRUE),
  list.files(file.path(lan_folder, "2025 projects/Creel Surveys/Archived Creel Forms/"), 
             pattern = "*.xlsx", recursive = TRUE, full.names = TRUE),
  file.path(lan_folder, "2023 projects/Creel Surveys/2023 SMB creel_final not edits.xlsx")
)

the_files <- the_files[!grepl("~$", the_files)] # Remove temporary files

# -----------------------------
# Read all sheets with proper renaming
# -----------------------------
files_read_l <- lapply(the_files, function(x) {
  all_sheets <- excel_sheets(x)
  setNames(lapply(all_sheets, function(sheet_name) {
    # read everything as text to preserve formatting
    dat_text <- read_excel(x, sheet = sheet_name, col_types = "text")
    
    # Rename known special columns from Excel quirks
    # These come from your original workflow
    names(dat_text)[names(dat_text) == "# SMB c...15"] <- "# SMB c"
    names(dat_text)[names(dat_text) == "# SMB c...23"] <- "# SMB r"
    
    # Clean names to snake_case
    dat_text <- janitor::clean_names(dat_text)
    
    return(dat_text)
  }), all_sheets)
})

# Flatten list of lists into single named list
files_read_f <- purrr::flatten(files_read_l)

# Get unique sheet names across all files
sheet_names <- unique(names(files_read_f))

# Combine sheets with the same name across files
sheets_combined <- setNames(
  lapply(sheet_names, function(sn) {
    bind_rows(files_read_f[names(files_read_f) == sn])
  }),
  sheet_names
)

# -----------------------------
# Main Data Cleaning
# -----------------------------
main_page <- sheets_combined$`Main Data` %>%
  mutate(
    date = as.Date(as.numeric(date), origin = "1899-12-30"),
    time = hms::as_hms(as.numeric(time) %% 1 * 86400),
    air_temperature = coalesce(air_temp, air_temperature, mean_air_temperature),
    wind = coalesce(wind, x44)
  ) %>%
  arrange(date) |> 
  select(-air_temp, -x44, -mean_air_temperature) %>%
  mutate(
    surveyor = ifelse(surveyor == "" | is.na(surveyor), NA, surveyor)
  ) %>%
  tidyr::fill(surveyor, .direction = "down") %>%
  filter(!is.na(date)) %>%                  # ✅ keep only rows with a valid date
  distinct() %>%
  arrange(date, time)

# -----------------------------
# Demographic Data Cleaning
# -----------------------------
# =====================================================
# DEMOGRAPHIC DATA CLEANING AND STANDARDIZATION
# =====================================================

demo_data <- sheets_combined$`Demographic Data`

# --- Optional: quick visualization of age classes ---
# age_summary <- demo_data %>%
#   count(age_class) %>%
#   mutate(age_class = factor(age_class, levels = c("<16", "16-65", ">65"))) %>%
#   filter(!is.na(age_class))

# ggplot(age_summary, aes(x = age_class, y = n, fill = age_class)) +
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   labs(
#     x = "Age Class",
#     y = "Count",
#     title = "Counts of Individuals by Age Class"
#   ) +
#   theme(legend.position = "none")

# =====================================================
# MAIN DEMO CLEANING
# =====================================================

demo_data <- demo_data %>%
  mutate(
    # Convert Excel serial datetime to proper POSIXct (PST)
    time_num = as.numeric(date_time),
    datetime = as.POSIXct(time_num * 86400, origin = "1899-12-30", tz = "UTC"),
    datetime = lubridate::with_tz(datetime, tzone = "America/Los_Angeles"),
    
    # Extract date and time
    date = as.Date(datetime),
    time = format(datetime, "%H:%M:%S")
  ) %>%
  # keep unique date-time rows only
  distinct(date, time, .keep_all = TRUE) %>%
  # drop helper columns
  select(-datetime, -time_num, -date_time) %>%
  # ensure we only keep rows with a valid date
  filter(!is.na(date)) %>%
  # order by date (then time if available)
  arrange(date, time) %>%
  
  # =====================================================
# ADDRESS / LOCATION STANDARDIZATION
# =====================================================
mutate(
  city_prov_country = str_to_upper(str_trim(city_prov_country)),
  
  # fix minor typos
  city_prov_country = str_replace_all(city_prov_country, "^BC$|^BC\\.$", "BC, CANADA"),
  
  # province normalization
  city_prov_country = case_when(
    city_prov_country %in% c("ON", "ONTARIO") ~ "ONTARIO, CANADA",
    city_prov_country %in% c("AB") ~ "ALBERTA, CANADA",
    city_prov_country %in% c("SASK", "SASKATCHEWAN") ~ "SASKATCHEWAN, CANADA",
    city_prov_country %in% c("FL", "FLORIDA") ~ "FLORIDA, USA",
    city_prov_country %in% c("CALIFORNIA") ~ "CALIFORNIA, USA",
    city_prov_country %in% c("UKRAINE") ~ "UKRAINE",
    city_prov_country %in% c("THAILAND") ~ "THAILAND",
    TRUE ~ city_prov_country
  ),
  
  # common BC cities
  city_prov_country = case_when(
    city_prov_country %in% c("VANCOUVER", "METRO VANCOUVER", "NORTH VANCOUVER") ~ "VANCOUVER, BC, CANADA",
    city_prov_country == "SURREY" ~ "SURREY, BC, CANADA",
    city_prov_country == "BURNABY" ~ "BURNABY, BC, CANADA",
    city_prov_country == "CHILLIWACK" ~ "CHILLIWACK, BC, CANADA",
    city_prov_country == "LANGLEY" ~ "LANGLEY, BC, CANADA",
    city_prov_country == "ABBOTSFORD" ~ "ABBOTSFORD, BC, CANADA",
    city_prov_country == "WHITE ROCK" ~ "WHITE ROCK, BC, CANADA",
    city_prov_country == "MAPLE RIDGE" ~ "MAPLE RIDGE, BC, CANADA",
    city_prov_country == "YARROW" ~ "YARROW, BC, CANADA",
    city_prov_country == "CULTUS" ~ "CULTUS, BC, CANADA",
    city_prov_country == "ROSEDALE" ~ "ROSEDALE, BC, CANADA",
    city_prov_country == "COQUITLAM" ~ "COQUITLAM, BC, CANADA",
    city_prov_country == "EDMONTON" ~ "EDMONTON, AB, CANADA",
    city_prov_country == "VANCOUVER ISLAND" ~ "VANCOUVER ISLAND, BC, CANADA",
    city_prov_country == "NORTH VAN" ~ "VANCOUVER, BC, CANADA",
    city_prov_country == "CULTUS LAKE" ~ "CULTUS LAKE, BC, CANADA",
    city_prov_country == "VICTORIA" ~ "VICTORIA, BC, CANADA",
    city_prov_country == "RICHMOND" ~ "RICHMOND, BC, CANADA",
    city_prov_country == "NEW WESTMINSTER" ~ "NEW WESTMINSTER, BC, CANADA",
    city_prov_country == "SLOCAN" ~ "SLOCAN, BC, CANADA",
    city_prov_country == "DELTA" ~ "DELTA, BC, CANADA",
    city_prov_country == "ALDERGROVE" ~ "ALDERGROVE, BC, CANADA",
    city_prov_country == "WALNUT GROVE" ~ "WALNUT GROVE, BC, CANADA",
    city_prov_country == "US, NORTH CAROLINA" ~ "NORTH CAROLINA, USA",
    city_prov_country == "ST ALBERT, ALBERTA" ~ "ST ALBERT, AB, CANADA",
    city_prov_country == "SANTA BARBARA, CALIFORNIA" ~ "SANTA BARBARA, CA, USA",
    city_prov_country == "PORTMOODY" ~ "PORT MOODY, BC, CANADA",
    city_prov_country == "NEWWESTMINSTER" ~ "NEW WESTMINSTER, BC, CANADA",
    city_prov_country == "MISSION" ~ "MISSION, BC, CANADA",
    city_prov_country == "KAMLOOPS" ~ "KAMLOOPS, BC, CANADA",
    city_prov_country == "CLOVERDALE" ~ "SURREY, BC, CANADA",
    city_prov_country == "CACHE CREEK" ~ "CACHE CREEK, BC, CANADA",
    city_prov_country == "PORT MOODY" ~ "PORT MOODY, BC, CANADA",
    city_prov_country == "PROMONTORY" ~ "PROMONTORY, BC, CANADA",
    TRUE ~ city_prov_country
  ),
  
  # --- Region grouping ---
  region_group = case_when(
    str_detect(city_prov_country, "BC, CANADA") ~ "BC",
    str_detect(city_prov_country, "CANADA") ~ "Other Canada",
    TRUE ~ "International"
  )
) %>%
  distinct()



# -----------------------------
# Fish Data Cleaning
# -----------------------------
fish <- sheets_combined$`Fish Data` %>%
  mutate(
    temp = coalesce(pit_tag_number, p_it_tag_number)
  ) %>%
  select(-pit_tag_number, -p_it_tag_number) %>%
  rename(pit_tag_number = temp) %>%
  filter(!is.na(date_time)) %>%
  mutate(
    date = as.Date(as.numeric(date_time), origin = "1899-12-30"),
    time = hms::as_hms(as.numeric(date_time) %% 1 * 86400)
  ) %>%
  select(-date_time) %>%
  distinct() %>%
  mutate(spp = case_when(
    spp %in% c("Lrg Scl Suckr", "Lrg Scale Suckr") ~ "Large Scale Sucker",
    spp %in% c("Pumpkin sedd", "PS") ~ "Pumpkinseed",
    spp %in% c("RT") ~ "Rainbow Trout",
    spp %in% c("SMB") ~ "Smallmouth Bass",
    TRUE ~ spp
  ))

# -----------------------------
# ICE Data Cleaning
# -----------------------------
ICE <- sheets_combined$ICE %>%
  mutate(
    time_num = as.numeric(time),
    datetime = as.POSIXct(time_num * 86400, origin = "1899-12-30", tz = "UTC"),
    date = as.Date(datetime),
    time = format(datetime, "%H:%M:%S")
  ) %>%
  distinct(date, time, .keep_all = TRUE) %>%
  select(-datetime, -time_num) %>%
  mutate(
    air_temperature = coalesce(air_temp, air_temperature),
    precipitation = coalesce(precip, precipitation)
  ) %>%
  select(-air_temp, -precip) %>%
  filter(!is.na(date)) %>%                  # ✅ keep only rows with a valid date
  mutate(across(everything(), as.character)) %>%
  arrange(date, time)


# Function to fix survey_number per day
fix_survey_number <- function(df) {
  if (!"date" %in% names(df)) return(df)  # skip sheets without a date
  df %>%
    arrange(date, dplyr::across(any_of("date"))) %>%  # sort by date and time if exists
    group_by(date) %>%
    mutate(survey_number = row_number()) %>%          # restart survey_number each date
    ungroup()
}

main_page <- main_page |> 
  # Keep only unique combinations of date and time
  distinct(date, time, .keep_all = TRUE)

main_page<- main_page |> 
  filter(!is.na(time))
demo_data<- demo_data |> 
  filter(!is.na(time))
fish<- fish |> 
  filter(!is.na(time))
ICE<- ICE |> 
  filter(!is.na(time))

main_page$date<-as.Date(main_page$date)
demo_data$date<-as.Date(demo_data$date)
fish$date<-as.Date(fish$date)
ICE$date<-as.Date(ICE$date)



main_page<-fix_survey_number(main_page)
demo_data<-fix_survey_number(demo_data)
fish<-fix_survey_number(fish)
ICE<-fix_survey_number(ICE)

  
# -----------------------------
# Save cleaned sheets back to Excel
# -----------------------------
output_file <- file.path( "./output/Creel_Survey_Cleaned.xlsx")

sheets_to_save <- list(
  "Main Data" = main_page,
  "Demographic Data" = demo_data,
  "Fish Data" = fish,
  "ICE" = ICE
)



write_xlsx(sheets_to_save, path = output_file)
message("✅ Cleaned data saved to: ", output_file)
