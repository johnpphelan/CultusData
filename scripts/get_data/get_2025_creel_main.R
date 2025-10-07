# Load libraries
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(hms)
library(purrr)

# Optional: source utility functions
source("scripts/utils/fix_col_names_f.R")

# Define file path
lan_folder <- "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"
creel_folder <- file.path(lan_folder, "2025 projects/Creel Surveys/Archived Creel Forms/")

# List Excel files
the_files <- list.files(
  path = creel_folder,
  pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE
)

# Remove temp files
the_files <- the_files[!grepl("~$", the_files)]

main_data_list <- map(the_files, function(file) {
  if ("Main Data" %in% excel_sheets(file)) {
    dat_text <- read_excel(file, sheet = "Main Data", col_types = "text")
    dat_guess <- read_excel(file, sheet = "Main Data")
    
    # Clean column names
    dat_text <- janitor::clean_names(dat_text)
    dat_guess <- janitor::clean_names(dat_guess)
    
    # Manually fix unnamed second column if needed
    if (length(names(dat_text)) > 1 && names(dat_text)[2] == "...2") {
      names(dat_text)[2] <- "surveyor"
    }
    
    # Replace surveyor if available
    if ("surveyor" %in% names(dat_guess)) {
      dat_text$surveyor <- dat_guess$surveyor
    }
    
    return(dat_text)
  } else {
    return(NULL)
  }
})


# Remove NULLs and bind rows
main_data_list <- compact(main_data_list)
main <- bind_rows(main_data_list)

# Optional: check for Andre Riebe
main |> filter(str_detect(surveyor, "Andre Riebe"))

# Save the cleaned main data
saveRDS(main, "output/2025_main_data.rds")
