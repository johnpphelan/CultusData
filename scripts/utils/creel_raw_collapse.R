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

sum_numbers_in_text <- function(x) {
  nums <- str_extract_all(x, "\\d+")         # extract all numbers
  sapply(nums, function(n) sum(as.numeric(n)))  # sum per element
}


lan_folder = "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2024 projects/Creel Surveys/"


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
# db_filepath = "output/CultusData.sqlite"
# con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)
# 
# DBI::dbListTables(con)

the_files = list.files("//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/2024 projects/Creel Surveys/Creel Survey forms 2024 (backup)/",
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


# now we flatten
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

main_t<-main[,1:32]
names(main_t)[names(main_t) == "# SMB c...15"]<- "# SMB c"
names(main_t)[names(main_t) == "# SMB c...23"]<- "# SMB r"

namesFix<-names(main_t)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(main_t)<-namesFix

main_t<-main_t |> 
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
    noSMBc = No_SMB_c,
    noKOc = No_KO_c,
    noCTc = No_CT_c,
    noRBc = No_RB_c,
    noBTc = No_BT_c,
    noLTc = No_LT_c,
    noOtherSppC = No_Other_Spp_c,
    totalRetained = Total_Retained,
    noSMBr = No_SMB_r,
    noKOr = No_KO_r,
    noCTr = No_CT_r,
    noRBr = No_RB_r,
    noBTr = No_BT_r,
    noLTr = No_LT_r,
    noOtherSppR = No_Other_Spp_r
  ) |>
  dplyr::mutate(across(everything(), as.character))

main_t<-main_t |> 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) |> 
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


main_t<-main_t |> 
  filter(as.Date(date) <"2025-01-01")

# Apply to main_t
main_t_totals <- main_t |>
  mutate(
    noOtherSppC_num = sum_numbers_in_text(noOtherSppC),
    noOtherSppR_num = sum_numbers_in_text(noOtherSppR)
  ) |>
  summarise(across(
    .cols = c(matches("^no.*[cr]$"), any_of(c("totalFishCaught", "totalRetained", "noOtherSppC_num", "noOtherSppR_num"))),
    .fns = ~ sum(as.numeric(.x), na.rm = TRUE)
  ))

# Optional: tidy view
main_t_totals_long <- main_t_totals |>
  pivot_longer(everything(), names_to = "metric", values_to = "total")

write.csv(main_t_totals, file = "output/counts_survey2024.csv", row.names = FALSE)
