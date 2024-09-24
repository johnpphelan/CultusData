library(data.table)
library(readxl)
library(dplyr)
library(stringr)
source("scripts/utils/fix_col_names_f.R")



lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


angling_data<-read_excel(path = paste0(lan_folder,"2023 projects/recapture_ angling-Basok/2023 angling recapture data - Basok.xlsx"),
                            sheet = 1, col_names = T, progress = readxl_progress())
angling_data<-names_fix(angling_data)

angling_data <- angling_data |> 
  mutate(Survey_Date = as.Date(Survey_Date), Surveyor_Name = as.character(Surveyor_Name),
         Time = as.character(Time), Weather = as.character(Weather), Location = as.character(Location),
         Fish_No = as.numeric(Fish_No), Length_mm = as.numeric(Length_mm), Weight_g = as.numeric(Weight_g),
         PIT_tag_No = as.numeric(PIT_tag_No), Acoustic_tag_No = as.numeric(Acoustic_tag_No), comments = as.character(comments))

