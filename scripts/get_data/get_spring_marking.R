library(data.table)
library(readxl)
library(dplyr)
library(stringr)
source("scripts/utils/fix_col_names_f.R")



lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


spring_marking<-read_excel(path = paste0(lan_folder,"2023 projects/spring marking/2023_SMB spring marking_data raw-DFO (verified).xlsx"),
                         sheet = 1, col_names = T, progress = readxl_progress())
spring_marking<-names_fix(spring_marking)

spring_marking<- spring_marking |> 
  mutate(date = as.Date(date), GPS_Point = as.character(GPS_Point), Start_Time = as.character(Start_Time),
         End_Time = as.character(End_Time), Seconds_Electricity = as.numeric(Seconds_Electricity),
         Pit_tag = as.numeric(Pit_tag), Acoustic_Tag = as.numeric(Acoustic_Tag), length = as.numeric(length),
         weight = as.numeric(weight), sex = as.character(sex), Maturity = as.character(Maturity), 
         Method = as.character(Method), Recap = as.character(Recap), Mort = as.character(Mort),
         Scale_Book_No = as.numeric(Scale_Book_No), Scale_Nos = as.character(Scale_Nos),
         Comments = as.character(Comments))
