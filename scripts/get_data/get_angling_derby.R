library(data.table)
library(readxl)
library(xlsx)
library(dplyr)
library(stringr)
library(chron)
source("scripts/utils/fix_col_names_f.R")



lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


angling_derby_data <- read_excel(path = paste0(lan_folder,"2023 projects/recapture_Cultus Lake Derby/2023_Cultus Lake Derby data.xlsx"),
                                 sheet = 1, col_names = T, progress = readxl_progress()) |> 
  rename_with(~str_replace_all(.x, "\\s", "_")) |> 
  mutate(start_time = as.numeric(start_time),
         end_time = as.numeric(end_time))


angling_derby_data<-names_fix(angling_derby_data)

angling_derby_data<- angling_derby_data |> 
  mutate(date_start_time = (start_time*24*3600),
         date_end_time = (end_time*24*3600),
         date = as.POSIXlt(date),
          date_st_tm = as.POSIXlt(  date + date_start_time),
         date_end_tm = as.POSIXlt(  date + date_end_time))  |> 
  select(c(date, location, Acoustic_Tag_No, PIT_Tag_No, Length_mm, Weight_g,
           comments, date_st_tm, date_end_tm)) |> 
  rename(start_time = date_st_tm,
         end_time = date_end_tm) |> 
  relocate(c(start_time, end_time), .after = location)

angling_derby_data <- angling_derby_data |> 
  mutate( date = as.Date(date), location = as.character(location), 
          Acoustic_Tag_No = as.numeric(Acoustic_Tag_No), PIT_Tag_No = as.numeric(PIT_Tag_No),
          Length_mm = as.numeric(Length_mm), Weight_g = as.numeric(Weight_g),
          comments = as.character(comments))


