library(data.table)
library(lubridate)
library(readxl)
library(dplyr)
library(stringr)
source("scripts/utils/remove_special_chars_f.R")


lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

#list.files(lan_folder)
#abun_names<-list.files(path = paste0(lan_folder,"2023 projects/abundance estimate"), pattern = ".xlsx", full.names = T)
#abun_data23<-rbindlist(lapply(tag_names, read_excel), fill = T)

capture_raw<-read_excel(path = paste0(lan_folder,"2023 projects/abundance estimate/2023 SMB abundance estimate.xlsx"),
                                            sheet = 1, col_names = T, progress = readxl_progress())

namesFix<-names(capture_raw)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(capture_raw)<-namesFix


## recap - split these? remove them from the excel and have them as a separate table probably

capture_raw |> 
  filter(Acoustic_Tag == "Yes")
capture_raw <- capture_raw %>%
  mutate(
    date = as.Date(date), Seconds_Electricity = as.numeric(Seconds_Electricity),
    Pit_tag = as.numeric(Pit_tag), length = as.numeric(length), weight = as.numeric(weight),
    sex = as.character(sex), Maturity = as.character(Maturity), Method = as.character(Method),
    Recap = as.numeric(Recap), Mort = as.character(Mort), Scale_Book_No = as.numeric(Scale_Book_No),
    Scale_Nos = as.character(Scale_Nos), Comments = as.character(Comments)
  ) |> 
  filter(!is.na(date))

capture_raw<- capture_raw |> 
  mutate(st_tm = hm(Start_Time),
         nd_tm = hm(End_Time)) |> 
  select(-Start_Time, - End_Time) |> 
  rename(Start_time = st_tm, End_Time = nd_tm)



recapture_raw<-read_excel(path = paste0(lan_folder,"2023 projects/abundance estimate/2023 SMB abundance estimate.xlsx"),
                              sheet = 2, col_names = T, progress = readxl_progress())

namesFix<-names(recapture_raw)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(recapture_raw)<-namesFix
names(recapture_raw)

recapture_raw <- recapture_raw |> 
  mutate(
    date = as.Date(date), location = as.character(location), Acoustic_Tag_No = as.numeric(Acoustic_Tag_No),
    PIT_Tag_No = as.numeric(PIT_Tag_No), Length_mm = as.numeric(Length_mm),
    Weight_g = as.numeric(Weight_g), comments = as.character(comments),
    start_time=as.numeric(start_time), end_time=as.numeric(end_time)
  ) |> 
  filter(!is.na(date))  # Filter for non-NA dates within mutate chain

recapture_raw <- recapture_raw |> 
  mutate(date_start_time = (start_time*24*3600),
         date_end_time = (end_time*24*3600),
         date = as.POSIXlt(date),
         date_st_tm = as.POSIXlt(  date + date_start_time),
         date_end_tm = as.POSIXlt(  date + date_end_time)) |> 
  select(-start_time, -end_time) |> 
  mutate(start_time = format(as.POSIXct(date_st_tm, format = "%H:%M"), format = "%H:%M"),
         end_time = format(as.POSIXct(date_end_tm, format = "%H:%M"), format = "%H:%M")) |>
  select(-date_st_tm, -date_end_tm, -date_start_time, -date_end_time) 
  
# |> 
#   select(-start_time, - end_time) |> 
#   rename(Start_Time = st_tm, End_Time = nd_tm)



capture_mark<-read_excel(path = paste0(lan_folder,"2023 projects/abundance estimate/2023 SMB abundance estimate.xlsx"),
                          sheet = 3, col_names = T, progress = readxl_progress())
namesFix<-names(capture_mark)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(capture_mark)<-namesFix

capture_mark <- capture_mark |> 
  mutate(date = as.Date(date), Pit_tag = as.numeric(Pit_tag), length = as.numeric(length),
         weight = as.numeric(weight), sex = as.character(sex), Maturity = as.character(Maturity),
         method = as.character(Method), Recap = as.character(Recap), Mort = as.character(Mort)
         ) |> 
  filter(!is.na(date))


recapture_wrk<-read_excel(path = paste0(lan_folder,"2023 projects/abundance estimate/2023 SMB abundance estimate.xlsx"),
                         sheet = 4, col_names = T, progress = readxl_progress())


names(recapture_wrk)[names(recapture_wrk) == "...5"]<-"comment"

namesFix<-names(recapture_wrk)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(recapture_wrk)<-namesFix
names(recapture_wrk)

recapture_wrk <- recapture_wrk |> 
  mutate(date = as.Date(date), PIT_Tag_No = as.numeric(PIT_Tag_No), Length_mm = as.numeric(Length_mm),
         Weight_g = as.numeric(Weight_g), comment = as.character(comment))

