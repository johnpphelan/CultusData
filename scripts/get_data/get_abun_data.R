library(data.table)
library(readxl)
library(dplyr)
library(stringr)

remove_special_chars <- function(string) {
  # Regular expression to match all special characters except underscores
  pattern <- "[^\\w_]"
  
  # Replace matched characters with an empty string
  str_replace_all(string, pattern, "")
}

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

#list.files(lan_folder)
#abun_names<-list.files(path = paste0(lan_folder,"2023 projects/abundance estimate"), pattern = ".xlsx", full.names = T)
#abun_data23<-rbindlist(lapply(tag_names, read_excel), fill = T)

capture_raw_init<-read_excel(path = paste0(lan_folder,"2023 projects/abundance estimate/2023 SMB abundance estimate.xlsx"),
                                            sheet = 1, col_names = T, progress = readxl_progress())

capture_raw<-capture_raw_init
namesFix<-names(capture_raw)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(capture_raw)<-namesFix
names(capture_raw)



unique(capture_raw$Seconds_Electricity)


capture_raw$Pit_tag<-as.numeric(capture_raw$Pit_tag)


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

head(capture_raw)
rm(capture_raw_init)
str(capture_raw)


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
    Weight_g = as.numeric(Weight_g), comments = as.character(comments)
  ) |> 
  filter(!is.na(date))  # Filter for non-NA dates within mutate chain

capture_mark<-read_excel(path = paste0(lan_folder,"2023 projects/abundance estimate/2023 SMB abundance estimate.xlsx"),
                          sheet = 3, col_names = T, progress = readxl_progress())
namesFix<-names(capture_mark)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(capture_mark)<-namesFix
names(capture_mark)
capture_mark <- capture_mark |> 
  mutate(date = as.Date(date), Pit_tag = as.numeric(Pit_tag), length = as.numeric(length),
         weight = as.numeric(weight), sex = as.character(sex), Maturity = as.character(Maturity),
         method = as.character(Method), Recap = as.character(Recap), Mort = as.character(Mort)
         ) |> 
  filter(!is.na(date))

recapture_wrk<-read_excel(path = paste0(lan_folder,"2023 projects/abundance estimate/2023 SMB abundance estimate.xlsx"),
                         sheet = 4, col_names = T, progress = readxl_progress())
recapture_wrk

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

