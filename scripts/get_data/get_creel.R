library(data.table)
library(readxl)
library(dplyr)
library(stringr)
source("scripts/utils/fix_col_names_f.R")



lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


main_page<-read_excel(path = paste0(lan_folder,"2023 projects/creel survey/2023 SMB creel_final working.xlsx"),
                             sheet = 1, col_names = T, progress = readxl_progress())
namesFix<-names(main_page)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(main_page)<-namesFix
names(main_page)

questionsAsked<-names(main_page)


main_page <- main_page |> 
  mutate(Survey_No = as.numeric(Survey_No), Surveyor = as.character(Surveyor), Date = as.Date(Date),
         Time = as.POSIXct(Time), Shift = as.character(Shift), Site = as.character(Site),
         Mean_Air_Temperature = as.numeric(Mean_Air_Temperature), Cloud_Cover = as.character(Cloud_Cover),
         Wind = as.character(Wind), Precip = as.character(Precip), No_Anglers = as.numeric(No_Anglers),
         No_Rods = as.numeric(No_Rods), Total_Person_Hr_Fished = as.numeric(Total_Person_Hr_Fished), 
         Total_Fish_Caught = as.numeric(Total_Fish_Caught), No_pikeminnow_caught = as.numeric(No_pikeminnow_caught),
         No_KO_c, as.numeric(No_KO_c), No_CT_c = as.numeric(No_CT_c), No_RB_c = as.numeric(No_RB_c),
         No_BT_c, as.numeric(No_BT_c), No_LT_c = as.numeric(No_LT_c), No_Other_Spp_c = as.character(No_Other_Spp_c),
         Total_Retained = as.numeric(Total_Retained), No_pikeminnow_r = as.numeric(No_pikeminnow_r),
         No_KO_r, as.numeric(No_KO_r), No_CT_r = as.numeric(No_CT_r), No_RB_r = as.numeric(No_RB_r),
         No_BT_r, as.numeric(No_BT_r), No_LT_r = as.numeric(No_LT_r), No_Other_Spp_r = as.character(No_Other_Spp_r),
         Release_Reason = as.character(Release_Reason), Vessel = as.character(Vessel), 
         Preferred_Catch_Spp = as.character(Preferred_Catch_Spp), Why = as.character(Why), 
         Why_this_Location = as.character(Why_this_Location), How_satisfied_were_you_with_your_fishing_experience_today = 
           as.character(How_satisfied_were_you_with_your_fishing_experience_today),
         Yearly_Fishing_Freq_at_this_location = as.character(Yearly_Fishing_Freq_at_this_location),
         How_many_bass_do_you_typically_catch_during_a_day_fishing_at_Cultus = as.character(How_many_bass_do_you_typically_catch_during_a_day_fishing_at_Cultus),
         Are_you_aware_that_the_movement_of_live_fish_and_the_use_of_live_bait_are_prohibited_in_BC = 
           as.character(Are_you_aware_that_the_movement_of_live_fish_and_the_use_of_live_bait_are_prohibited_in_BC),
         Are_you_aware_of_the_Clean_Drain_Dry_practices = as.character(Are_you_aware_of_the_Clean_Drain_Dry_practices),
         Interviewed_this_year_at_this_location = as.character(Interviewed_this_year_at_this_location),
         Angler_comments = as.character(Angler_comments), Notes = as.character(Notes))

fish_edit<-read_excel(path = paste0(lan_folder,"2023 projects/creel survey/2023 SMB creel_final working.xlsx"),
                      sheet = 2, col_names = T, progress = readxl_progress())
fish_edit<-names_fix(fish_edit)
names(fish_edit)[names(fish_edit) == "Date__Time"]<-"Date_Time"

fish_edit <- fish_edit |> 
  mutate(Survey_No = as.numeric(Survey_No), Date_Time = as.POSIXct(Date_Time),
         Fish_No = as.numeric(Fish_No), Spp = as.character(Spp), Length_mm = as.numeric(Length_mm),
         Weight_g = as.numeric(Weight_g), PIT_tag_No = as.numeric(PIT_tag_No), Notes = as.character(Notes))

demography_edit<-fish_edit<-read_excel(path = paste0(lan_folder,"2023 projects/creel survey/2023 SMB creel_final working.xlsx"),
                                 sheet = 3, col_names = T, progress = readxl_progress())
demography_edit<-names_fix(demography_edit)
names(demography_edit)[names(demography_edit) == "Date__Time"]<-"Date_Time"

demography_edit <- demography_edit |> 
  mutate(Survey = as.numeric(Survey), Date_Time = as.POSIXct(Date_Time), Gender = as.character(Gender),
         Age_Class = as.character(Age_Class), License_Period = as.character(License_Period), 
         Residency = as.character(Residency), City_Prov_Country = as.character(City_Prov_Country),
         Postal_Code_first_3 = as.character(Postal_Code_first_3), Notes = as.character(Notes))

ICE<-fish_edit<-read_excel(path = paste0(lan_folder,"2023 projects/creel survey/2023 SMB creel_final working.xlsx"),
                                       sheet = 4, col_names = T, progress = readxl_progress())
ICE<-names_fix(ICE)
names(ICE)[names(ICE) == "8"]<-"Weather"

ICE <- ICE |> 
  mutate(Date = as.Date(Date), Time = as.POSIXct(Time), No_angling_boats = as.numeric(No_angling_boats),
         No_boat_anglers = as.numeric(No_boat_anglers), No_shore_anglers = as.numeric(No_shore_anglers),
         No_dock_anglers = as.numeric(No_dock_anglers), Comments = as.character(Comments), 
         Weather = as.character(Weather))
