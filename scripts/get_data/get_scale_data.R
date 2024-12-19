library(data.table)
library(readxl)
library(readODS)
library(dplyr)
library(stringr)
library(chron)
library(lubridate)

source("scripts/utils/fix_col_names_f.R")



lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


scale_data <- read_ods(path = paste0(lan_folder,"2023 projects/Scale aging/2023 Cultus Lk Smallmouth Bass_BCPAL.Aged.Mar27.24.ods"),
                                 sheet = 1, col_names = T)|> 
  #rename_with(~str_replace_all(.x, "\\s", " ")) |> 
  mutate(Start_Time = as.character(`Start Time`),
         End_Time = as.character(`End Time`))





#scale_data<-names_fix(scale_data)

scale_data<-scale_data |> 
  mutate(Date = as.character(Date))

scale_data <- scale_data |> 
  mutate(across(everything(), as.character)) |> 
  mutate(`original order` = as.numeric(`original order`),
         `Scale Book #` = as.numeric(`Scale Book #`),
         `Seconds Electricity` = as.numeric(`Seconds Electricity`),
         `Length mm` = as.numeric(`Length mm`),
         Weight = as.numeric(Weight))

names(scale_data) <- tolower(names(scale_data))

# scale_data <- scale_data |> 
#   mutate(original_order = as.numeric(original_order), Waterbody = as.character(Waterbody),
#          Species = as.character(Species), Scale_Book_No = as.numeric(Scale_Book_No), 
#          Scale_Nos = as.character(Scale_Nos), 
#          BCPAL_To_Age_YN_Filtered_by_Y_or_N = as.character(BCPAL_To_Age_YN_Filtered_by_Y_or_N),
#          GPS_Point = as.character(GPS_Point), 
#          Seconds_Electricity = as.numeric(Seconds_Electricity), 
#          Pit_tag = as.numeric(Pit_tag), 
#          Acoustic_Tag = as.numeric(Acoustic_Tag), 
#          Length_mm = as.numeric(Length_mm), 
#          Weight = as.numeric(Weight),
#          Sex = as.character(Sex), 
#          Maturity = as.character(Maturity),
#          Rand = as.numeric(Rand),
#          Scale_Age_based_on_fertilization_birthdayUsed_to_calculate_Brood_YearFall_spawningspecieswill_have_1_added_to_winter_check_count_if_sampled_September_1st_onwardsIf_Spring_spawning_fishEgg_fertilized_in_spring_months_is_caught_in_the_Spring_Ex_FFSBC_Brood_fish_then_the_ager_may_add_1_onto_No_winter_checks_or_may_not_according_to_whats_visible_on_age_structure_and_agers_final_decison_Look_in_Otolith_Age_with_Edge_Notation_to_clarify_what_was_done = as.character(Scale_Age_based_on_fertilization_birthdayUsed_to_calculate_Brood_YearFall_spawningspecieswill_have_1_added_to_winter_check_count_if_sampled_September_1st_onwardsIf_Spring_spawning_fishEgg_fertilized_in_spring_months_is_caught_in_the_Spring_Ex_FFSBC_Brood_fish_then_the_ager_may_add_1_onto_No_winter_checks_or_may_not_according_to_whats_visible_on_age_structure_and_agers_final_decison_Look_in_Otolith_Age_with_Edge_Notation_to_clarify_what_was_done),
#          Scale_Age_using_Jan1_st_birthday_switchover_for_all_species= as.character(Scale_Age_using_Jan1_st_birthday_switchover_for_all_species),
#          Scale_Age_with_Edge_Notation_Criteria_outlined_in_this_cells_note_when_hovered_over = as.character(Scale_Age_with_Edge_Notation_Criteria_outlined_in_this_cells_note_when_hovered_over),
#          Age_Confidence_Criteria_outlined_in_this_cells_note_when_hovered_over = as.character(Age_Confidence_Criteria_outlined_in_this_cells_note_when_hovered_over),
#          Age_CommentsNotes_about_structure_condition_broken_or_vateritic_otoliths_regen_scales_unable_to_age_data_mix_up_missing_sample_etcvat__vateritic_UD__Upside_Down_RG_or_regen__regenerated = as.character(Age_CommentsNotes_about_structure_condition_broken_or_vateritic_otoliths_regen_scales_unable_to_age_data_mix_up_missing_sample_etcvat__vateritic_UD__Upside_Down_RG_or_regen__regenerated),
#          Imaged_YN = as.character(Imaged_YN))

# scale_data <- scale_data |> 
#   rename(originalOrder = original_order, waterbody = Waterbody, date = Date, species = Species,
#          scaleBookNo = Scale_Book_No, scaleNos = Scale_Nos, bcpalToAge = BCPAL_To_Age_YN_Filtered_by_Y_or_N,
#          GPS = GPS_Point, startTime = Start_Time, endTime = End_Time, secondsElectricity = Seconds_Electricity,
#          pitTagNo = Pit_tag, acousticTagNo = Acoustic_Tag, lengthmm = Length_mm, weightg = Weight,
#          sex = Sex, maturity = Maturity, method = Method, recap = Recap, mort = Mort, comments = Comments,
#          rand = Rand)
  
scale_500_data<-read_excel(path = paste0(lan_folder,"2023 projects/Scale aging/2023 SMB data for 500 scale aging.xlsx"),
                           sheet = 1, col_names = T)
scale_500_data<-names_fix(scale_500_data)  

scale_500_data <- scale_500_data |> 
  mutate(Scale_Book_No = as.character(Scale_Book_No), Scale_Nos = as.character(Scale_Nos), BCPAL_To_Age_YN = as.character(BCPAL_To_Age_YN)
         , GPS_Point = as.character(GPS_Point), Start_Time = as.character(Start_Time),
         End_Time = as.character(End_Time), Seconds_Electricity = as.numeric(Seconds_Electricity),
         Pit_tag = as.numeric(Pit_tag), Acoustic_Tag = as.numeric(Acoustic_Tag), length = as.numeric(length),
         weight = as.numeric(weight), Maturity = as.character(Maturity), sex = as.character(sex),
         Method = as.character(Method), Recap = as.character(Recap), Mort = as.character(Mort),
         Comments = as.character(Comments), Rand = as.character(Rand))
  
  