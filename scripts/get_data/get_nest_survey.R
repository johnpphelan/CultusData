library(data.table)
library(readxl)
library(dplyr)
library(stringr)
source("scripts/utils/fix_col_names_f.R")



lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


raw_nest_survey<-read_excel(path = paste0(lan_folder,"2023 projects/nest surveys/2023 nest survey data raw.xlsx"),
                      sheet = 1, col_names = T, progress = readxl_progress())
raw_nest_survey<-names_fix(raw_nest_survey)

names(raw_nest_survey)[names(raw_nest_survey) == "northinh"] <-"northing"

testnum<-raw_nest_survey$Approximate_diameter_of_nest_m
testnum<-as.numeric(testnum)
raw_nest_survey <- raw_nest_survey |> 
  mutate(Date = as.Date(Date), waypoint = as.numeric(waypoint), easting = as.numeric(easting),
         northing = as.numeric(northing), Depth_of_nest_m = as.numeric(Depth_of_nest_m),
         Approximate_diameter_of_nest_m = as.character(Approximate_diameter_of_nest_m),
         Presence_of_guarding_male_YN = as.character(Presence_of_guarding_male_YN), 
         Life_stage_on_nest_Egg_alevin_or_fry = as.character(Life_stage_on_nest_Egg_alevin_or_fry),
         Adjacent_structure_eg_dock_or_mouring_buoy_NA = as.character(Adjacent_structure_eg_dock_or_mouring_buoy_NA),
         Activity_completed_observation_vs_nest_destruction = as.character(Activity_completed_observation_vs_nest_destruction),
         Nest_fully_destroyed_YNPartially = as.character(Nest_fully_destroyed_YNPartially), Comments = as.character(Comments))

nest_data_wrk<-read_excel(path = paste0(lan_folder,"2023 projects/nest surveys/2023 nest survey data raw.xlsx"),
                                           sheet = 2, col_names = T, progress = readxl_progress())
nest_data_wrk<-names_fix(nest_data_wrk)
nest_data_wrk <- nest_data_wrk |> 
  mutate(Date = as.Date(Date), waypoint = as.numeric(waypoint), nest_ID_name = as.numeric(nest_ID_name),
         easting = as.numeric(easting), northing = as.numeric(northing), water_temperature_C = as.numeric(water_temperature_C),
         Depth_of_nest_m = as.numeric(Depth_of_nest_m), Approximate_diameter_of_nest_m = as.character(Approximate_diameter_of_nest_m),
         Presence_of_guarding_male_YN = as.character(Presence_of_guarding_male_YN), Life_stage_on_nest_Egg_alevin_or_fry = as.character(Life_stage_on_nest_Egg_alevin_or_fry),
         Adjacent_structure_eg_dock_or_mouring_buoy_NA = as.character(Adjacent_structure_eg_dock_or_mouring_buoy_NA),
         Activity_completed_observation_vs_nest_destruction = as.character(Activity_completed_observation_vs_nest_destruction),
         Comments = as.character(Comments)) 

lifestage<-read_excel(path = paste0(lan_folder,"2023 projects/nest surveys/2023 nest survey data raw.xlsx"),
                      sheet = 3, col_names = T, progress = readxl_progress())
lifestage<- lifestage |> 
select(c(Date, Lifestage))

lifestage<- lifestage |> 
  mutate(Date = as.Date(Date), Lifestage = as.character(Lifestage))

guarding_males<-read_excel(path = paste0(lan_folder,"2023 projects/nest surveys/2023 nest survey data raw.xlsx"),
                           sheet = 4, col_names = T, progress = readxl_progress())
guarding_males <- guarding_males |> 
  select(c(Date, `Presence of guarding male? (Y/N)`))

guarding_males<-names_fix(guarding_males)         
guarding_males<- guarding_males |> 
  mutate(Date = as.Date(Date), Presence_of_guarding_male_YN = as.character(Presence_of_guarding_male_YN))

guarding_males_comb<-read_excel(path = paste0(lan_folder,"2023 projects/nest surveys/2023 nest survey data raw.xlsx"),
                           sheet = 5, col_names = T, progress = readxl_progress())
guarding_males_comb<-names_fix(guarding_males_comb)
guarding_males_comb <- guarding_males_comb |> 
  select(c(Date, Presence_of_guarding_male_YN, Lifestage))
guarding_males_comb<- guarding_males_comb |> 
  mutate(Date = as.Date(Date), Presence_of_guarding_male_YN = as.character(Presence_of_guarding_male_YN),
         )

