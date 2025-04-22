library(data.table)
library(sqldf)
library(dplyr)
library(stringr)
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

tag_names<-list.files(path = paste0(lan_folder,"2023 projects/acoustic telemetry/2023-2024 downloads"),
                      pattern = ".csv", full.names = T)
tag_data<-rbindlist(lapply(tag_names, fread), fill = TRUE)


db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

names_fix(names(tag_data))
names(tag_data)<-remove_special_chars(names(tag_data))

tag_data |> 
  rename()



