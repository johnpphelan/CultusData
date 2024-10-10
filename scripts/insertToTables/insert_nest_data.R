library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
source("scripts/utils/fix_col_names_f.R")
source("scripts/insertToTables/insert_creel_functions.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


file_loc<-"2024 projects/Nest Surveys/Survey_Data_Compiled-Nicole_Kaminski.xlsx"

tot_file<-paste0(lan_folder,file_loc)
sheets<-excel_sheets(tot_file)
sheetOI<-grep("Nests", sheets, value = TRUE)
nestData<-data.frame(lapply(sheetOI, read_excel, path = tot_file))
names(nestData)[names(nestData) == "..13"]<-"comments"

names(nestData)





