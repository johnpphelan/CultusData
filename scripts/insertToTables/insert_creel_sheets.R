library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
source("scripts/utils/fix_col_names_f.R")
source("scripts/insertToTables/insert_creel_functions.R")

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


file_list<-list.files(pattern = paste0(lan_folder,"2024 projects/Creel Surveys/Creel Survey forms 2024 (backup)/*.xlsx"),
                      recursive = T)

file_list<-list.files(path =paste0(lan_folder,"2024 projects/Creel Surveys/Creel Survey forms 2024 (backup)/"),
                      pattern = "*.xlsx", full.names = T)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)


f_name<-file_list[13]
f_name

creelData<-read_excel(path = f_name, sheet = 1, col_names = TRUE)
names(creelData)[names(creelData) == "# SMB c...15"]<- "# SMB c"
names(creelData)[names(creelData) == "# SMB c...23"]<- "# SMB r"

namesFix<-names(creelData)
namesFix<-gsub(" ", "_", namesFix)
namesFix<-gsub("#", "No", namesFix)
namesFix<-remove_special_chars(namesFix)
names(creelData)<-namesFix

sheets<-excel_sheets(f_name)
sheetOI<-grep("Demographic", sheets, value = TRUE)
demography<-data.frame(lapply(sheetOI, read_excel, path = f_name))

demography <- demography |> 
  rename(surveyNumber = Survey.., time = Date...Time, gender = Gender, ageClass = Age.Class,
         licensePeriod = License.Period, residency = Residency, cityProvinceCountry = City..Prov..Country,
         postCode = Postal.Code..first.3., notes = Notes)

# demography <- demography |> 
#   mutate(time = convertToDateTime(as.numeric(time)))
  
demography <- demography |> 
  mutate(date = format(as.Date(time, "%d-%m-%y")))


sheetOI<-grep("Fish", sheets, value = TRUE)
fishdata<-data.frame(lapply(sheetOI, read_excel, path = f_name))

sheetOI<-grep("ICE", sheets, value = TRUE)
ICE<-data.frame(lapply(sheetOI, read_excel, path = f_name))

# iced<-ICE |> 
#   mutate(Date = as.Date(Date, origin = "1899-12-30"),
#                 Time = convertToDateTime(as.numeric(Time)))

insertToMainCreel(creelData)
insertToFishingCatchCreel(creelData)
insertfishingDetailsCreel(creelData)
insertWeatherCreel(creelData)
insertQuestionsCreel(creelData)
insertSurveyAnswersCreel(creelData)
insertICEData(ICE)
insertAnglerInfo(demography)
insertfishcaughtDetails(fishdata)



dbDisconnect(con)
