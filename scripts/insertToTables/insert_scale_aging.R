library(sqldf)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)


lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


scale_data <- read_excel(path = paste0(lan_folder,"2024 projects/Scale aging/2024 Cultus Lk_SMB_WLRS_2024-09-17_BCPAL.Aged_Mar8.2025.xlsx"),
                       sheet = 1, col_names = T)

query <- "SELECT * FROM scaleRawTable"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)

names(scale_data)

names(scale_data)<-gsub(";", ". ", names(scale_data))
ncol(scale_data)
names(scale_data)

scale_data <- scale_data |> 
  rename(originalOrder = `BCPAL original order`, waterbody = `Waterbody Name`, date = Date,
         species = Species, toAge = `To Be Aged by BCPAL? Y/N`,
         length = `Length (mm)`, weight = `Weight (g)`, scaleBookNo = `Sc Book #`, scaleNo = `Scale ID (Scale Book #, Scale #'s)`,
         stomach = `Stomach Contents`) |> 
  mutate(across(everything(), ~as.character(.)))

scale_data = scale_data |> 
  select(-c(`Waterbody ID (if known)`)) |> 
  rename(uniqueFishID = `Unique Fish ID`, condition = condition_factor, maturity = Maturity, otolithCollected = `Otolith collected?`,
         scaleCollected = `Scale Collected?`, otolithID = `Otolith ID (if different from Fish ID`, sex = Sex) 


original_names <- colnames(scale_data)

scale_data = scale_data |> 
  rename(eggYear = `Aged_Brood_Year Calendar year in which this fish was an egg.Spawn Year/Egg Fertilization`,
         scaleAgeFertilisation = `SCALE AGE based on fertilization birthday: Used to calculate Brood Year:Fall spawning species will have 1 added to winter check count if sampled September 1st onwards.  If Spring spawning fish (Egg fertilized in spring months) is caught in the Spring  then the ager may add 1 onto "# winter checks" or may not according to catch date and what's visible on age structure and ager's final decison. SMB spawning late May into June. The Cultus Lk SMB are being caught during their spawning season.`,
         scaleAgeEdgeNotation = `SCALEAge with Edge Notation`, 
         scaleAgeJan1 = `Scale Age using Jan1stbirthday switchover for all species`,
         ageConfidence = `Age Confidence (Criteria outlined in this cell's note when hovered over)`,
         ageComments = `Age Comments: Notes about structure condition (broken or vateritic otoliths, regen scales).  unable to age.  data mix up, missing sample etc. vat = vateritic. UD = Upside Down.  RG or regen = regenerated.  "+" growth = "plus" growth`,
         imaged = `Imaged Y/N`)

renamed_names <- colnames(scale_data)

# Match old and new names for renamed columns
name_map <- data.frame(
  original = original_names[original_names != renamed_names],
  renamed  = renamed_names[renamed_names != original_names]
)

renamed_only <- name_map[name_map$original != name_map$renamed, ]

write.csv(name_map, "data/column_renaming_reference_scaleDataRaw2025.csv", row.names = FALSE)





dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN uniqueFishID TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN condition TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN otolithCollected TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN scaleCollected TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN otolithID TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN stomach TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN comment TEXT")


query <- query <- "SELECT * FROM scaleRawTable"
result <- dbGetQuery(con, query)

initCount<- result |> 
  arrange(desc(originalOrder))
initCount<-slice(initCount, 1)
initCount<-initCount$originalOrder


scale_data <- scale_data |> 
  mutate(uniqueOrder = row_number() + as.numeric(initCount)) |> 
  mutate(originalOrder = uniqueOrder) |>
  select(-uniqueOrder) |> 
  mutate(originalOrder = as.character(originalOrder))

dbAppendTable(con, "scaleRawTable", scale_data)
dbDisconnect(con)










