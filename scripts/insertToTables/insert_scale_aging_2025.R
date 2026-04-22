library(sqldf)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(readxl)
library(stringr)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)


lan_folder = "//sfp.idir.bcgov/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


scale_data <- read_excel(path = paste0(lan_folder,"2025 projects/Scale Aging 2025/Results/2025 Cultus LK_SMB_WLRS_BCPAL.Aged_Mar23.2026.xlsx"),
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
  rename(originalOrder = `original order`, waterbody = `Waterbody Name`, date = `Collection Date`,
         species = Species, toAge = `To Be Aged by BCPAL? Y/N`,
         length = `Length (mm)`, weight = `Weight (g)`,
         stomach = `Stomach Contents`) |> 
  mutate(across(everything(), ~as.character(.)))



scale_data <- scale_data |>
  mutate(scaleBookNo = str_extract(`Unique Fish ID`, "^[^-]+"),
         scaleNo = str_extract(`Unique Fish ID`, "(?<=-).*"))



scale_data = scale_data |> 
  select(-c(`Waterbody ID (if known)`)) |> 
  rename(uniqueFishID = `Unique Fish ID`, condition = condition_factor, maturity = Maturity, otolithCollected = `Otolith collected?`,
         scaleCollected = `Scale Collected?`, otolithID = `Otolith ID (if different from Fish ID`, sex = Sex) 


original_names <- colnames(scale_data)


scale_data <- scale_data |>
  rename(
    eggYear = matches("^Aged_Brood_Year"),
    scaleAgeFertilisation = matches("^SCALE AGE based on fertilization birthday"),
    scaleAgeEdgeNotation = matches("^SCALE Age with Edge Notation"),
    scaleAgeJan1 = matches("^Scale Age using Jan1stbirthday"),
    ageConfidence = matches("^Age Confidence"),
    ageComments = matches("^Age Comments"),
    imaged = matches("^Imaged Y/N")
  )


renamed_names <- colnames(scale_data)

# Match old and new names for renamed columns
name_map <- data.frame(
  original = original_names[original_names != renamed_names],
  renamed  = renamed_names[renamed_names != original_names]
)

renamed_only <- name_map[name_map$original != name_map$renamed, ]

write.csv(name_map, "data/column_renaming_reference_scaleDataRaw2025.csv", row.names = FALSE)

# dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN uniqueFishID TEXT")
# dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN condition TEXT")
# dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN otolithCollected TEXT")
# dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN scaleCollected TEXT")
# dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN otolithID TEXT")
# dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN stomach TEXT")
# dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN comment TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN otolithAgeFertilization TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN otolithAgeEdge TEXT")
dbExecute(con, "ALTER TABLE scaleRawTable ADD COLUMN otolithJan1stAge TEXT")


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

test = scale_data

scale_data = test |> 
  select(-c(Region, `Scale ID (if different from Fish ID`)) |> 
  rename(otolithAgeFertilization = `OTOLITH Age: Fertilization`,
         otolithAgeEdge = `OTOLITH Age with Edge`,
         otolithJan1stAge = `OTOLITH Jan1st Age`)

dbAppendTable(con, "scaleRawTable", scale_data)
dbDisconnect(con)










