library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(sf)
source("scripts/utils/fix_col_names_f.R")
source("scripts/insertToTables/insert_creel_functions.R")

db_filepath = "output/CultusData.sqlite"
bc<-bcmaps::bc_bound()

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


file_loc<-"2025 projects/Nest Destruction/2025 Cultus Lake SMB Nest Destruction Surveys.xlsx"

tot_file<-paste0(lan_folder,file_loc)
sheets<-excel_sheets(tot_file)
sheetOI<-grep("Survey Data", sheets, value = TRUE)
nestData<-data.frame(lapply(sheetOI, read_excel, path = tot_file))
names(nestData)[names(nestData) == "...13"]<-"comments"
names(nestData)[names(nestData) == "...15"]<-"locs"

names(nestData)


bc<-bcmaps::bc_bound() |> sf::st_transform(3005)
# Fixes the isntance where the position was given as lat long
latfix<-nestData |> 
  filter(str_detect(Easting, "[0-9]{2}\\."))
  
#latfix$northing[latfix$northing == 1299517] <- 1293517

nestData <- nestData |> 
  filter(!is.na(Easting) | !is.na(Northing)) |> 
  sf::st_as_sf(coords = c("Easting","Northing" ), crs = 32610) |> 
  mutate(easting = sf::st_coordinates(geometry)[,2],
         northing = sf::st_coordinates(geometry)[,1],
  )

library(leaflet)
# leaflet map the locations of hte nests
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = nestData |> st_transform(4326), color = "blue", radius = 5, label = ~as.character(1:nrow(nestData)))

library(ggplot2)
library(sf)
 

nestDF<- nestData |> 
  rename(date = Date, 
         depth = Depth..m., diameter = Diameter..m., guarding = Guarding.Male..Y.N.,
         lifeStage = Life.stage.of.nest,
         adjacentStrucuture = Adjacent.structure,
         habitatType = Habitat.type, 
         startTime = Start.Time,
         endTime = End.Time,
         location = Location.Name,
         surfaceTemp = Surface..Temp,
         locationMarker = Location.Marker,
         otherInvasives = Other.invasive.species.present.,
         activityCompleted = Activity.completed.by.surveyor, 
         nestDestroyed = Nest.fully.destroyed.,
         substrate = Habitat.type,
         comments = Comments)

## merge locationMarker and adjacentStrucuture, remove them both and then rename it adjacentStrucutre


nestDF <- nestDF |>
  dplyr::mutate(
    adjacentStructure = dplyr::if_else(
      !is.na(adjacentStrucuture) & !is.na(locationMarker),
      paste(adjacentStrucuture, locationMarker, sep = "; "),
      dplyr::coalesce(adjacentStrucuture, locationMarker)
    )
  ) |> 
  dplyr::select(-locationMarker, -adjacentStrucuture) |> 
  sf::st_drop_geometry()



nestDF <- nestDF |> 
  mutate(date = as.character(date))

nestDF <- nestDF |>
  mutate(
    startTime = as.POSIXct(startTime * 86400,
                         origin = "1899-12-30",
                         tz = "UTC"),
    endTime = as.POSIXct(endTime * 86400,
                         origin = "1899-12-30",
                         tz = "UTC")
  )


nestDF |>
  group_by(date) |>
  summarise(count = n(), .groups = "drop")

### Adding new columns to the data table 
# addCol<-"ALTER TABLE nestRaw ADD COLUMN AIS VARCHAR"
# dbExecute(con = con, addCol)
# addCol<-"ALTER TABLE nestRaw ADD COLUMN Substrate VARCHAR"
# dbExecute(con = con, addCol)
# delCol<-"ALTER TABLE nestRaw DROP COLUMN locationMarker"
# dbExecute(con = con, delCol)
addCol<-"ALTER TABLE nestRaw ADD COLUMN startTime VARCHAR"
dbExecute(con = con, addCol)

addCol<-"ALTER TABLE nestRaw ADD COLUMN endTime VARCHAR"
dbExecute(con = con, addCol)

addCol<-"ALTER TABLE nestRaw ADD COLUMN Participants VARCHAR"
dbExecute(con = con, addCol)

addCol<-"ALTER TABLE nestRaw ADD COLUMN location VARCHAR"
dbExecute(con = con, addCol)

addCol<-"ALTER TABLE nestRaw ADD COLUMN surfaceTemp VARCHAR"
dbExecute(con = con, addCol)

addCol<-"ALTER TABLE nestRaw ADD COLUMN otherInvasives VARCHAR"
dbExecute(con = con, addCol)



dbAppendTable(con, "nestRaw", nestDF)

query <- "SELECT * FROM nestRaw"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)




