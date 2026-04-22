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
# ggplot() +
#   geom_sf(data = bc, color = "grey") +
#   geom_sf(data = nestData, aes(color = 1:nrow(latfix))) +
#   scale_color_viridis_c(option = "plasma") +
#   geom_sf_text(data = latfix, aes(label = 1:nrow(latfix)), size = 3, nudge_y = 0.001)
# 
# latfix[25,]$northing
# latfix$northing[latfix$northing == 1299517] <- 1293517
# latfix[25,]$northing <- 1293517
# latfix[25,]$easting <- 444402.7
# 
# latfix[25,]$northing
# latfix[25,]$easting
# latfix <- latfix |> 
#   st_drop_geometry() |> 
#   st_as_sf(coords = c("northing","easting"), crs = 3005) |> 
#   st_transform(32610)
# 
# ggplot() +
#   geom_sf(data = bc, color = "grey") +
#   geom_sf(data = latfix, aes(color = 1:nrow(latfix))) +
#   scale_color_viridis_c(option = "plasma") +
#   geom_sf_text(data = latfix, aes(label = 1:nrow(latfix)), size = 3, nudge_y = 0.001)


# fixes the instance where the position was given a different CRS
# east<-nestData |> 
#   filter(!str_detect(Latitude, "[0-9]{2}\\.")) |> 
#   mutate(new_north = ifelse(str_count(Latitude, "[0-9]")==7, Latitude, Longitude),
#          new_east = ifelse(str_count(Longitude, "[0-9]")==6, Longitude, Latitude)
#          ) |> 
#   sf::st_as_sf(coords = c( "new_east","new_north"), crs = 32610) |> 
#   mutate(easting = sf::st_coordinates(geometry)[,2],
#          northing = sf::st_coordinates(geometry)[,1],
#   ) |> 
#   select(-c(Latitude, Longitude))
# 
# ggplot() +
#   geom_sf(data = bc, color = "grey") +
#   geom_sf(data = east, aes(color = 1:nrow(east))) +
#   scale_color_viridis_c(option = "plasma") +
#   geom_sf_text(data = east, aes(label = 1:nrow(east)), size = 3, nudge_y = 0.001)
# 
# latfix <- latfix |> 
#   mutate(easting = sf::st_coordinates(geometry)[,2],
#          northing = sf::st_coordinates(geometry)[,1],
#   ) |> 
#   st_drop_geometry()
# 
# east <- east |> 
#   st_drop_geometry()
# 
# 
# blanks<- nestData |> 
#   filter(is.na(Latitude)) |> 
#   rename(easting = Latitude, northing = Longitude)
# 
#   
# nestdataClean<-rbind(latfix, east)
# nestdataClean<-rbind(nestdataClean, blanks)



# nestSF<- sf::st_as_sf(nestdataClean[!is.na(nestdataClean$easting),], coords = c("northing", "easting"), crs = 32610)
# 
# ggplot() +
#   geom_sf(data = bc, color = "grey") +
#   geom_sf(data = nestSF, aes(color = 1:nrow(nestSF))) +
#   scale_color_viridis_c(option = "plasma") +
#   geom_sf_text(data = nestSF, aes(label = 1:nrow(nestSF)), size = 3, nudge_y = 0.001)






# test <- nestData |> 
#   mutate(Nests.Destroyed = case_when(
#     !is.na(Nests.Destroyed) & Nests.Destroyed == "Y" ~ "Nest destroyed.",
#     !is.na(Nests.Destroyed) & Nests.Destroyed == "N" ~ "Nest not destroyed.",
#     !is.na(Nests.Destroyed) & Nests.Destroyed == "P" ~ "Nest partially destroyed.",
#     !is.na(Nests.Destroyed) & Nests.Destroyed == "n/a" ~ "",
#     !is.na(Nests.Destroyed) & Nests.Destroyed == "N (prev Y)" ~ "Nest not destroyed. ",
#     is.na(Nests.Destroyed) ~ "", # or any other desired value for NA
#     TRUE ~ ""
#   )) |> 
#   mutate(Fry.Captured = case_when(
#     !is.na(Fry.Captured) & Fry.Captured == "N" ~ "No fry captured. ",
#     !is.na(Fry.Captured) & Fry.Captured == "Y" ~ "Fry captured. ",
#     !is.na(Fry.Captured) & Fry.Captured == "Y (3)" ~ "Three fry captured. ",
#     !is.na(Fry.Captured) & Fry.Captured == "Y (2)" ~ "Two fry captured. ",
#     !is.na(Fry.Captured) & Fry.Captured == "Y (1)" ~ "Fry captured. ",
#     !is.na(Fry.Captured) & Fry.Captured == "N (attempted)" ~ "Tried to catch fry, unsuccessful. ",
#     !is.na(Fry.Captured) & Fry.Captured == "n/a" ~ "",
#     is.na(Fry.Captured) ~ "",
#     TRUE ~ ""
#   )) |> 
#   mutate(activity = paste(Nests.Destroyed, Fry.Captured, sep = ". ")) |> 
#   mutate(comments = paste(comments, location, ". Substrate:",Substrate, sep = " "))
#   

nestDF<- nestData |> 
  rename(date = Date, easting = Easting, northing = Northing, 
         depth = Depth..m., diameter = Diameter..m., guarding = Gaurding.Male..Y.N.,
         lifeStage = Life.stage.of.nest,
         adjacentStructure = Adjacent.structure,
         habitatType = Habitat.type, 
         
         
         activityCompleted = activity.completed.by.surveyor, 
         nestDestroyed = Nest.fully.destroyed.,
         
         comments = comments) |> 
  select(-c(locs, Fry.Captured, location))

nestDF <- nestDF |> 
  mutate(date = as.character(date))

### Adding new columns to the data table 
addCol<-"ALTER TABLE nestRaw ADD COLUMN AIS VARCHAR"
dbExecute(con = con, addCol)
addCol<-"ALTER TABLE nestRaw ADD COLUMN Substrate VARCHAR"
dbExecute(con = con, addCol)


dbAppendTable(con, "nestRaw", nestDF)

query <- "SELECT * FROM nestRaw"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)




