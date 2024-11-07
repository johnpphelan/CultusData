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

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"


file_loc<-"2024 projects/Nest Surveys/2024_Nest_Survey_Data_Compiled-Nicole_Kaminski.xlsx"

tot_file<-paste0(lan_folder,file_loc)
sheets<-excel_sheets(tot_file)
sheetOI<-grep("Nests", sheets, value = TRUE)
nestData<-data.frame(lapply(sheetOI, read_excel, path = tot_file))
names(nestData)[names(nestData) == "...13"]<-"comments"
names(nestData)[names(nestData) == "...15"]<-"locs"

names(nestData)

# Fixes the isntance where the position was given as lat long
latfix<-nestData |> 
  filter(str_detect(Latitude, "[0-9]{2}\\.")) |> 
  sf::st_as_sf(coords = c("Longitude","Latitude" ), crs = 4326) |> 
  sf::st_transform(3005) |> 
  mutate(easting = sf::st_coordinates(geometry)[,2],
         northing = sf::st_coordinates(geometry)[,1],
  ) |> 
  sf::st_drop_geometry()
  
# fixes the instance where the position was given a different CRS
east<-nestData |> 
  filter(!str_detect(Latitude, "[0-9]{2}\\.")) |> 
  mutate(new_north = ifelse(str_count(Latitude, "[0-9]")==7, Latitude, Longitude),
         new_east = ifelse(str_count(Longitude, "[0-9]")==6, Longitude, Latitude)
         ) |> 
  sf::st_as_sf(coords = c( "new_east","new_north"), crs = 32610) |> 
  sf::st_transform(3005) |> 
  mutate(easting = sf::st_coordinates(geometry)[,2],
         northing = sf::st_coordinates(geometry)[,1],
  ) |> 
  sf::st_drop_geometry() |> 
  select(-c(Latitude, Longitude))

blanks<- nestData |> 
  filter(is.na(Latitude)) |> 
  rename(easting = Latitude, northing = Longitude)
#32610

# library(ggplot2)
# ggplot2::ggplot() +
# geom_sf(data=bc)+ 
#   geom_sf(data = east, color = "green") +
#   geom_sf(data = latfix, color = "brown")
  
nestdataClean<-rbind(latfix, east)
nestdataClean<-rbind(nestdataClean, blanks)

test <- nestdataClean |> 
  mutate(Nests.Destroyed = case_when(
    !is.na(Nests.Destroyed) & Nests.Destroyed == "Y" ~ "Nest destroyed.",
    !is.na(Nests.Destroyed) & Nests.Destroyed == "N" ~ "Nest not destroyed.",
    !is.na(Nests.Destroyed) & Nests.Destroyed == "P" ~ "Nest partially destroyed.",
    !is.na(Nests.Destroyed) & Nests.Destroyed == "n/a" ~ "",
    !is.na(Nests.Destroyed) & Nests.Destroyed == "N (prev Y)" ~ "Nest not destroyed. ",
    is.na(Nests.Destroyed) ~ "", # or any other desired value for NA
    TRUE ~ ""
  )) |> 
  mutate(Fry.Captured = case_when(
    !is.na(Fry.Captured) & Fry.Captured == "N" ~ "No fry captured. ",
    !is.na(Fry.Captured) & Fry.Captured == "Y" ~ "Fry captured. ",
    !is.na(Fry.Captured) & Fry.Captured == "Y (3)" ~ "Three fry captured. ",
    !is.na(Fry.Captured) & Fry.Captured == "Y (2)" ~ "Two fry captured. ",
    !is.na(Fry.Captured) & Fry.Captured == "Y (1)" ~ "Fry captured. ",
    !is.na(Fry.Captured) & Fry.Captured == "N (attempted)" ~ "Tried to catch fry, unsuccessful. ",
    !is.na(Fry.Captured) & Fry.Captured == "n/a" ~ "",
    is.na(Fry.Captured) ~ "",
    TRUE ~ ""
  )) |> 
  mutate(activity = paste(Nests.Destroyed, Fry.Captured, sep = ". ")) |> 
  mutate(comments = paste(comments, location, ". Substrate:",Substrate, sep = " "))
  

nestDF<- nestdataClean |> 
  rename(date = Survey.., easting = easting, northing = northing, 
         depth = Depth, diameter = Diameter, guarding = Gaurding.Male, lifeStage = Life.Stage,
         adjacentStructure = Adj..Structure, activityCompleted = activity, nestDestroyed = Nests.Destroyed,
         comments = comments) |> 
  select(-c(locs, Fry.Captured, location))

### Adding new columns to the data table 
# addCol<-"ALTER TABLE nestRaw ADD COLUMN AIS VARCHAR"
# dbExecute(con = con, addCol)


dbAppendTable(con, "nestRaw", nestDF)

query <- "SELECT * FROM nestRaw"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)




