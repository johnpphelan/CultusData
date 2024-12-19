library(sqldf)
library(dplyr)
library(tidyr)

setwd("C:/Users/JPHELAN/OneDrive - Government of BC/R_projects/CultusData")

source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

source("scripts/get_data/get_nest_survey.R")

raw_nest_survey <- raw_nest_survey[!is.na(raw_nest_survey$Date), ]
  

names(raw_nest_survey)
nestRaw<- raw_nest_survey
names(nestRaw)<-names_fix(names(nestRaw))
names(nestRaw)<-remove_special_chars(names(nestRaw))
names(nestRaw)<-gsub("_", "",names(nestRaw))



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

nestRaw <- nestRaw |> 
  mutate(nestID = row_number())

nestRaw <- nestRaw |> 
  mutate(Date = as.character(Date)) |> 
    rename(date = Date, depth = Depthofnestm, diameter = Approximatediameterofnestm,
         guarding = PresenceofguardingmaleYN, lifeStage = LifestageonnestEggalevinorfry,
         adjacentStructure = AdjacentstructureegdockormouringbuoyNA, activityCompleted = Activitycompletedobservationvsnestdestruction,
         nestDestroyed = NestfullydestroyedYNPartially, comments = Comments)

nestcoords<-nestRaw[!is.na(nestRaw$easting),]
nestna<-nestRaw[is.na(nestRaw$easting),]
nestSF<- sf::st_as_sf(nestcoords, coords = c("easting", "northing"), crs = 32610) 
bc<-bcmaps::bc_bound() |> sf::st_transform(32610)
library(ggplot2)

ggplot() +
  geom_sf(data = bc, color = "grey") +
  geom_sf(data = nestSF, aes(color = 1:nrow(nestSF))) +
  scale_color_viridis_c(option = "plasma") +
  geom_sf_text(data = nestSF, aes(label = 1:nrow(nestSF)), size = 3, nudge_y = 0.001)
  
nestSF <- nestSF |> 
  sf::st_transform(32610) |> 
  mutate(easting = sf::st_coordinates(geometry)[,1],
         northing = sf::st_coordinates(geometry)[,2],
         ) |> 
  sf::st_drop_geometry()
  
# ggplot() +
#   geom_sf(data = bc, color = "grey") +
#   geom_sf(data = nestSF, aes(color = 1:nrow(nestSF))) +
#   scale_color_viridis_c(option = "plasma") +
#   geom_sf_text(data = nestSF, aes(label = 1:nrow(nestSF)), size = 3, nudge_y = 0.001)

nestRaw<-rbind(nestSF, nestna)

col_types<-get_col_types(nestRaw)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("nestID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS nestRaw (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (nestID))")
dbExecute(con, sql)
dbWriteTable(conn = con, "nestRaw", nestRaw, row.names = F, append = T)
dbListTables(con)
dbDisconnect(con)
