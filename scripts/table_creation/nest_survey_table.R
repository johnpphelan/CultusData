library(sqldf)
library(dplyr)
library(tidyr)

source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

source("scripts/get_data/get_nest_survey.R")
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
