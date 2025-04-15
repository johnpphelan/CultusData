library(sqldf)
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
source("scripts/get_data/get_scale_data.R")
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

scaleDataRaw<-scale_data
#names(scaleDataRaw)<-names_fix(names(scaleDataRaw))
names(scaleDataRaw)<-gsub(";", ". ", names(scaleDataRaw))

                          

scaleDataRaw <- scaleDataRaw |> 
  pivot_longer(cols = c(23:ncol(scaleDataRaw)),
               names_to = "AgeType",
               values_to = "result")

scaleNames<-unique(scaleDataRaw$AgeType)

scaleNames<- scaleNames |> 
  data.frame() |> 
  mutate(keyID = paste0(LETTERS[row_number()], row_number())) |> 
  rename(detailedNames = scaleNames)

joined_data <- left_join(scaleDataRaw, scaleNames, by = c("AgeType" = "detailedNames"))

names(joined_data)<-gsub(" ", "_", names(joined_data))

updated_data <- joined_data %>%
  mutate(ageType = coalesce(keyID, AgeType)) %>%
  select(-keyID, -AgeType)
scaleDataRaw<-updated_data

names(scaleDataRaw)<-gsub("#", "Num",names(scaleDataRaw))
names(scaleDataRaw)<-gsub("'", "", names(scaleDataRaw))
names(scaleDataRaw) <- gsub("\\\\", "", names(scaleDataRaw))
names(scaleDataRaw)<-gsub("[/()]", "_", names(scaleDataRaw))
scaleDataRaw<- scaleDataRaw |> 
  mutate(date = dmy(date)) |> 
  mutate(date = as.character(date),
         start_time = as.character(start_time),
         end_time = as.character(end_time)) |> 
  rename(length = length_mm) |> 
  rename(pitTagNo = pit_tag, acousticTagNo = acoustic_tag)
  

  


col_types<-get_col_types(scaleDataRaw)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("original_order, AgeType") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS scaleRawTable (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (original_order, AgeType))")
dbExecute(con, sql)
dbWriteTable(conn = con, "scaleRawTable", scaleDataRaw, row.names = F, append = T)
dbListTables(con)
###########################################################################################



names(scaleNames)[2]<-"ageType"

col_types<-get_col_types(scaleNames)

sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("ageType") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS scaleColNameKey (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (ageType),
             \n FOREIGN KEY (AgeType) REFERENCES scaleRawTable (AgeType))")
dbExecute(con, sql)
dbWriteTable(conn = con, "scaleColNameKey", scaleNames, row.names = F, append = T)
dbListTables(con)

#############################################################################################

scale500<-scale_500_data |> 
  mutate(scaleID = row_number())

names(scale500)<-names_fix(names(scale500))
names(scale500)<-gsub("_","", names(scale500))
names(scale500)<-names_fix(names(scale500))
col_types<-get_col_types(scale500)


scale500<- scale500 |> 
  mutate(date = as.character(date)) |> 
  rename(acousticTagNo = AcousticTag, pitTagNo = Pittag)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("scaleID") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS scale500 (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (scaleID))")
dbExecute(con, sql)
dbWriteTable(conn = con, "scale500", scale500, row.names = F, append = T)
dbListTables(con)

#######################################################################################################

dbDisconnect(con)
