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

original_names <- colnames(scaleDataRaw)

scaleDataRaw <- scaleDataRaw |> 
  rename(eggYear = `aged_brood_yearcalendar year in which this fish was an egg.spawn year/egg fertilization`,
         scaleAgeFertilisation = `scale age based on fertilization birthday:used to calculate brood year:fall spawningspecieswill have 1 added to winter check count if sampled september 1st onwards. if spring spawning fish(egg fertilized in spring months) is caught in the spring (ex. ffsbc brood fish) then the ager may add 1 onto \"# winter checks\" or may not according to what's visible on age structure and ager's final decison. look in \"otolith age with edge notation\" to clarify what was done.`,
         scaleAgeJan1 = `scale age using jan1 st birthday switchover for all species`,
         scaleAgeEdgeNotation = `scale age with edge notation (criteria outlined in this cell's note when hovered over)`,
         ageConfidence = `age confidence (criteria outlined in this cell's note when hovered over)`,
         ageComments = `age comments:notes about structure condition (broken or vateritic otoliths, regen scales).  unable to age.  data mix up, missing sample etc.vat = vateritic. ud = upside down.  rg or regen = regenerated. `,
         imaged = `imaged y/n`,
         startTime = start_time,
         endTime = end_time)                      

renamed_names <- colnames(scaleDataRaw)

# Match old and new names for renamed columns
name_map <- data.frame(
  original = original_names[original_names != renamed_names],
  renamed  = renamed_names[renamed_names != original_names]
)

renamed_only <- name_map[name_map$original != name_map$renamed, ]

write.csv(name_map, "data/column_renaming_reference_scaleDataRaw.csv", row.names = FALSE)

# scaleDataRaw <- scaleDataRaw |> 
#   pivot_longer(cols = c(23:ncol(scaleDataRaw)),
#                names_to = "AgeType",
#                values_to = "result")
# 
# scaleNames<-unique(scaleDataRaw$AgeType)
# 
# scaleNames<- scaleNames |>
#   data.frame() |>
#   mutate(keyID = paste0(LETTERS[row_number()], row_number())) |>
#   rename(detailedNames = scaleNames)
# 
# scaleNames <- scaleNames |> 
#   rename(eggYear = `aged_brood_yearcalendar year in which this fish was an egg.spawn year/egg fertilization`,
#          scaleAgeFertilisation = `scale age based on fertilization birthday:used to calculate brood year:fall spawningspecieswill have 1 added to winter check count if sampled september 1st onwards. if spring spawning fish(egg fertilized in spring months) is caught in the spring (ex. ffsbc brood fish) then the ager may add 1 onto \"# winter checks\" or may not according to what's visible on age structure and ager's final decison. look in \"otolith age with edge notation\" to clarify what was done.`,
#          scaleAgeJan1 = `scale age using jan1 st birthday switchover for all species`,
#          scaleAgeEdgeNotation = `scale age with edge notation (criteria outlined in this cell's note when hovered over)`,
#          ageConfidence = `age confidence (criteria outlined in this cell's note when hovered over)`,
#          ageComments = `age comments:notes about structure condition (broken or vateritic otoliths, regen scales).  unable to age.  data mix up, missing sample etc.vat = vateritic. ud = upside down.  rg or regen = regenerated. `,
#          imaged = `imaged y/n`,
#          startTime = start_time,
#          endTime = end_time)
# 
# 
# 
# 
# joined_data <- left_join(scaleDataRaw, scaleNames, by = c("AgeType" = "detailedNames"))
# 
# names(joined_data)<-gsub(" ", "_", names(joined_data))





# updated_data <- joined_data %>%
#   mutate(ageType = coalesce(keyID, AgeType)) %>%
#   select(-keyID, -AgeType)
# scaleDataRaw<-updated_data
names(scaleDataRaw)<-gsub(" ", "_", names(scaleDataRaw))
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
  

scaleDataRaw <- scaleDataRaw |> 
  select(-c(start_time, end_time)) |> 
  rename(originalOrder = original_order, scaleBookNo = scale_book_Num, scaleNo = scale_Nums,
         toAge = bcpal_to_age_y_n__filtered_by_y_or_n, gpsPoint = gps_point, secondsElectricity = seconds_electricity)

  


col_types<-get_col_types(scaleDataRaw)


sur_col_types_sql <- col_types |> 
  dplyr::mutate(key_status = case_when(
    col_name %in% c("originalOrder, AgeType") ~ "KEY",
    TRUE ~ ""
  )) |> 
  dplyr::reframe(a = paste0(col_name, " ", stringr::str_to_upper(sqlite_type), " ", key_status))

sql = paste0("CREATE TABLE IF NOT EXISTS scaleRawTable (
       ",paste0(sur_col_types_sql$a,collapse = ",\n"),
             ",\nPRIMARY KEY (originalOrder))")
dbExecute(con, sql)
dbWriteTable(conn = con, "scaleRawTable", scaleDataRaw, row.names = F, append = T)
dbListTables(con)
###########################################################################################



names(scaleNames)[2]<-"ageType"

scaleNames

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



scale500<- scale500 |> 
  mutate(date = as.character(date)) |> 
  rename(acousticTagNo = AcousticTag, pitTagNo = Pittag, scaleBookNo = ScaleBookNo, scaleNo = ScaleNos, toAge = BCPALToAgeYN,
         endTime = EndTime, startTime = StartTime, secondsElectricity = SecondsElectricity, maturity = Maturity, method = Method, 
         recap = Recap, mort = Mort, comments = Comments, rand = Rand)

col_types<-get_col_types(scale500)

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
