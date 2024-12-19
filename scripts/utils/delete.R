library(data.table)
library(dplyr)
library(stringr)
library(reshape2)
library(DBI)
library(RSQLite)
source("scripts/utils/fix_col_names_f.R")
source("scripts/utils/col_types_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)
print(dbListObjects(con))


droptable<-"DROP TABLE IF EXISTS fishingDetails"
dbExecute(con, droptable)


dropping<- "DELETE FROM weatherDetails WHERE time = '2024-05-25 07:09:00'"
dbExecute(con, dropping)


dropmult<- "DELETE FROM iceData WHERE date IN (SELECT date FROM iceData ORDER BY date DESC LIMIT 3)"
dbExecute(con, dropmult)

deleteYear<- "DELETE FROM fishingDetails WHERE time > '2024-01-01'"
dbExecute(con, deleteYear)
