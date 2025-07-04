library(sqldf)
library(dplyr)
#source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath)



print(dbGetInfo(con))
print(dbListObjects(con))

query <- "PRAGMA table_info(tagEvent);"
result <- dbSendQuery(con, query)
column_names <- fetch(result, n = -1)
column_names
dbClearResult(result)

query <- "SELECT * FROM tagEvent"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

tags2023<-df


