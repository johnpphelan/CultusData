library(sqldf)
library(dplyr)
#source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath)


print(dbGetInfo(con))
print(dbListObjects(con))

describe



query <- "SELECT * FROM surveyData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df

query <- "PRAGMA table_info(surveyData);"
result <- dbSendQuery(con, query)
column_names <- fetch(result, n = -1)
column_names

query<-"SELECT DISTINCT ageClass FROM anglerInfo"
result<-dbSendQuery(con, query)
unique_age_classes <- fetch(result, n = -1)
unique_age_classes

query<-"SELECT surveydata.*, anglerInfo.*
        FROM surveyData 
        JOIN  anglerInfo ON surveyData.date = anglerInfo.date AND surveyData.surveyNumber = anglerInfo.surveyNumber
                  WHERE DATE(surveyData.time) BETWEEN '2023-07-01' and '2023-07-31'"
result<-dbSendQuery(con, query)
unique_age_classes <- fetch(result, n = -1)
unique_age_classes


dbGetQuery(con, "DESCRIBE surveyData")

dbquery<-"SELECT * FROM anglerInfo, catchDetails where anglerInfo."

# dbExecute(con, queryAll)
# dbSendQuery(con, queryAll)
