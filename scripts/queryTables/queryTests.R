library(sqldf)
library(dplyr)
#source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath)



print(dbGetInfo(con))
print(dbListObjects(con))

query <- "PRAGMA table_info(creelTables);"
result <- dbSendQuery(con, query)
column_names <- fetch(result, n = -1)
column_names
dbClearResult(result)


query <- "SELECT * FROM creelTables"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

write.csv(df, "./output/csv/creelTablesDetails.csv", row.names = FALSE)


query<-"SELECT surveydata.*, anglerInfo.*
        FROM surveyData 
        JOIN  anglerInfo ON surveyData.date = anglerInfo.date AND surveyData.surveyNumber = anglerInfo.surveyNumber
                  WHERE DATE(surveyData.time) BETWEEN '2023-07-01' and '2023-07-31'"

result<-dbSendQuery(con, query)
anglerInfo <- fetch(result, n = -1)
anglerInfo
str(anglerInfo)
dbClearResult(result)

write.csv(anglerInfo, "./output/csv/anglerInfo.csv", row.names = FALSE)

query <- "SELECT * FROM fishCatch"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

write.csv(df, "./output/csv/surveyQuestions.csv", row.names = FALSE)


query <- "SELECT * FROM surveyAnswers"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

write.csv(df, "./output/csv/surveyAnswers.csv", row.names = FALSE)
