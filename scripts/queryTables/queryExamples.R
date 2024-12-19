library(sqldf)
library(dplyr)
#source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath)


print(dbGetInfo(con))
print(dbListObjects(con))

query <- "PRAGMA table_info();"
result <- dbSendQuery(con, query)
column_names <- fetch(result, n = -1)
column_names

query <- "PRAGMA table_info(surveyData);"
result <- dbSendQuery(con, query)
column_names <- fetch(result, n = -1)
column_names
dbClearResult(result)



query <- "SELECT * FROM surveyData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)
df
dbClearResult(result)

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

query<- "SELECT time, numberAnglers, numberRods, personHoursFished, vessel 
          RANK() OVER (ORDER BY personHoursFished DESC) as ranking
          FROM fishingDetails
          ORDER BY ranking
          "

result<-dbSendQuery(con, query)
fishingOrder <- fetch(result, n = -1)
fishingOrder
dbClearResult(result)

tempTable<- "CREATE TEMPORARY TABLE ranked_fishing_details AS
              SELECT time, numberAnglers, surveyNumber, numberRods, personHoursFished, vessel,
                RANK() OVER (ORDER BY personHoursFished DESC) AS ranking
                  FROM fishingDetails 
                    ORDER BY ranking"
dbExecute(con, tempTable)

query<- "SELECT fishCatch.*, ranked_fishing_details.ranking, ranked_fishing_details.vessel, ranked_fishing_details.personHoursFished
                    FROM fishCatch
                      JOIN ranked_fishing_details ON fishCatch.time = ranked_fishing_details.time
                        ORDER BY ranking"
result<-dbSendQuery(con, query)
fishCatchOrder <- fetch(result, n = -1)
str(fishCatchOrder)
dbClearResult(result)

library(ggplot2)
library(data.table)
fishCatchOrder$ranking


ggplot(data=fishCatchOrder, aes(x = as.numeric(personHoursFished), y = as.numeric(totFishCaught))) +
  geom_point()+
  geom_line()


write.csv(fishCatchOrder, "./output/csv/fishCatchOrder.csv", row.names = FALSE)



query<-"SELECT surveydata.*, anglerInfo.*
        FROM surveyData 
        JOIN  anglerInfo ON surveyData.date = anglerInfo.date AND surveyData.surveyNumber = anglerInfo.surveyNumber
                  WHERE DATE(surveyData.time) BETWEEN '2023-07-01' and '2023-07-31'"

query<-"SELECT abundanceCapture.*, scaleRawTable.* 
        FROM abundanceCapture
          JOIN scaleRawTable ON abundanceCapture.ScaleBookNo = ScaleRawTable.ScaleBookNo AND
                abundanceCapture.ScaleNos = ScaleRawTable.ScaleNos
            WHERE scaleRawTable.result IS NOT NULL"
result<-dbSendQuery(con, query)
scaleDetails <- fetch(result, n = -1)
scaleDetails
str(fishCatchOrder)
dbClearResult(result)
# 
# 
# dbGetQuery(con, "DESCRIBE surveyData")
# 
# dbquery<-"SELECT * FROM anglerInfo, catchDetails where anglerInfo."
# 
# # dbExecute(con, queryAll)
# # dbSendQuery(con, queryAll)
