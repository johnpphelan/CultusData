library(sqldf)
library(dplyr)
#source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")



db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath)

query<- "SELECT * FROM springMarking"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
springMarking<-fetch(result, -1)
dbClearResult(result)

query<- "SELECT * FROM tagData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
tagData<-fetch(result, -1)
dbClearResult(result)

query<- "SELECT * FROM recapture"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
recap<-fetch(result, -1)
dbClearResult(result)

dbDisconnect(con)

head(recap)
head(tagData)
head(springMarking)

springMatch<- merge(springMarking, recap, by = "pitTagNo") |> 
  filter(!is.na(pitTagNo))

write.csv(springMatch, "output/springMatch.csv")

springMatch |>  group_by(pitTagNo) |> 
  filter(!is.na(pitTagNo)) |> 
  summarise(count = n()) |> 
  arrange(desc(count))
