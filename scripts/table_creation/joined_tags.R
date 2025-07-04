library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
library(ggplot2)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

query <- "SELECT * FROM tagData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
tagData<-fetch(result, -1)

dbClearResult(result)

query <- "SELECT * FROM tagEvent"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
tagEvent<-fetch(result, -1)

dbClearResult(result)

dbDisconnect(con)

tagJoined<- 
  

