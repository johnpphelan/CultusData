library(sqldf)
library(dplyr)
#source("scripts/get_data/get_creel.R")
source("scripts/utils/col_types_f.R")



db_filepath = "scripts/shiny/www/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath)

update_command <- "UPDATE nestRaw SET lifeStage = 'alevin' WHERE lifeStage = 'alvein';"
dbExecute(con, update_command)

update_command2 <- "UPDATE nestRaw SET lifeStage = 'eggs' WHERE lifeStage = 'egg';"
dbExecute(con, update_command2)

