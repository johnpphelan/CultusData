library(sqldf)
library(dplyr)
library(tidyr)
source("scripts/get_data/get_spring_marking.R")
source("scripts/utils/col_types_f.R")
source("scripts/utils/fix_col_names_f.R")

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

spring_marking

#### check the contents of spring marking - does this table need foriegn keys?

