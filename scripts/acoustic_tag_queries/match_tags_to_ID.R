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

### loading data
receiver_info<-read_excel("data/Acoustic_tags/2023 receiver locations and deployment information.xlsx")
transmitter_data_thomspon_one<-read_excel("data/Acoustic_tags/29375 THOMPSON RIVERS UNIVERSITY.xls", sheet = "Other Tags")
transmitter_data_thomspon_two<-read_excel("data/Acoustic_tags/29375 THOMPSON RIVERS UNIVERSITY.xls", sheet = "Sensor Tags")
tags2023<-read_excel("data/Acoustic_tags/36148 MOE04 - BC MINISTRY OF ENVIRONMENT.xls", sheet = "Sensor Tags")
tagsheet2024<-read_excel("data/Acoustic_tags/TagSheet_38416_2024051JPP.xls", sheet = "Tag Summary")



query <- "SELECT * FROM tagData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)

dbDisconnect(con)

names(df)
unique(df$receiver) # the things floating in the water
unique(df$transmitter) # the things in the fish
unique(df$transmitterSerial) # nothing
unique(df$transmitterType) # nothing


#Get only unique receivers and their unique receiver names
receiver_data <- df |> 
  distinct(receiver, receiver_name)


# 1. Receiver data

# joined what we have in the database with what was supplied about the receivers based on name
joined_receivers <- receiver_info |> 
  rename(receiver_name = `receiver label number`) |> 
  mutate(receiver_name = as.character(receiver_name)) |> 
  full_join(receiver_data, by = "receiver_name")

# now for the transmitters, get the data from the table and then compare with the various sources
transmitter_data<- df |> 
  distinct(transmitter)


# 2. Thompson one data

# Find the location of the cell that contains "Date"
pos <- which(transmitter_data_thomspon_one == "Date:", arr.ind = TRUE)
date_value <- transmitter_data_thomspon_one[pos[1], pos[2]+2]
real_date <- as.Date(as.numeric(date_value), origin = "1899-12-30")
transmitter_data_thomspon_one<-transmitter_data_thomspon_one[3:nrow(transmitter_data_thomspon_one), 8:ncol(transmitter_data_thomspon_one)]
transmitter_data_thomspon_one<-transmitter_data_thomspon_one[complete.cases(transmitter_data_thomspon_one),]
names(transmitter_data_thomspon_one)<-transmitter_data_thomspon_one[1,]
transmitter_data_thomspon_one<-transmitter_data_thomspon_one[-1,]
transmitter_data_thomspon_one$date=real_date


#only keep what was joined in this case
# joined_transmitters <- transmitter_data_thomspon_one |> 
#   rename(transmitter = `VUE Tag ID
# (Freq-Space-ID)`) |> 
#   mutate(transmitter = as.character(transmitter)) |> 
#   full_join(transmitter_data, by = "transmitter")

#match the transmitters we have in the database, with what we now have info for
transmitter_data_thomspon_one <-transmitter_data_thomspon_one |> 
  rename(transmitter = `VUE Tag ID\n(Freq-Space-ID)`)
joined_transmitters<- transmitter_data |>
  full_join(transmitter_data_thomspon_one, by = "transmitter")
  
#transmitters1<-joined_transmitters[complete.cases(joined_transmitters),]
# transmitters1<-transmitters1 |> 
#   select(c(`Tag Family`, `Serial No.`, transmitter, date)) |> 
#   rename(tagFamily = `Tag Family`, serialNo = `Serial No.`)

#next sheet  
pos <- which(transmitter_data_thomspon_two == "Date:", arr.ind = TRUE)
date_value <- transmitter_data_thomspon_two[pos[1], pos[2]+2]
real_date <- as.Date(as.numeric(date_value), origin = "1899-12-30")
transmitter_data_thompson_two<-transmitter_data_thomspon_two[3:nrow(transmitter_data_thomspon_two), 8:ncol(transmitter_data_thomspon_two)]
names(transmitter_data_thompson_two)<-transmitter_data_thompson_two[1,]
transmitter_data_thompson_two<-transmitter_data_thompson_two[-1,]
transmitter_data_thompson_two$date=real_date

#only keep what was joined in this case
# joined_transmitters <- transmitter_data_thompson_two |>
#   rename(transmitter = `VUE Tag ID\n(Freq-Space-ID)`) |> 
#   mutate(transmitter = as.character(transmitter)) |> 
#   full_join(transmitter_data, by = "transmitter")

# joined_transmitters<-joined_transmitters[complete.cases(joined_transmitters),]
# transmitters2<- joined_transmitters |> 
#   select(c(`Tag Family`, `Serial No.`, transmitter, date)) |> 
#   rename(tagFamily = `Tag Family`, serialNo = `Serial No.`)
# 
# final_transmitters<-rbind(transmitters1,transmitters2)
transmitter_data_thompson_two <-transmitter_data_thompson_two |> 
  rename(transmitter = `VUE Tag ID\n(Freq-Space-ID)`)
joined_transmitters<- transmitter_data |>
  full_join(transmitter_data_thomspon_one, by = "transmitter")



# 3. Tag data

pos <- which(tags2023 == "Date:", arr.ind = TRUE)
date_value <- tags2023[pos[1], pos[2]+2]
real_date <- as.Date(as.numeric(date_value), origin = "1899-12-30")

tags2023<-tags2023[3:nrow(tags2023), 8:ncol(tags2023)]
names(tags2023)<-tags2023[1,]
tags2023<-tags2023[-1,]
tags2023$date=real_date

names(tags2023)[1]<-"Missing"

#only keep what was joined in this case
joined_transmitters <- tags2023 |> 
  rename(transmitter = `VUE Tag ID\n(Freq-Space-ID)`) |> 
  mutate(transmitter = as.character(transmitter)) |> 
  full_join(transmitter_data, by = "transmitter") |> 
  select(-Missing)

joined_transmitters<-joined_transmitters[complete.cases(joined_transmitters),]
transmitters3<- joined_transmitters |> 
  select(c(`Tag Family`, `Serial No.`, transmitter, date)) |> 
  rename(tagFamily = `Tag Family`, serialNo = `Serial No.`)
final_transmitters<-rbind(final_transmitters,transmitters3)

# 4. Tagsheet 2024 data






write.csv(final_transmitters, "data/Acoustic_tags/TransmitterSalesValues.csv", row.names = FALSE)
