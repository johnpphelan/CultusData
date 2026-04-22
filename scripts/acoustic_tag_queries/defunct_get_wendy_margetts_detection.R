library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
library(ggplot2)

root_loc <- "Z:/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/DATA FILES Cultus SMB/Acoustic telemetry/"


wendy_detec<-read.csv(paste0(root_loc,"Acoustic tag ID and deployments/Detection Data/R_All_Jan11_Wendy-Margetts.csv"))


str(wendy_detec)

other_detec<-read.csv(paste0(root_loc,"all_receiver_data.csv"))

str(other_detec)

wendy_detec <- wendy_detec |> 
  rename(time = Date.and.Time..UTC., receiver = Receiver, transmitter = Transmitter, transmitterName = Transmitter.Name,
         transmitterSerial = Transmitter.Serial, sensorValue = Sensor.Value, sensorUnit = Sensor.Unit, 
         stationName = Station.Name, latitude = Latitude, longitude = Longitude, transmitterType = Transmitter.Type,
         sensorPrecision = Sensor.Precision) 

wendy_detec <- wendy_detec |> 
  mutate(across(everything(), as.character))

other_detec <- other_detec |> 
  mutate(across(everything(), as.character))

other_detec<-other_detec |> 
  rename(sensorUnit = Sensor.Unit)

all_data = bind_rows(wendy_detec, other_detec)

str(all_data)

all_na_cols <- sapply(all_data, function(x) all(is.na(x)))

names(all_na_cols[all_na_cols])

all_data<- all_data |> 
  select(-sensorPrecision)
