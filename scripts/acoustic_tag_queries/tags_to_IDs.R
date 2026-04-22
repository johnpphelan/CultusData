library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
library(ggplot2)

## first we will get the tag events
root_loc <- "Z:/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/DATA FILES Cultus SMB/Acoustic telemetry/"

tag_events<-read.xlsx(paste0(root_loc,"Acoustic tag ID and deployments/Tag Deployment and Removal/Acoustic-Tag_Deployment_and_Removal-Log_all-years.xlsx"))

tag_events <- tag_events |> 
  janitor::clean_names() |> 
  dplyr::mutate(
    tagging_date = as.Date(tagging_date, origin = "1899-12-30")
  )

tag_events<- tag_events |> 
  mutate(sex = coalesce(sex, sex_2)) |> 
  select(-sex_2)

tag_events <- tag_events |> 
  rename(taggingDate = tagging_date, acousticTagIDCode = acoustic_tag_id_code, tagModel = tag_model, pitTagID = pit_tag_id,
         floyTagNumber = floy_tag_number, lengthAtTagging = length_at_tagging_mm, weight = weight_g, removalDate = removal_date,
         lengthAtRemoval = length_at_removal_mm, weightRemoval = weight_at_removal_g, otolithScaleID = otolith_scale_id, 
         removalMethod = removal_method, tagRecovered = tag_recovered 
         )

### get the receiver data

receiver_data<-read.csv(paste0(root_loc, "all_receiver_data.csv"))


receiver_data_clean <- receiver_data %>%
  mutate(
    acousticTagIDCode = as.numeric(sub(".*-", "", transmitter))
  )

receiver_data_clean <- receiver_data_clean %>%
  mutate(
    date_time = parse_date_time(
      time,
      orders = c(
        "ymd HMS", "ymd HM",
        "mdy HMS", "mdy HM",
        "dmy HMS", "dmy HM"
      ),
      tz = "UTC"
    )
  )



## the receiver data has information about the tag ID in "transmitter", which are full ID codes. These correspond to 
## acouticTagIDCode in the tag_events data. There are numbers which are the last digits in transmitter 



receiver_latest <- receiver_data_clean %>%
  group_by(acousticTagIDCode) %>%
  summarise(
    most_recent_receiver_date = {
      x <- date_time
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    },
    .groups = "drop"
  )


tag_events <- tag_events %>%
  left_join(
    receiver_latest,
    by = "acousticTagIDCode"
  )

# for those that were NA after this, we could check Wendy's data to see if those tags are present there.

  wendy_detec<-read.csv(paste0(dirname(root_loc),"/Acoustic tag ID and deployments/Detection Data/R_All_Jan11_Wendy-Margetts.csv"))

wendy_detec <- wendy_detec |> 
  rename(time = Date.and.Time..UTC., receiver = Receiver, transmitter = Transmitter, transmitterName = Transmitter.Name,
         transmitterSerial = Transmitter.Serial, sensorValue = Sensor.Value, sensorUnit = Sensor.Unit, 
         stationName = Station.Name, latitude = Latitude, longitude = Longitude, transmitterType = Transmitter.Type,
         sensorPrecision = Sensor.Precision) 

wendy_detec <- wendy_detec |> 
  mutate(across(everything(), as.character))


### now the same matching process as above





write.xlsx(tag_events, paste0(root_loc,"most_recent_tag_detection.xlsx"))
