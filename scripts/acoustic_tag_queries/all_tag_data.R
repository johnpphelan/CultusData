library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
library(DBI)
library(lubridate)

db_filepath <- "output/CultusData.sqlite"
con <- dbConnect(RSQLite::SQLite(), db_filepath, extended_types = TRUE)

root_loc <- "Z:/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/DATA FILES Cultus SMB/Acoustic telemetry/VR2 acoustic receiver data"

csv_files <- list.files(root_loc, "\\.csv$",  full.names = TRUE)
xl_files  <- list.files(root_loc, "\\.xlsx$", full.names = TRUE)
zip_files <- list.files(root_loc, "\\.zip$",   full.names = TRUE, recursive = TRUE)

std_names <- function(df) {
  df |>
    rename_with(~ gsub("\\s+", ".", .x)) |>
    rename_with(~ gsub("\\.+", ".", .x))
}

parse_utc <- function(x) {
  parse_date_time(
    str_trim(na_if(na_if(as.character(x), ""), "NA")),
    orders = c(
      "Ymd HMS", "Ymd HM", "Ymd",
      "mdY HMS", "mdY HM", "mdY"
    ),
    tz = "UTC"
  )
}

read_any <- function(f) {
  ext <- tolower(tools::file_ext(f))
  if (ext == "csv") {
    read_csv(f, show_col_types = FALSE)
  } else {
    read_excel(f)
  }
}

csv_data <- map(csv_files, read_csv, show_col_types = FALSE)


xl_data <- xl_files %>%
  map(~ {
    sheets <- excel_sheets(.x)
    
    map(sheets, function(s) {
      read_excel(path = .x, sheet = s)
    })
  }) %>%
  flatten() %>%
  bind_rows()



# unzip telemetry archives
tmp_dir <- file.path(tempdir(), "telemetry_unzip")
dir.create(tmp_dir, showWarnings = FALSE)
map(zip_files, unzip, exdir = tmp_dir)

zip_data <- list.files(tmp_dir, "\\.(csv|xls|xlsx)$",
                       full.names = TRUE, recursive = TRUE) |>
  map(read_any)


csv_all <- csv_data |>
  lapply(function(df) {
    df |>
      std_names() |>
      mutate(across(everything(), as.character))
  }) |>
  bind_rows(.id = "source_file") 
  
csv_all <- csv_all |>
  mutate(
    needs_shift =
      !is.na(Latitude) &
      Latitude != "" &
      is.na(suppressWarnings(as.numeric(Latitude)))
  )

csv_all <- csv_all |>
  mutate(
    `Station.Name` = if_else(
      needs_shift,
      str_trim(paste(`Station.Name`, Latitude)),
      `Station.Name`
    ),
    
    Latitude = if_else(needs_shift, Longitude, Latitude),
    Longitude = if_else(needs_shift, `Transmitter.Type`, Longitude),
    `Transmitter.Type` = if_else(needs_shift, `Sensor.Precision`, `Transmitter.Type`),
    `Sensor.Precision` = if_else(needs_shift, `Date(UTC)`, `Sensor.Precision`),
    `Date(UTC)` = if_else(needs_shift, `Time(UTC)`, `Date(UTC)`),
    `Time(UTC)` = if_else(needs_shift, `Date(Local)`, `Time(UTC)`),
    `Date(Local)` = if_else(needs_shift, `Time(Local)`, `Date(Local)`),
    `Time(Local)` = if_else(needs_shift, NA_character_, `Time(Local)`)
  )


### lets see if we can fill in some dates
csv_all = csv_all |> 
  mutate(`Date.and.Time.(UTC)` = coalesce(`Date.and.Time.(UTC)`, `Date(UTC)`))



xl_all <- bind_rows(xl_data, .id = "source_file") |>
  std_names() |>
  mutate(across(everything(), as.character))



zip_all <- zip_data |>
  lapply(function(df) {
    df |>
      std_names() |>
      mutate(across(everything(), as.character))
  }) |>
  bind_rows(.id = "source_file")


#### Add wendy margetts data here

wendy_detec<-read.csv(paste0(dirname(root_loc),"/Acoustic tag ID and deployments/Detection Data/R_All_Jan11_Wendy-Margetts.csv"))

wendy_detec <- wendy_detec |> 
  rename(time = Date.and.Time..UTC., receiver = Receiver, transmitter = Transmitter, transmitterName = Transmitter.Name,
         transmitterSerial = Transmitter.Serial, sensorValue = Sensor.Value, sensorUnit = Sensor.Unit, 
         stationName = Station.Name, latitude = Latitude, longitude = Longitude, transmitterType = Transmitter.Type,
         sensorPrecision = Sensor.Precision) 

wendy_detec <- wendy_detec |> 
  mutate(across(everything(), as.character))

#---------------------
# Wendys other data

wendy_detec_2<- read_csv(paste0(dirname(root_loc),"/Acoustic tag ID and deployments/Wendy Margetts Data sheets/Copy of R_All_Jan11.csv"))

wendy_detec_2 <- wendy_detec_2 |> 
  janitor::clean_names()

wendy_detec_2 <- wendy_detec_2 |>
  rename(
    time               = date_and_time_utc,
    receiver           = receiver,
    transmitter        = transmitter,
    transmitterName    = transmitter_name,
    transmitterSerial  = transmitter_serial,
    sensorValue        = sensor_value,
    sensorUnit         = sensor_unit,
    stationName        = station_name,
    latitude           = latitude,
    longitude          = longitude,
    transmitterType    = transmitter_type,
    sensorPrecision    = sensor_precision
  )





all_data <- bind_rows(
  csv_all,
  xl_all,
  zip_all,
)

all_data <- all_data |>
  mutate(
    `Transmitter.Name`   = coalesce(`Transmitter.Name`,   TransmitterName),
    `Transmitter.Serial` = coalesce(`Transmitter.Serial`, TransmitterSerial),
    
    `Sensor.Value` = coalesce(`Sensor.Value`, SensorValue),
    `Sensor.Unit`  = coalesce(`Sensor.Unit`,  SensorUnit),
    
    `Station.Name` = coalesce(`Station.Name`, StationName),
    
    `Transmitter.Type` = coalesce(`Transmitter.Type`, TransmitterType),
    `Sensor.Precision` = coalesce(`Sensor.Precision`, SensorPrecision)
  ) |>
  select(
    -TransmitterName,
    -TransmitterSerial,
    -SensorValue,
    -SensorUnit,
    -StationName,
    -TransmitterType,
    -SensorPrecision
  )

all_data <- all_data |> 
  select(-c(needs_shift, source_file, `Date(UTC)`, `Time(UTC)`, `Date(Local)`, `Time(Local)`))


all_data = all_data |> 
  mutate(date = as.Date(`Date.and.Time.(UTC)`))



all_data = all_data |> 
  rename(
   time = `Date.and.Time.(UTC)`, receiver = Receiver, transmitter = Transmitter,
   transmitterName = Transmitter.Name, transmitterSerial = Transmitter.Serial,
   sensorValue = Sensor.Value, stationName = Station.Name, latitude = Latitude,
   longitude = Longitude, transmitterType = Transmitter.Type, sensorPrecision = Sensor.Precision
  )



## Drop columns that are only NA
cols_to_drop=names(all_data)[colSums(!is.na(all_data)) == 0]

all_data<- all_data |> 
  select(-all_of(cols_to_drop))

#### fix some column names
all_data <- all_data |> 
  rename(
    sensorUnit = Sensor.Unit
  )

wendy_detec_2<- wendy_detec_2 |> 
  mutate(across(everything(), as.character))

#### checking before merging
all_data <- all_data %>% mutate(time_parsed = parse_utc(time))
wendy_detec <- wendy_detec %>% mutate(time_parsed = parse_utc(time))
wendy_detec_2 <-wendy_detec_2 %>% mutate(time_parsed = parse_utc(time))
exact_match <- inner_join(
  all_data, 
  wendy_detec, 
  by = "time_parsed", 
  suffix = c("_all", "_wendy"),
  relationship = "many-to-many"
)
nrow(exact_match)

exact_match <- inner_join(
  all_data, 
  wendy_detec_2, 
  by = "time_parsed", 
  suffix = c("_all", "_wendy"),
  relationship = "many-to-many"
)
nrow(exact_match)



wendy_detec_2_new <- wendy_detec_2 |>
  anti_join(
    all_data |> distinct(time_parsed),
    by = "time_parsed"
  )



all_data_updated <- bind_rows(all_data, wendy_detec_2_new)

## Join on wendy margetts data
all_data_final<-bind_rows(all_data, wendy_detec)


all_data_final = all_data_final |> 
  mutate(time = parse_utc(time))
  


## Drop columns that are only NA
cols_to_drop=names(all_data_final)[colSums(!is.na(all_data_final)) == 0]

all_data_final<- all_data_final |> 
  select(-all_of(cols_to_drop))

## make sure there are unique entries here
all_data_final = all_data_final |> distinct()

# write.csv(
#   all_data,
#   file = file.path(dirname(root_loc), "all_receiver_data.csv"),
#   row.names = FALSE
# )



p1 <- ggplot(all_data, aes(x = as.Date(time))) +
  geom_histogram(bins = 50) +
  scale_x_date(
    date_breaks = "3 month",
    date_labels = "%B %Y"
  ) +
  labs(
    title = "Distribution of Date-Time Values: csv / excel / zipped",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave("./images/csv_excel_zip_date_range.png", p1, width = 8, height = 6)

p2<-ggplot(wendy_detec, aes(x = as.Date(time))) +
  geom_histogram(bins = 50) +
  scale_x_date(
    date_breaks = "3 month",
    date_labels = "%B %Y"
  ) +
  labs(
    title = "Distribution of Date-Time Values: Wendy",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave("./images/Margetts_date_range.png", p2, width = 8, height = 6)

p3 <-ggplot(all_data_final, aes(x = as.Date(time))) +
  geom_histogram(bins = 50) +
  scale_x_date(
    date_breaks = "3 month",
    date_labels = "%B %Y"
  ) +
  labs(
    title = "Distribution of Date-Time Values: csv / excel / zipped",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave("./images/Full_data_date_range.png", p3, width = 8, height = 6)
