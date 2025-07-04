library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(kableExtra)
library(data.table)
library(lubridate)
library(geosphere)
library(ggmap)
library(sqldf)
source("scripts/utils/col_types_f.R")


db_filepath <- "scripts/shiny/www/CultusData.sqlite"
con <- dbConnect(RSQLite::SQLite(), db_filepath)

# Find all tables with 'creel' in the name
creel_table_names <- dbGetQuery(con, "
  SELECT name
  FROM sqlite_master
  WHERE type='table' AND name LIKE '%creel%'
")$name

# Read each table into a list
creel_tables_list <- lapply(creel_table_names, function(tbl) {
  dbReadTable(con, tbl)
})

# Name the list elements by their corresponding table names
names(creel_tables_list) <- creel_table_names

surveyQuestionsAnswers<- merge(
  creel_tables_list$creelSurveyAnswers,
  creel_tables_list$creelSurveyQuestions,
  by = "questionID"
)

# # Remove the two original tables from the list
# creel_tables_list$creelSurveyAnswers <- NULL
# creel_tables_list$creelSurveyQuestions <- NULL
# 
# 
# # Remove 'time' column from each table in the list, if it exists
# creel_tables_list_cleaned <- lapply(creel_tables_list, function(df) {
#   if ("time" %in% names(df)) {
#     df <- select(df, -time)
#   }
#   df
# })
# 
# # Perform full join across all cleaned tables
# all_creel <- reduce(creel_tables_list_cleaned, full_join)

################################################################
creel_shifts<-creel_tables_list[["creelShifts"]]

creel_weather_shifts <- dplyr::inner_join(
  creel_tables_list[["creelWeather"]] |> select(-time),
  creel_tables_list[["creelShifts"]],
  by = c("date", "surveyNumber")
)

creel_shifts$date<-lubridate::ymd(creel_shifts$date)

creel_shifts<-creel_shifts |> 
  filter(date > lubridate::ymd("2024-01-01"))

creel_weather_shifts <- creel_weather_shifts |>
  mutate(site = case_when(
    site == "Maple Bay" ~ "3: Maple Bay",
    TRUE ~ site
  ))

site_days <- creel_weather_shifts |>
  distinct(site, date) |>
  count(site, name = "days_surveyed") |>
  arrange(desc(days_surveyed))

p1<-ggplot(site_days, aes(x = reorder(site, -days_surveyed), y = days_surveyed, fill = site)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Number of Survey Days per Site",
    x = "Site",
    y = "Survey Days"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Removes the legend
  ) +
  geom_text(aes(label = days_surveyed), vjust = -0.5, size = 4) +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )
ggsave("images/survey_days_per_site.png", plot = p1, width = 10, height = 6)


################################################################
creelFishDetails<-creel_tables_list[["creelFishDetails"]] |> 
  select(-time) |> 
  mutate(
    date = lubridate::ymd(date),
    surveyNumber = as.integer(surveyNumber)
  ) |> 
  filter(date > lubridate::ymd("2024-01-01"))

smb_fish <- creelFishDetails |> 
  filter(Spp == "SMB")

smb_size<-ggplot(smb_fish, aes(x = length, y = weight)) +
  geom_point(color = "tomato", alpha = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "SMB Length vs. Weight", x = "Length (mm)", y = "Weight (g)")+
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )
ggsave("images/smb_length_weight.png", plot = smb_size, width = 10, height = 6)


################################################################
creelFishResults<-creel_tables_list[["creelFishResults"]] |> 
  select(-time) |> 
  mutate(
    date = lubridate::ymd(date),
    surveyNumber = as.integer(surveyNumber))

vessl_summary <- creelFishResults %>%
  filter(!is.na(vessl)) %>%                # Remove NAs
  count(vessl) %>%                         # Count occurrences
  mutate(percent = n / sum(n) * 100)       # Calculate percentage

vessel<-ggplot(vessl_summary, aes(x = reorder(vessl, -percent), y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Percentage of Water Access Methods",
    x = "Access Method",
    y = "Percentage (%)"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percent)), vjust = -0.5) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )
ggsave("images/access_method.png", plot = vessel, width = 10, height = 6)

releaseReason = creelFishResults %>%
  filter(!is.na(releaseReason)) %>%                # Remove NAs
  count(releaseReason) %>%                         # Count occurrences
  mutate(percent = n / sum(n) * 100)       # Calculate percentage


#################################################################
creelDemography<-creel_tables_list[["creelFisherDemography"]] |> 
  select(-time) |> 
  mutate(
    date = lubridate::ymd(date),
    surveyNumber = as.integer(surveyNumber))

# Age Class distribution
creelDemography |> 
  count(AgeClass) |> 
  arrange(desc(n)) |> 
  mutate(percent = n / sum(n) * 100)  

# Residency breakdown
creelDemography |> 
  count(residency) |> 
  arrange(desc(n)) |> 
  mutate(percent = n / sum(n) * 100)

# License period frequency
creelDemography |> 
  count(licensePeriod)|> 
  arrange(desc(n))


ggplot(creelDemography, aes(x = AgeClass)) +
  geom_bar(fill = "#69b3a2") +
  labs(title = "Age Class Distribution", x = "Age Class", y = "Count") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

ggplot(creelDemography, aes(x = residency)) +
  geom_bar(fill = "#404080") +
  labs(title = "Residency of Anglers", x = "Residency", y = "Count") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

ggplot(creelDemography, aes(x = licensePeriod)) +
  geom_bar(fill = "#ffa07a") +
  labs(title = "Fishing License Type", x = "License Period", y = "Count") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

creelDemography |> 
  mutate(date = ymd(date)) |> 
  count(date) |> 
  ggplot(aes(x = date, y = n)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  labs(title = "Number of Anglers Interviewed per Day", x = "Date", y = "Count") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

##################################################################

query <- "SELECT * FROM nestRaw"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)


head(df)

unique(df$activityCompleted)

df<- df |> 
  mutate(date = as.Date(date))

summary(df)

df<- df |> 
  filter(date > "2024-01-01")

names(df)
activities<- df
unique(df$guarding)

#cleaned_activities <- gsub(" ", "", activities)
split_activities <- str_split(activities, fixed(".")) # Use fixed() to treat "." as a literal dot
split_activities_clean <- lapply(split_activities, function(x) x[x != ""])
unique(split_activities)

# Remove leading and trailing whitespace
split_activities <- str_trim(unlist(split_activities))
split_activities <- split_activities[split_activities != ""]
unique(split_activities)

sum(df$activityCompleted == "nest destruction", na.rm = TRUE) +
  sum(grepl("Nest destroyed", df$activityCompleted, ignore.case = TRUE), na.rm = TRUE)

activity_counts <- as.data.frame(table(df$activityCompleted))
ggplot(activity_counts, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Activity Frequency",
    x = "Activity",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
activity_counts <- table(df$activityCompleted)
print(activity_counts)

sum(df$activityCompleted == "Fry captured.", na.rm = TRUE) +
  sum(grepl("Nest destroyed", df$activityCompleted, ignore.case = TRUE), na.rm = TRUE)


activity_counts <- table(split_activities)
print(activity_counts)

activity_counts <- table(split_activities)

# Convert to data frame for ggplot
activity_df <- as.data.frame(activity_counts)
colnames(activity_df) <- c("Activity", "Count")

# Plot the histogram (bar chart)
ggplot(activity_df, aes(x = reorder(Activity, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(
    title = "Frequency of Activity Fragments",
    x = "Activity",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

query <- "SELECT * FROM creelSurveyQuestions"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df_q<-fetch(result, -1)

dbClearResult(result)



query <- "SELECT * FROM creelSurveyAnswers"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)



names(df)

df <- df |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= "2024-01-01")

df |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= "2024-01-01") |> 
  distinct(date, surveyNumber) |> 
  count()

df |> 
  filter(questionID == 3) |> 
  count(answer) |> 
  arrange(desc(n))

df   |> 
  distinct(surveyNumber, date) |> 
  nrow()



unique(df$answer)

#Why_this_Location
df_summary <- df |> 
  filter(questionID == 3) |> 
  mutate(
    reason = case_when(
      str_detect(answer, regex("SMB|small ?mouth ?bass| bass", ignore_case = TRUE)) ~ "SMB / bass",
      str_detect(answer, regex("fish|fishing|caught|rod", ignore_case = TRUE)) ~ "Fishing",
      str_detect(answer, regex("close|nearby|near|live|local|camping|close ?by|campground", ignore_case = TRUE)) ~ "Proximity",
      str_detect(answer, regex("recreation|swim|BBQ|quiet|vacation|camping", ignore_case = TRUE)) ~ "Recreation / Relaxation",
      TRUE ~ "Other"
    )
  ) |> 
  count(reason, sort = TRUE) |> 
  mutate(percentage = round(n / sum(n) * 100, 1))

df_summary

# are you aware that the fish were illegally introduced?
df |> 
  filter(questionID == 7) |> 
  count(answer, sort = TRUE) |> 
  mutate(percentage = round(n / sum(n) * 100, 1))
# are you aware the movement of live fish is illegal?
df |> 
  filter(questionID == 8) |> 
  count(answer, sort = TRUE) |> 
  mutate(percentage = round(n / sum(n) * 100, 1))

#clean drain dry?
df |> 
  filter(questionID == 9) |> 
  count(answer, sort = TRUE) |> 
  mutate(percentage = round(n / sum(n) * 100, 1))

# Are you aware of don't let it loose practices?
df |> 
  filter(questionID == 15) |> 
  count(answer, sort = TRUE) |> 
  mutate(percentage = round(n / sum(n) * 100, 1))



query <- "SELECT * FROM creelFisherDemography"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)

df <- df |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= "2024-01-01")

head(df)

df |> 
  count(AgeClass)

query <- "SELECT * FROM creelFishResults"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)

df <- df |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= "2024-01-01")

head(df)

df |> 
  count(totFishCaught) |> 
  mutate(percentage = round(n / sum(n) * 100, 1))

df |> 
  count(totRetained) |> 
  mutate(percentage = round(n / sum(n) * 100, 1))


#filter out catchVsRetain, if both totFishCaught and totRetained are 0
df <- df |> 
  filter(!(totFishCaught == 0 & totRetained == 0))
#whats the difference
df <- df |> 
  mutate(totFishCaught = replace_na(totFishCaught, 0),
         totRetained = replace_na(totRetained, 0)) |>
  mutate(catchVsRetain = as.numeric(totFishCaught) - as.numeric(totRetained))

#percentages of people who caught more fish than they retained and who retained all the fish they caught
sum(df$catchVsRetain >=1, na.rm = TRUE) / nrow(df) * 100
sum(df$catchVsRetain <= 0, na.rm = TRUE) / nrow(df) * 100


query <- "SELECT * FROM tagData"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)


