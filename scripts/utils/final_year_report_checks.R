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


print(dbGetInfo(con))
print(dbListObjects(con))

tables <- dbListTables(con)

for (table in tables) {
  
  column_info <- dbGetQuery(con, paste0("PRAGMA table_info(", table, ")"))
  
  cat("\nTable:", table, "\n")
  cat("Columns:\n")
  cat(paste0(" - ", column_info$name), sep = "\n")
}

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

unique(df$answer)

df_summary <- df |> 
  filter(questionID == 3) |> 
  mutate(
    reason = case_when(
      str_detect(answer, regex("SMB|small ?mouth ?bass", ignore_case = TRUE)) ~ "SMB / bass",
      str_detect(answer, regex("fish|fishing|caught|rod", ignore_case = TRUE)) ~ "Fishing",
      str_detect(answer, regex("close|nearby|near|live|local|camping|close ?by|campground", ignore_case = TRUE)) ~ "Proximity",
      str_detect(answer, regex("recreation|swim|BBQ|quiet|vacation", ignore_case = TRUE)) ~ "Recreation / Relaxation",
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


# query <- "SELECT * FROM creelShifts"
# dbExecute(con = con, query)
# #querydelete<-"DROP TABLE surveyData"
# result <- dbSendQuery(conn = con, query)
# df<-fetch(result, -1)
# 
# dbClearResult(result)
# df
# 
# df |> 
#   mutate(date = as.Date(date)) |> 
#   filter(date >= "2024-01-01") |> 
#   distinct(date, surveyNumber) |> 
#   count()
