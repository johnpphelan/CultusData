library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)

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

query <- "SELECT * FROM creelSurveyAnswers"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
df<-fetch(result, -1)

dbClearResult(result)
df

df <- df |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= "2024-01-01")

df |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= "2024-01-01") |> 
  distinct(date, surveyNumber) |> 
  count()

unique(df$answer)

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
