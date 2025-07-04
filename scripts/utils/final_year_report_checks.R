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

unique(df$activityCompleted)

not_destroyed <- df[grepl("Nest not destroyed", df$activityCompleted), ]
destroyed <- df[grepl("Nest destroyed", df$activityCompleted), ]


not_destroyed <- not_destroyed |>  mutate(status = "Not Destroyed")
destroyed <- destroyed |>  mutate(status = "Destroyed")
combined <- bind_rows(not_destroyed, destroyed)


depth_destroyed<-ggplot() +
  geom_histogram(data = not_destroyed, aes(x = depth, fill = "Not Destroyed"), alpha = 0.5, bins = 30) +
  geom_histogram(data = destroyed, aes(x = depth, fill = "Destroyed"), alpha = 0.5, bins = 30) +
  scale_fill_manual(name = "Nest Status", values = c("Not Destroyed" = "darkblue", "Destroyed" = "orange")) +
  ggtitle("Depth of nests") +
  theme_minimal()

ggsave("images/nest_depth_histogram.png",depth_destroyed, width = 8, height = 6)

substrate_destroyed<-ggplot()+
  geom_bar(data = combined, aes(x = Substrate, fill = status), alpha = 0.5, position = "identity") +
  scale_fill_manual(name = "Nest Status", values = c("Not Destroyed" = "darkblue", "Destroyed" = "orange")) +
  labs(
    title = "Substrate of Nests",
    x = "Substrate",
    y = "Count"
  )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("images/nest_substrate_bar_chart.png",substrate_destroyed, width = 8, height = 6)

combined$reason <- case_when(
  grepl("couldn.?t locate", combined$comments, ignore.case = TRUE) ~ "Couldn't Locate",
  grepl("old nest", combined$comments, ignore.case = TRUE) ~ "Old Nest",
  grepl("no fry captured", combined$activityCompleted, ignore.case = TRUE) ~ "No Fry Captured",
  TRUE ~ "Other"
)

ggplot(combined, aes(x = "", fill = reason)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  labs(
    title = "Inferred Reasons for Not Destroying Nests",
    fill = "Reason"
  ) +
  theme_void()

library(tm)
library(wordcloud)
library(RColorBrewer)
comments_text <- Corpus(VectorSource(combined$comments))
# Clean the text
comments_text <- comments_text %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)
# Create a term-document matrix
tdm <- TermDocumentMatrix(comments_text)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)
# Plot the word cloud
set.seed(123)
# Set file name and size
png("./images/wordcloud_comments.png", width = 800, height = 600)

# Plot the word cloud
wordcloud(words = df$word,
          freq = df$freq,
          min.freq = 2,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

# Close the PNG device to write the file
dev.off()
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

df

unique(df$answer)

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
