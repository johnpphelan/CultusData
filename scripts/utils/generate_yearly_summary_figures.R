library(tidyverse)
library(DBI)
library(RSQLite)
library(sf)
library(osmdata)
library(knitr)
library(kableExtra)
library(patchwork)
library(bcdata)


db_filepath = "scripts/shiny/www/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

DBI::dbListTables(con)


query <- "SELECT * FROM creelFish"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
creelFishDetails<-fetch(result, -1)
dbClearResult(result)

query <- "SELECT * FROM creelMain"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
creelMain<-fetch(result, -1)
dbClearResult(result)

query <- "SELECT * FROM creelFisherDemography"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
creelFisherDemography<-fetch(result, -1)
dbClearResult(result)


query <- "SELECT * FROM creelSurveyAnswers"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
creelSurveyAnswers<-fetch(result, -1)
dbClearResult(result)

query <- "SELECT * FROM creelSurveyQuestions"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
creelSurveyQuestions<-fetch(result, -1)
dbClearResult(result)

creelMain <- creelMain |> 
  filter(date > "2024-12-31")

creelMain <- creelMain %>%
  mutate(
    # extract the start and end times using regex
    start_time = str_extract(shift, "(?<=:)\\s*\\d{1,2}:\\d{2}") %>% str_trim(),
    end_time   = str_extract(shift, "(?<=-)\\d{1,2}:\\d{2}") %>% str_trim(),
    
    # convert to times (hms objects)
    start_time = hm(start_time),
    end_time   = hm(end_time),
    
    # compute duration in hours
    hours_worked = as.numeric(end_time - start_time, units = "hours")
  )

ggplot(creelMain, aes(x = as.integer(hours_worked))) +  # Ensure integer values
  geom_bar(fill = "orange", alpha = 0.6) + 
  scale_x_continuous(breaks = seq(min(creelMain$hours_worked, na.rm = TRUE), 
                                  max(creelMain$hours_worked, na.rm = TRUE), 
                                  by = 1)) +  # Ensure each integer has a tick mark
  theme_minimal() + 
  labs(title = "Distribution of Hours Worked per Shift", x = "Hours Worked", y = "Count")

p1<-creelMain %>%
  group_by(site) %>%
  summarise(days_sampled = n_distinct(date)) %>%
  ggplot(aes(x = reorder(site, -days_sampled), y = days_sampled, fill = site)) +
  geom_col() +
  geom_text(aes(label = days_sampled), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "Site",
    y = "Number of Days Sampled"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("./output/2025_creel_sites.jpg", p1, height = 6, width = 10)

creelFishDetails <- creelFishDetails |> 
  filter(date > "2025-01-01",
         spp == "Smallmouth Bass") |> 
  mutate(length_mm = as.numeric(length_mm),
         weight_g = as.numeric(weight_g))

p1<-ggplot(creelFishDetails, aes(x = length_mm, y = weight_g)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +   # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +  # lm line
  labs(
    x = "Length (mm)",
    y = "Weight (g)"
  ) +
  theme_minimal(base_size = 14)

ggsave("./output/2025_length_weight.jpg", p1, height = 6, width = 10)

names(creelSurveyAnswers)
creelSurveyQuestions

preferredspp<-creelSurveyAnswers |> 
  filter(Question == 3) |> 
  filter(date > "2025-01-01")

preferred_clean <- preferredspp %>%
  filter(!is.na(Answer)) %>%
  mutate(
    # Normalize capitalization
    Answer = str_to_lower(Answer),
    
    # Map to main species categories
    Species = case_when(
      str_detect(Answer, "smb|smallmouth") ~ "Smallmouth Bass",
      str_detect(Answer, "lmb|large mouth|largemouth") ~ "Largemouth Bass",
      str_detect(Answer, "trout") ~ "Trout",
      str_detect(Answer, "carp") ~ "Carp",
      str_detect(Answer, "sucker|chub|minnow") ~ "Suckers / Minnows",
      str_detect(Answer, "none|anything|whatever") ~ "Any / No Preference",
      TRUE ~ "Other"
    )
  )


species_counts <- preferred_clean %>%
  group_by(Species) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

p1<-ggplot(species_counts, aes(x = fct_reorder(Species, count), y = count, fill = Species)) +
  geom_col() +
  geom_text(aes(label = count), hjust = -0.1) +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  labs(
    x = "Species Group",
    y = "Number of Responses"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("./output/2025_preferred_spp.jpg", p1, height = 6, width = 10)


location<-creelSurveyAnswers |> 
  filter(Question == 2) |> 
  filter(date > "2025-01-01")

loc_count<- location |> 
  group_by(Answer) |> 
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

loc_count_clean <- loc_count %>%
  filter(!is.na(Answer)) %>%
  arrange(desc(count))

# Bar plot
p1<-ggplot(loc_count_clean, aes(x = reorder(Answer, -count), y = count, fill = Answer)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.5, size = 5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "Location",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("./output/2025_fishing_location.jpg", p1, height = 6, width = 10)

query <- "SELECT * FROM creelFish"
dbExecute(con = con, query)
result <- dbSendQuery(conn = con, query)
creelFishDetails<-fetch(result, -1)
dbClearResult(result)


dbDisconnect(con)
