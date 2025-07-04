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

onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/R_projects/CultusData/")
all_creel <- read_xlsx(paste0(onedrive_wd,"/data/Cultus_Lake_Creel_-_All_Query.xlsx"))

# db_filepath <- "scripts/shiny/www/CultusData.sqlite"
# con <- dbConnect(RSQLite::SQLite(), db_filepath)
# 
# # Find all tables with 'creel' in the name
# creel_table_names <- dbGetQuery(con, "
#   SELECT name 
#   FROM sqlite_master 
#   WHERE type='table' AND name LIKE '%creel%'
# ")$name
# 
# # Read each table into a list
# creel_tables_list <- lapply(creel_table_names, function(tbl) {
#   dbReadTable(con, tbl)
# })
# 
# # Name the list elements by their corresponding table names
# names(creel_tables_list) <- creel_table_names
# 
# surveyQuestionsAnswers<- merge(
#   creel_tables_list$creelSurveyAnswers,
#   creel_tables_list$creelSurveyQuestions,
#   by = "questionID"
# )
# 
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

all_creel <- all_creel[,colSums(is.na(all_creel))<nrow(all_creel)]

samples<- all_creel |> 
  filter(date == ("2023-07-02"))

results_2023<- all_creel |> 
  mutate(date = as.Date(date)) |> 
  filter(date < as.Date("2024-01-01")) |> 
  summarize(not_na_count = sum(!is.na(Preferred_Catch_Spp)))

results_2024<- all_creel |> 
  mutate(date = as.Date(date)) |> 
  filter(date >= as.Date("2024-01-01")) |> 
  summarize(not_na_count = sum(!is.na(Preferred_Catch_Spp)))  

custom_theme <- theme(
  axis.title = element_text(face = "bold"),    # Make axis titles bold
  axis.text = element_text(face = "bold"),     # Make axis ticks bold
  legend.title = element_text(face = "bold"),  # Make legend title bold
  legend.text = element_text(face = "bold")    # Make legend labels bold
)

fish_spp2023<- all_creel |> 
  mutate(date = as.Date(date)) |> 
  subset(date < as.Date("2024-01-01")) |> 
  select(date, anglerID, Preferred_Catch_Spp, noAnglers, noRods, personHours, totFishCaught , totRetained) 




species_count_2023 <- fish_spp2023 |> 
  mutate(Preferred_Catch_Spp = str_replace_all(Preferred_Catch_Spp, ";", ",")) |> 
  mutate(
    Preferred_Catch_Spp = str_trim(Preferred_Catch_Spp),  # Remove leading/trailing spaces
    Preferred_Catch_Spp = str_to_lower(Preferred_Catch_Spp),
    Preferred_Catch_Spp = str_replace_all(Preferred_Catch_Spp,  # Correct specific typos
                                   c(
                                     "^trout$" = "trout",
                                     " northern pikeminow" = "northern pikeminnow",
                                     "trout " = "trout",
                                     "pikeminow" = "northern pikeminnow",
                                     "northern northern pikeminnow" = "northern pikeminnow",
                                     "typically fish salmon" = "salmon"))
  ) |> 
  separate_rows(Preferred_Catch_Spp, sep = ",") |>      # Handle multiple species in a single cell
  mutate(Preferred_Catch_Spp = str_trim(Preferred_Catch_Spp)) |> 
  filter(!is.na(Preferred_Catch_Spp) & Preferred_Catch_Spp != "") |> 
  count(Preferred_Catch_Spp, sort = TRUE)

species_23_plot<- species_count_2023 |> arrange(desc(n)) %>% 
  slice_head(n = 6) 

# Generate a vector of distinct colors, one for each species
species_colors <- RColorBrewer::brewer.pal(n = length(unique(species_23_plot$Preferred_Catch_Spp)), name = "Dark2")


ggplot(species_23_plot, aes(x = reorder(Preferred_Catch_Spp, n), y = n, fill = Preferred_Catch_Spp)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Preferred Species: 2023",
    x = "Species",
    y = "Count"
  ) +
  scale_fill_manual(values = species_colors) +  # Use the manually assigned colors
  geom_text(aes(label = n), hjust = -0.2, size = 4, fontface = "bold") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),  # Bold x-axis text and ticks
    axis.text.y = element_text(size = 12, face = "bold"),  # Bold y-axis text and ticks
    axis.title.x = element_text(size = 14, face = "bold"),  # Larger and bold x-axis label
    axis.title.y = element_text(size = 14, face = "bold"),  # Larger and bold y-axis label
    plot.title = element_text(size = 16, face = "bold")
  )


#skimr::skim(all_creel) |> View()
# skimmed <- skimr::skim(all_creel)
# DT::datatable(skimmed)

# library(naniar)
# vis_miss(all_creel)  # Heatmap of missingness

all_creel$date<-lubridate::ymd(all_creel$date)

all_creel<-all_creel |> 
  filter(date > lubridate::ymd("2024-01-01"))

all_creel |> 
  summarise(
    total_caught = sum(totFishCaught, na.rm = TRUE),
    total_retained = sum(totRetained, na.rm = TRUE),
    total_released = total_caught - total_retained
  )

all_creel |> 
  summarise(
    total_caught = sum(totFishCaught, na.rm = TRUE),
    total_retained = sum(totRetained, na.rm = TRUE),
    total_released = total_caught - total_retained
  ) |>
  pivot_longer(everything(), names_to = "category", values_to = "count") |> 
  ggplot(aes(x = category, y = count, fill = category)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("total_caught" = "steelblue", "total_retained" = "darkorange", "total_released" = "forestgreen")) +
  labs(title = "Total Caught, Retained, and Released Fish", x = "", y = "Total Count") +
  theme_minimal()




 all_creel |>
  filter(!is.na(totFishCaught) | !is.na(totRetained)) |> 
  group_by(date) |> 
  summarise(
    total_caught = sum(totFishCaught, na.rm = TRUE),
    total_retained = sum(totRetained, na.rm = TRUE)
  ) |> 
  pivot_longer(cols = c(total_caught, total_retained), names_to = "type", values_to = "count") |> 
  ggplot(aes(x = as.Date(date), y = count, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("total_caught" = "steelblue", "total_retained" = "darkorange")) +
  labs(
    title = "Total Fish Caught and Retained per Day",
    x = "Date",
    y = "Count",
    fill = "Metric"
  ) +
  theme_minimal()

 
 all_creel |> 
   filter(!is.na(site.x)) |> 
   count(site.x, name = "n_surveys") |> 
   arrange(desc(n_surveys))
