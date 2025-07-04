library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(DBI)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

db_filepath = "output/CultusData.sqlite"

con<-dbConnect(RSQLite::SQLite(), db_filepath,extended_types = TRUE)

print(dbListObjects(con))

query <- "SELECT * FROM scaleRawTable"
dbExecute(con = con, query)
#querydelete<-"DROP TABLE surveyData"
result <- dbSendQuery(conn = con, query)
scale_ages<-fetch(result, -1)

dbClearResult(result)



summary(scale_ages)

scale_ages<- scale_ages |> 
  filter(date > "2024-01-01")


scale_age_dist<-ggplot(scale_ages, aes(x = scaleAgeJan1) )+ 
  geom_bar(fill = "steelblue")+
  labs(title = "Scale Age Distribution", x = "Scale Age", y = "Count") +
  theme_minimal() 
ggsave("images/scale_age_distribution.png",scale_age_dist, width = 8, height = 6)
 

scale_age_perc <- scale_ages |> 
  count(scaleAgeJan1) |> 
  mutate(percent = n / sum(n))

scale_age_dist_perc <- ggplot(scale_age_perc, aes(x = scaleAgeJan1, y = percent)) + 
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Scale Age Distribution",
    x = "Scale Age ",
    y = "Percent"
  ) +
  theme_minimal()

ggsave("images/scale_age_distribution_percent.png", scale_age_dist_perc, width = 8, height = 6)

scale_age_boxplot <- ggplot(scale_ages, aes(x = factor(scaleAgeJan1), y = length)) + 
  geom_boxplot(fill = "lightblue") +
  labs(title = "Scale Age Boxplot", x = "Scale Age", y = "Length") +
  theme_minimal()
ggsave("images/scale_age_boxplot.png", scale_age_boxplot, width = 8, height = 6)

length_age_sex<-ggplot(scale_ages, aes(
  x = as.numeric(as.character(scaleAgeJan1)),
  y = length,
  color = forcats::fct_explicit_na(factor(sex), na_level = "NA")
)) +
  geom_point() +
  facet_wrap(~sex) +
  scale_color_brewer(palette = "Set1", na.value = "black") +
  labs(
    title = "Length vs. Age by Sex",
    x = "Scale Age",
    y = "Length (mm)",
    color = "Sex"
  )  +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # thick border around each facet
  )
ggsave("images/scale_age_length_scatter.png",length_age_sex, width = 8, height = 6)


ggplot(scale_ages, aes(x = length, fill = sex)) + 
  geom_density(alpha = 0.4)+
  theme_minimal()
ggsave("images/scale_age_length_density.png", width = 8, height = 6)


sale_age_effort<-ggplot(scale_ages, aes(x = date)) + geom_bar(fill ="steelblue")
ggsave("images/scale_age_date_distribution.png",sale_age_effort, width = 8, height = 6)


scale_ages |> 
  count(sex)



