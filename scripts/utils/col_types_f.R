get_col_types <- function(df) {
  
  sur_col_types <- data.frame(col_name = names(df),
                              type = as.vector(sapply(df, typeof))) %>%
    mutate(type = case_when(
      col_name == "date" ~ "TEXT",
      col_name == "time" ~ "TEXT",
      TRUE ~ type
    )) %>%
    mutate(sqlite_type = case_when(
      type == "double" ~ "REAL",
      type == "character" ~ "TEXT",
      type == "integer" ~ "INTEGER",
      type == "logical" ~ "INTEGER",
      type == "Date" ~ "TEXT",
      type == "POSIXct" ~ "TEXT",
      TRUE ~ paste0(type)
    ))
  
  sur_col_types
}
