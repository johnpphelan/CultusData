get_col_types <- function(df) {
  
  sur_col_types <- data.frame(col_name = names(df),
                              type = as.vector(sapply(df, typeof))) %>%
    mutate(type = case_when(
      col_name == "date" ~ "DATE",
      col_name == "time" ~ "DATETIME",
      TRUE ~ type
    )) %>%
    mutate(sqlite_type = case_when(
      type == "double" ~ "REAL",
      type == "character" ~ "TEXT",
      type == "integer" ~ "INTEGER",
      type == "logical" ~ "INTEGER",
      type == "Date" ~ "DATE",
      type == "POSIXct" ~ "DATETIME",
      TRUE ~ paste0(type)
    ))
  
  sur_col_types
}
