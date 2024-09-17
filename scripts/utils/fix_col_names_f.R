remove_special_chars <- function(string) {
  gsub("[^[:alnum:]_]", "", string)
}

names_fix <- function(df) {
  names(df) <- gsub(" ", "_", names(df))
  names(df) <- gsub("#", "No", names(df))
  names(df) <- remove_special_chars(names(df))
  return(df)
}

remove_special_chars <- function(string) {
  # Regular expression to match all special characters except underscores
  pattern <- "[^\\w_]"
  
  # Replace matched characters with an empty string
  str_replace_all(string, pattern, "")
}