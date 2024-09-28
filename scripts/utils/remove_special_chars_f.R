remove_special_chars <- function(string) {
  # Regular expression to match all special characters except underscores
  pattern <- "[^\\w_]"
  
  # Replace matched characters with an empty string
  str_replace_all(string, pattern, "")
}