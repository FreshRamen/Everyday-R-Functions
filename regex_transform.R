regex_transform <- function(string){
  # Wraps regex word boundaries around string
  # String is a string or character vector
  regex <- paste0("\\b", string, "\\b")
  return(regex)
}