tonum <- function(x, decimals = F){
  # This function transforms strings to numeric
  # that contain some non-numeric characters
  # It ignores dots
  if (decimals == F){
    x <- gsub("[^0-9]","",x)
    x <- as.numeric(x)
  } else {
    x <- gsub("\\,","",x)
    x <- stri_extract_all_regex(x, "[0-9]*\\.[0-9]{0,2}")
    x <- unlist(x)
    x <- stri_flatten(x)
    x <- as.numeric(x)
  }
  return(x)
}
