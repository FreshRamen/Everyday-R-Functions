tonum.eu <- function(x){
  # This function transforms number to proper digits by replacing "," with "."
  x <- gsub("\\.", "", x)
  x <- gsub("\\,","\\.",x)
  x <- as.numeric(x)
  return(x)
}
