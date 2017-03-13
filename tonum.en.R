tonum.en <- function(x){
  # This function transforms number to proper digits by replacing "," with "."
  x <- gsub("\\,","",x)
  x <- as.numeric(x)
  return(x)
}
