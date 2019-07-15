tonum.ch <- function(x){
  # This function transforms swiss numerals to numeric type
  x <- gsub("\\'", "", x)
  x <- as.numeric(x)
  return(x)
}
