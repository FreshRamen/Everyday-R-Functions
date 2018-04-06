get_rows_csv <- function(file){
  rows <- system2("wc", args=paste("-l", file), stdout=TRUE)
  rows <- strsplit(rows, " ", fixed=TRUE)
  rows <- rows[[1]][2]
  rows <- as.numeric(rows)
  return(rows)
}