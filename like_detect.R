like_detect <- function(strings, likes){
  # vectorized like(...), requires data.table
  strings <- toupper(strings)
  likes <- toupper(likes)
  result <- lapply(likes, function(x) strings %like% x)
  result <- Reduce("|", result)
  return(result)
}