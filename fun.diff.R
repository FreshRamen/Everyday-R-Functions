fun.diff <- function(x, d=1) {
  NAs <- rep(NA, d)
  diffres <- diff(x, lag=d)
  res <- c(NAs, diffres)
  return(res)
}