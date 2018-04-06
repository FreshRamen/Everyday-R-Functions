get.mode <- function(v, na.rm = TRUE) {
  if (na.rm==TRUE) {
    v <- v[!is.na(v)]
  }
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}