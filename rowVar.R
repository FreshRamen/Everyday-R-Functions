rowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(ncol(x) - 1)
}
