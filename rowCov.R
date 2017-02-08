rowCov <- function(x,y){
  rowSums( (x - rowMeans(x)) * (y - rowMeans(y)) ) / ( ncol(x) - 1 )
}