plt.scatter <- function(y.cols, x.col, data){
    
  x.dat <- data[, .SD, .SDcols=x.col]
  dt <- data[, .SD, .SDcols=c(y.cols)]

  dt <- melt(dt, measure.vars=y.cols, variable.factor=FALSE)
  
  dt[, c(x.col) := unlist(rep(x.dat, times=length(y.cols)))]
  
  plt <- 
  
  ggplot(dt, aes(get(x.col), value)) + geom_point(color="firebrick") + facet_wrap(~variable, scales = "free") + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) # Plot object
  
  return(plt)

}