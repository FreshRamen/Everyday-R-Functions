plt.multi_series <- function(y.cols, x.col, data){
  
  dt <- data[, .SD, .SDcols=c(x.col, y.cols)]
  dt[, c(x.col) := lapply(.SD, as.factor), .SDcols=x.col]
  
  dt <- melt(dt, id.vars=x.col, measure.vars=y.cols, variable.factor=FALSE, )
  
  plt <- ggplot(dt, aes(get(x.col), value)) + geom_point(color="firebrick") + facet_wrap(~variable, scales = "free") + theme(axis.text.x = element_text(angle=45)) # Plot object
  
  return(plt)

}