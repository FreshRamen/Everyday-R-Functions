plt.raster_cor <- function(columns, data) { 
  # Creates a tile/raster plot of correlations
  # Insprited from http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
  # Section: Create a tiled correlation plot (geom_tile())
  
  cols <- sort(columns)
  thecor <- DT[, round(cor(.SD, method="pearson", use="pairwise.complete.obs"), 3), .SDcols=cols] # Create correlation table
  thecor[upper.tri(thecor)] <- NA # Remove lower triangle
  
  thecor <- as.data.table(thecor)
  thecor[, Variable := cols]
  
  thecor <- melt(thecor, measure.vars=cols, variable.factor=FALSE, id.vars="Variable")
  thecor <- thecor[!is.na(value)]

  plt <- ggplot(thecor, aes(Variable, variable)) + geom_tile(data=thecor, aes(fill=value), color="white") + scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), name=element_blank()) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + ggtitle("Correlation (Pearson, Pairwise)") + coord_equal() + geom_text(aes(label=value))# Create plot object
  
  return(plt)
}