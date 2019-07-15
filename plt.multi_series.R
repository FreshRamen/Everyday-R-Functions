plt.multi_series <- function(columns, data){

ggplot(nmmaps, aes(temp, o3))+geom_point(color="firebrick")+facet_wrap(~season, scales="free")+
  annotation_custom(my_grob)

}