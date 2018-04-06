winsorize <- function(x, level, na.rm=TRUE, ...){
	# x is a vector of numeric values
	# level is the level at which to windsorize, in % from 0 to 100
	# level = 0 means nothing cut, level = 100 means all cut
	level_min = (0 + level) / 100
	level_max = (100 - level) / 100
	min_x <- quantile(x, probs=level_min, na.rm=na.rm)
	max_x <- quantile(x, probs=level_max, na.rm=na.rm)
	x[!is.na(x) & x > max_x] <- max_x
	x[!is.na(x) & x < min_x] <- min_x
	return(x)
}