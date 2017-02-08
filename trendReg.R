trendReg <- function(x, ...) {
	coefficients(
		lm(
			x ~ as.numeric(1:length(x)),
#			na.action = na.omit,
			...
		)
	)[2]
}