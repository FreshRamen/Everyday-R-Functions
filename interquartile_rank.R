interquartile_rank <- function(x) cut(x, breaks = quantile(x, c(0.25, 0.5, 0.75, 1)), include.lowest = TRUE, labels = FALSE)
