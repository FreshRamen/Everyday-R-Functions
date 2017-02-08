set_laps <- function(values, intervals){
  # Function sets intervals for a loop over a vector of items
  # values is the vector of values; interval the number of intervals
  # Returns a vector of integer indices
  stop_int <- length(values)
  start_int <- round(stop_int / intervals)
  laps <- seq(start_int, stop_int, length.out = intervals)
  laps <- round(laps)
  laps <- c(0, laps)
  return(laps)
}