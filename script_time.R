script_time <- function(start_time, round = 2, message=TRUE){
  # Function returns run time of script with unit
  # Requires start_time to be set, i.e., start_time <- Sys.time()
  # Use round to control digits
  time_diff <- Sys.time() - start_time
  round_units <- round
  units <- attr(time_diff, "unit")
  time <- round(time_diff, round_units)
  script_time <- paste(time, units)
  if (message==FALSE){
    return(script_time)    
  } else {
    message("\nDone after ", script_time)
  }
}