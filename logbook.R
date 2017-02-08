# These functions I use for logging. logbook.add() adds text to a vector, which is then, in the end, concentrated and written do disk as a markdown file by logbook.write().

logbook.add <- function(..., append = T, collapse = T){
  # Adds character vector(s) to a "logbook", a vector containing text to be logged. 
  # Can be written to disk as markdown using logbook.write().
  # Use append = F to overwrite existing logbook. If logbook is missing, the script will create one.
  # Use collapse = F to store each element of ... as an individual line in the logbook (useful, e.g., for items in a list).
  if (!exists("logbook") | append == F) logbook <<- vector()
  if (collapse == T){
      logbook.new <- paste(c(...), collapse = " ")
  } else {
    logbook.new <- paste(c(...), collapse = "\n")
  }
  logbook <<- c(logbook, logbook.new)
}


logbook.write <- function(logfile = "logbook", filename, path = getwd(), PDF = F){
  log_out <- file.path(path,filename)
  if (length(logfile) == 1) logfile <<- get(logfile)
  cat(logfile, sep = "\n\n", file = log_out)
  if (PDF == T) {
    log_name <- stri_split_fixed(filename, ".")[[1]][1]
    log_pdf <- paste0(log_name, ".pdf")
    log_pdf <- file.path(path, log_pdf)
    system(paste("pandoc -o",log_pdf,log_out))
  }
}