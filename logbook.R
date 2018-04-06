# These functions I use for logging. logbook.add() adds text to a vector, which is then, in the end, concentrated and written do disk as a markdown file by logbook.write().

logbook.add <- function(..., append=TRUE, collapse=TRUE){
  # Adds character vector(s) to a "logbook", a vector containing text to be logged. 
  # Can be written to disk as markdown using logbook.write().
  # Use append = F to overwrite existing logbook. If logbook is missing, the script will create one.
  # Use collapse = F to store each element of ... as an individual line in the logbook (useful, e.g., for items in a list).
  if (!exists("logbook") | append == FALSE) logbook <<- vector()
  if (collapse==TRUE){
      logbook.new <- paste(c(...), collapse = " ")
      logbook.new <- c(logbook.new, "\n")
  } else {
    logbook.new <- paste(c(...), collapse = "\n")
    logbook.new <- c(logbook.new, "\n")
  }
  logbook <<- c(logbook, logbook.new)
}

logbook.header <- function(title="Enter title here"){
  # Create new logbook
  logbook <<- vector()
  logbook.add(
    title, 
    "============================", 
    ""
    , collapse=FALSE)
}

logbook.write <- function(logfile = "logbook", file, path = getwd(), PDF = F){
  log_out <- file.path(path,file)
  if (length(logfile) == 1) logfile <<- get(logfile)
  cat(logfile, sep = "\n", file = log_out)
  if (PDF == T) {
    log_name <- stri_split_fixed(file, ".")[[1]][1]
    log_pdf <- paste0(log_name, ".pdf")
    log_pdf <- file.path(path, log_pdf)
    system(paste("pandoc -o",log_pdf,log_out))
  }
}

log.head <- logbook.header
log.header <- logbook.header
log.add <- logbook.add
log.write <- logbook.write

# writelog <- function(setpath=False){
#   if (path==False){
#     logbook.write(logbook, "log.md", path.compustat)
#   } else
#
# }