logbook.add <- function(..., append = T, collapse = T){
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