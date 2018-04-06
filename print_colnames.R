print_colnames <- function(DT=data.table()) {
  # Print column names of a data.table, ready-usable for a list
  print <- paste0("'",paste(colnames(DT), collapse="', '"), "'")
  return(print)
}