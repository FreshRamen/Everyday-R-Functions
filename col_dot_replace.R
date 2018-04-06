col_dot_replace <- function(DT){
  # Replace . in colnames by _
  # Useful after loading Excel sheets with spaces in colnames using openxlsx
  # DT = a data.table object
  cols <- colnames(DT)
  cols_new <- gsub(".","_",cols, fixed = TRUE)
  setnames(DT, cols_new)
}