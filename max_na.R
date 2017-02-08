max_na <- function(x, ...) {
  # This function provides an NA handler for max, fixing an issue where all n being NA returns inf
  ifelse(any(!is.na(x)), max(x, na.rm = T, ...), as.numeric(NA) )
}