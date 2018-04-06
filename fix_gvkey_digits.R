fix_gvkey_digits <- function(x) {
  # Sets x to 6 digits
  x <- as.character(x)
  ifelse(nchar(x)==4, paste0("00",x), ifelse(nchar(x)==5, paste0("0",x), x))
}