get.text_file <- function(file, ...) {
  textfile <- scan(file, character(0), sep = "\n", quiet = T, ...)
  return(textfile)
}