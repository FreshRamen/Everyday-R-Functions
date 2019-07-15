get.text_file <- function(file, ...) {
  textfile <- scan(file, character(0), sep ="\n", quiet=TRUE, ...)
  return(textfile)
}