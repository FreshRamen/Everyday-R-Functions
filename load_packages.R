load_packages <- function(packages) {
  quietly <- sapply(packages, function(x) suppressMessages(library(x, character.only = T)))
}