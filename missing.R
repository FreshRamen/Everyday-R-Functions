missing.percent <- function(x) {sum(is.na(x)) / length(x)}

missing.number <- function(x) {sum(is.na(x))}

missing.value <- function(x) {ifelse(is.na(x),1,0)}