# Function to count number of words that occur in a string
vec_str_count <- function(word_list, text) {
  counts <- sapply(text, function(x) sum(stri_detect_fixed(x, word_list)))
  names(counts) <- NULL
  return(counts)
}

# vec_str_count3 <- function(word_list, text) {
#   # This function will not word accurately, because it does not correctly tokenize text before using %chin%. Words can be separated by a "," instead of a " ".
#   counts <- lapply(stri_split_fixed(text, " "), function(x) sum(unique(x) %chin% word_list))
#   counts <- unlist(counts)
#   return(counts)
# }


# text <- DT.Docs[1:1000, Text]

# system.time(test1 <- vec_str_count1(tokens_neg, text))
# system.time(test2 <- vec_str_count2(tokens_neg, text))


# identical(test1, test2)

#
# a <- text[1]
# res1 <- tokens_neg[stri_detect_fixed(a, tokens_neg)]
# res2 <- unique(unlist(stri_split_fixed(a, " ")))[unique(unlist(stri_split_fixed(a, " "))) %chin% tokens_neg]
#
#
# test <- "This is an example text with good words and good stuff awesome cool super"
# test <- c(test, paste(test, "cool cool"))
# words <- c("good", "awesome", "cool", "super")
