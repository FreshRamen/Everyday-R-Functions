# Function to count number of words that occur in a string
vec_string_count <- function(word_list, text) sapply(text, function(x) sum(stri_detect_fixed(x, word_list)))

