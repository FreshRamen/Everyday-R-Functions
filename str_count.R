# Function to count number of words that occur in a string
str_count <- function(word_list, text) sum(unlist(strsplit(text, " ")) %chin% word_list)