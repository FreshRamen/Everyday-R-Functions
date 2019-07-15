JFCI <- function(pos_words, neg_words) {
	# Compute Janis-Fadner Coefficient of Imbalance
  # For data.table with by, use sum(pos_words), sum(neg_words)
	# pos_words and neg_words are one-dimensional numeric vectors, indicating the number of positive and negative words in a text, respectively.
	p <- pos_words
	n <- neg_words
  jfci <- ifelse(
    p > n, (p^2 - p * n) / (p + n)^2, 
    (p * n - n^2) / (p + n)^2)
  jfci[!is.finite(jfci)] <- NA # Replacing infinite values by NA (where n=p=0)
  return(jfci)
}