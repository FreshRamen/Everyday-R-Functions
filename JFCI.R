JFCI <- function(pos_words, neg_words){
	# This function computes the Janis-Fadner Coefficient of Imbalance, for sentiment analysis.
	# pos_words and neg_words are one-dimensional numeric vectors, indicating the number of 
	# positive and negative words in a text, respectively.
	p <- sum(pos_words)
	n <- sum(neg_words)
	if (p > n) {
		return( (p^2 - p * n) / (p + n)^2 )
	} else if (p < n) {
		return( (p * n - n^2) / (p + n)^2 )
	} else {
		return( 0 )
	}
}
