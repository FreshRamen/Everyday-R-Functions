JFCI <- function(pos_words, neg_words){
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