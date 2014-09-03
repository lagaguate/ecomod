percentDifference <- function(x) {
	 n <- length(x)
 	 x <- x[order(x)]
 	 v <- (x[2:n]-x[1:n-1])/((x[2:n]+x[1:n-1])/2)*100
	return(v)
}