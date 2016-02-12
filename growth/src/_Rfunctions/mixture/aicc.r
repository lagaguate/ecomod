aicc <- function(lL,k,n) {
		a <- 2*k-2*lL + (2*k*(k+1))/(n-k-1)
		return(a)
}