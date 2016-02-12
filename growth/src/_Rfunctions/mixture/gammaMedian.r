gammaMedian <- function(shape,scale) {
		a <- qgamma(.5,shape=shape,scale=scale)
		return(a)
}