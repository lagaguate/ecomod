loglikeMix <- function(x,mixture) {
		loglike <- dMix(x,mixture,log=TRUE)
		return(sum(loglike))
	}