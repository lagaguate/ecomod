dMix <- function(x,mixture,log=FALSE) {
		lambda <- mixture$lambda
		k <- length(lambda)
	
	# Calculate share of likelihood for all data for one component
		like.component <- function(x,component) {
		lambda[component]*dnorm(x,mean=mixture$mu[component],
		sd=mixture$sigma[component])
		}
	# Create array with likelihood shares from all components over all data
		likes <- sapply(1:k,like.component,x=x)
		# Add up contributions from components
		d <- rowSums(likes)
		if (log) {
		d <- log(d)
		}
	return(d)
}
