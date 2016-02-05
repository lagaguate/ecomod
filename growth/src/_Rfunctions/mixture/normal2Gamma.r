normal2Gamma <- function(mu,sd) {
	beta = sd^2 / mu
	alpha= mu / beta
	return(c(alpha=alpha,beta=beta))
}