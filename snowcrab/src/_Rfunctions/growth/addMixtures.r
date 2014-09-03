addMixtures <- function(mixture,component.number,...) {

	d = mixture$ft
	if(d=='normalmixEM') {
	curve(mixture$lambda[component.number] *	
			dnorm(x,mean=mixture$mu[component.number],	
			sd=mixture$sigma[component.number]), add=TRUE, ...)
			
			}
	if(d=='gammamixEM') {

	curve(mixture$lambda[component.number] *	
			dgamma(x,shape=mixture$gamma.pars[1,component.number],scale=mixture$gamma.pars[2,component.number]),	add=TRUE, ...)
	}
	
			
		}