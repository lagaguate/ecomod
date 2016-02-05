nMixGammaBoot <- function(x,aic.threshold=2,mode.threshold=5,init.mode,niter=1000) {

		require(mixtools)
		loglikes 	<- vector(length=niter)
		aic 		<- vector(length=niter)
		nmodes 		<- vector(length=niter)
		modes 		<- list()
		x 			<- na.omit(x)
		n			<- length(x)
		nsamp 		<- length(init.mode)
		alpha.in 	<- list()
		beta.in 	<- list()
		outmodes	<- list()
		outalpha	<- list()
		outbeta		<- list()
		
		for(i in 1:niter) {
		ins <- sample(init.mode,size=sample.int(nsamp,1),replace=F)
		k			<- length(ins)
		nmodes[[i]] <- k
		sd			<- runif(k,1,4)
		alpha 		<- c()
		beta		<- c()	
		for(w in 1:k) {
					h <- normal2Gamma(ins[w],sd[w])
					alpha[w]	<- h[1]
					beta[w] 	<- h[2]
				}
		
		alpha.in[[i]] 	<- alpha	
		beta.in[[i]] 	<- beta	
		
		if(k==1) {
				mu<-mean(x) # MLE of mean
				sigma <- sd(x)*sqrt((n-1)/n) # MLE of standard deviation
				loglikes[i] <- sum(dnorm(x,mu,sigma,log=TRUE))
				aic[i] <- aicc(lL=loglikes[i],k=k*3-1,n=n)
				outmodes[[i]] <- mu
				outalpha[[i]] <- 0
				outbeta[[i]] <- 0
				
			} else {
				mixture <- try(gammamixEM(x,k=k,maxit=10000,epsilon=1e-2,alpha=alpha,beta=beta,verb=F),silent=T)
				if(class(mixture)!= "try-error") {
				loglikes[i] <- mixture$loglik
				aic[i] <- aicc(lL=loglikes[i],k=k*3-1,n=n)
				outmodes[[i]] <- gammaMedian(shape=mixture$gamma.pars[1,],scale=mixture$gamma.pars[2,])
				outalpha[[i]] <- mixture$gamma.pars[1,]
				outbeta[[i]] <- mixture$gamma.pars[2,]
			} else {
				loglikes[i] <- -1000000
				aic[i] <- aicc(lL=loglikes[i],k=k*3-1,n=n)
				outmodes[[i]] <- 0
			}
			
			}
			}

	#nk <- findMinAIC(x=data.frame(nmodes,aic,1:length(aic)),thresh=aic.threshold)	
	nk <- findBestModel(x=data.frame(nmodes,aic,1:length(aic)),aic.thresh=aic.threshold,modes=outmodes,mode.thresh=6)	

	nmo <- nmodes[nk]
	if(nmo>1) {
		mixture <- try(gammamixEM(x,k=nmodes[nk],maxit=1000,epsilon=1e-2,alpha=outalpha[[nk]],beta=outbeta[[nk]],verb=F),silent=T)
		if(class(mixture)=='try-error') mixture <- gammamixEM(x,k=nmodes[nk],maxit=1000,epsilon=1e-2)
		mixture$median <- gammaMedian(shape=mixture$gamma.pars[1,],scale=mixture$gamma.pars[2,])
		o <- sort(mixture$median)
	} else {
				mu<-mean(x) # MLE of mean
				sigma <- sd(x)*sqrt((n-1)/n) # MLE of standard deviation
				loglikes <- sum(dnorm(x,mu,sigma,log=TRUE))
		mixture=list(mu=mu,sigma = sigma,ft='normalmixEM',lambda=1,ft='normalmixEM',loglik=loglikes)
	}

	return(mixture)
}