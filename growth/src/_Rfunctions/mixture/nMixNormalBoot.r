nMixNormalBoot <- function(x,aic.threshold=2,mode.thresh=5,init.mode,niter=10000,init.lam,init.var) {

		require(mixtools)
		loglikes 	<- vector(length=niter)
		aic 		<- vector(length=niter)
		nmodes 		<- vector(length=niter)
		modes 		<- list()
		x 			<- na.omit(x)
		n			<- length(x)
		nsamp 		<- length(init.mode)
		outmodes	<- list()
		
		for(i in 1:niter) {
		if(nsamp>2) {
		v <- sample(2:nsamp,size=1)
		oi 	<-	sample(nsamp,size=v,replace=F)
			
		ins.mu 	<- init.mode[oi]
		ins.var	<-	init.var[oi]
		ins.lam	<- init.lam[oi]
		k			<- length(oi)
		modes[[i]] 	<- ins.mu
		nmodes[[i]] <- k
		} else {
		oi 	<-	1:2
		ins.mu 	<- init.mode[oi]
		ins.var	<-	init.var[oi]
		ins.lam	<- init.lam[oi]
		k			<- length(oi)
		modes[[i]] 	<- ins.mu
		nmodes[[i]] <- k
		}
		
		
			if(k==1) {	# k=1 needs special handling
					mu			<- mean(x) 				# MLE of mean
					sigma 		<- sd(x)*sqrt((n-1)/n) 	# MLE of standard deviation
					 o <- makemultdata(x,cuts=seq(min(x)-1,max(x)+1))
 					p <- multmixmodel.sel(o, comps = c(1), epsilon = 1e-03)
					loglikes[i] <- p[[5]]
					aic[i] 		<- p[[1]]
					outmodes[[i]] <- mu
				} else {
					mixture 	<- try(normalmixEM(x,k=k,maxit=10000,epsilon=1e-3,mu=ins.mu,verb=F,sigma=sqrt(ins.var),lambda=ins.lam),silent=T)
					if(class(mixture)!='try-error') {
					loglikes[i] <- loglikeMix(x,mixture=mixture)
					aic[i] 		<- aicc(lL=loglikes[i],k=k*3-1,n=n)
					outmodes[[i]] <- mixture$mu
				} else {
					loglikes[i] <- -100000
					aic[i] 		<- aicc(lL=loglikes[i],k=k*3-1,n=n)
					outmodes[[i]] <- 0
				}
			}
		}
	#nk <- findMinAIC(x=data.frame(nmodes,aic,1:length(aic)),thresh=aic.threshold)	
	nk <- findBestModel(x=data.frame(nmodes,aic,1:length(aic)),aic.thresh=aic.threshold,modes=modes,mode.thresh=mode.thresh)	
	nmo <- nmodes[nk]
	mixture=list(mu=0,sigma = 0,ft='normalmixEM',lambda=1,ft='normalmixEM',loglik=99999)
	if(length(nmo>1)==0 | !is.finite(nmo)) browser()
	if(nmo>1) {
			mixture <- try(normalmixEM(x,k=nmodes[nk],maxit=10000,epsilon=1e-2,mu=outmodes[[nk]],verb=F),silent=T)
			if(class(mixture)!='try-error') {
			o 		<- sort(mixture$mu) 
			} else {
			mixture=list(mu=0,sigma = 0,ft='normalmixEM',lambda=1,ft='normalmixEM',loglik=99999)
			}
	} else {
		mu<-mean(x) # MLE of mean
				sigma <- sd(x)*sqrt((n-1)/n) # MLE of standard deviation
				loglikes <- sum(dnorm(x,mu,sigma,log=TRUE))
		mixture=list(mu=mu,sigma = sigma,ft='normalmixEM',lambda=1,ft='normalmixEM',loglik=loglikes)
	}

	return(mixture)
}