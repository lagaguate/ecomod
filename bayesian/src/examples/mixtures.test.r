  #          K                          
  # P(yi ) = [SUM]  k Normal(yi | µk , sigma(k)^2 )
  #          k=1


  require (rjags)
  require(coda)
  
  loadfunctions( "bayesian") 




	# simulated data using the same model with 2 components .. not bad 
	
	nx =c ( 100, 150, 200 )*2
	nmeans = c( 57, 67, 91 )
	nprec = c( 1, 2., 5 )

	nc = length( nx )

	dists = data.frame()
	for (i in 1:nc ) {
		dists = rbind ( dists, data.frame( cbind ( 
			y=rnorm(nx[i], mean=nmeans[i], sd=nsd[i]) , id = i ) 
		) )
	}
	hist( dists$y, "fd")

	fdat = list(
		N = nrow(dists),
		y = dists$y,
		k = nc,
		mmodes = c(55, 70, 95 ),
		mtau = rep(1 / var(dists$y)/nc, nc) ,
		gmu = rep(1, nc),
		gr= rep(1, nc), 
		e = rep(1, nc)
	)
 
  model.test = "
model {
  for (i in 1:N) {
    y[i] ~ dnorm( mu[S[i]], tau[S[i]] )
    S[i] ~ dcat(eta)
  }
  for (j in 1:k) {
    mu[j] ~ dnorm( mmodes[j], mtau[j] )
    tau[j] ~ dgamma( gmu[j], gr[j] )
	}
	eta ~ ddirch(e[])
}  
"

	vars.to.sample = c("S", "mu", "tau", "mmodes", "mtau", "gmu", "gr" )
	
	# initiate and adapt steps
	m = jags.model( file=jags.setup(model.test), data=fdat, n.chains=3, n.adapt=1000 )  
	
	# sample from posterior
	x = jags.samples(m, variable.names=vars.to.sample, n.iter=4000) 
 
	
  dists$pred=apply(x$S, MARGIN=c(1), FUN=mean )
	dists$p = as.numeric( as.character( 
		cut( dists$pred, breaks=c(0, 1.5, 2.5, 4), labels=c(1,2,3) )
	))
	plot(jitter(p) ~ jitter(id), dists )
	a = lm ( p~ id, dists) 
  summary(a)

	lapply(x, FUN=function(x) apply(x, MARGIN=c(1), FUN=mean) )
	lapply(x, FUN=function(x) apply(x, MARGIN=c(1), FUN=sd) )

  variable.names (m)
  coef(m)

	# using coda interface
	# sample from posterior
  x = coda.samples(m, variable.names=vars.to.sample, n.iter=1000, thin = 10) 
  summary(x)
  plot(x, ask=T)
  autocorr.plot(x)

	densityplot(x)
	autocorr.plot(x[1])

	acf( x[1])
	autocorr(x, lags = c(0, 1, 5, 10, 50), relative=TRUE)

	geweke.plot(x)
	gelman.plot(x)
	

