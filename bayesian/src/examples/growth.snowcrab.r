
# ----------------------
# mixtures analysis using snowcrab data
	loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 
  loadfunctions( "bayesian" ) 



	det = snowcrab.db( DS ="det.georeferenced" ) 
  
	det$cw = log( det$cw )

  hvar="cw" #!
  base = 2
  s0 = 10  # ~ ln(10)
  s1 = 175  # ~ ln(160)
  i0 = which( det[,hvar]>=s0 & det[,hvar]<=s1 )

  # ii = which(det$sex==0 & det$mat=="1" & as.numeric(substring(as.character(det$trip),6,9))==2008)
  ifemale = which(det$sex==1 )
  imale = which(det$sex==0 )

	i = intersect( i0, ifemale)
	i = intersect( i, which(det$yr==2008) )

	hist( det$cw[i] , "fd" )
	
	Y = det[ i ,]
	ninstars

	snowcrab = list(
		cw = Y$cw,
		n = nrow(Y),
		k = ninstars,
		cwmodes = ,
		cwtau = ,
		e = rep(1, ninstars )
	)


	 
  jags.nodes = "
model {
  for (i in 1:N) {
    cw[i] ~ dnorm( mu[S[i]], tau[S[i]] )
    S[i] ~ dcat(eta)
  }
  for (j in 1:k) {
    mu[j] ~ dnorm( cwmodes[j], cwtau[j] )
    tau[j] ~ dgamma( cwmu[j], cwr[j] )
	}
	eta ~ ddirch(e[])
}  
"

	vars.to.sample = c("S", "mu", "tau", "cwmodes", "cwtau", "cwmu", "cwr" )
	
	# initiate and adapt steps
	m = jags.model( file=jags.setup( jags.nodes ), data=fdat, n.chains=3, n.adapt=1000 )  
	
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
	


  


