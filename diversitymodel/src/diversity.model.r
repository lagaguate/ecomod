
 
  require(rjags)
  rjags::load.module("dic")
  rjags::load.module("glm")

	source(	file.path( project.directory("diversity")model, "src", "load.environment.diversitymodel.r" ) )
  
 
  redo.data=F
  
  if (redo.data) { 
    gf = bio.db( DS="subset", p=p)
    speciesarea.db(DS="speciesarea.counts.redo", p=p)
  }


  # MCMC/Gibbs parameters
  n.adapt = 1000 # burn-in
  n.iter = 1000 
  n.chains = 3
  n.thin = 10
  n.iter.final = n.iter * n.thin
  
  # fnres = file.path( project.directory("diversity")model, "mcmc.rdata" )
  fnres = file.path( project.directory("diversity")model, "mcmc.debug.rdata" )


  # -------------------
  ##  simple logistic with observation and process error
  m = jags.model( file=diversity.model.jags ( DS="logistic" ), data=sb, n.chains=n.chains, n.adapt=n.adapt )  
  
  tomonitor =  c("Z", "B", "Rhat", "q" )
  tomonitor = intersect( variable.names (m), tomonitor )
  coef(m)
  
  dic.samples(m, n.iter=n.iter ) # pDIC
  
  y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior
  
  graphics.off() ; x11()
  hist( y$Z[], "fd")
 
  # update if not yet converged 
  #  update(m, n.iter=n.iter ) # above seems enough for convergence but a few more to be sure


  # autocorrelation thinning
    y = coda.samples(m, variable.names=c("K", "r", "q"), n.iter=n.iter, thin=n.thin) # sample from posterior
    autocorr.plot(y)
    # plot(y, ask=T)
    # autocorr(y, lags = c(0, 1, 5, 10, 50), relative=TRUE)


  # final sampling from the posteriors
   #  y = jags.samples(m, variable.names=tomonitor, n.iter=200000, thin=100) # sample from posterior
    y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

    save(y, file=fnres, compress=T)
    load( fnres )


  # Figures
    graphics.off()
 




