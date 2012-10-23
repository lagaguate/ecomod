
  require (rjags)
  require(coda)
  
  loadfunctions( "bayesian") 




  # age structure of koalas based on tooth wear categorizations

  model.multinom = 
"
model {
  Y[1:9] ~ dmulti( p[1:9],N )
  p[1:9] ~ ddirch(alpha[]) # informative prior for proportions: distribution is bounded 0-1   
}
"
  
  data.multinom  = list( 
    Y=c(55, 132, 88, 48, 31, 26, 14, 3, 0)
  )
  data.multinom$N = sum( data.multinom$Y )
  data.multinom$alpha = rep(1, length(data.multinom$Y) ) # uniform prior Pr for each age class 

  m = jags.model( file=jags.setup(model.multinom ), data=data.multinom , n.chains=1, n.adapt=10000 )  #initiate and adapt steps

  variable.names (m)
  coef(m)

  y = coda.samples(m, variable.names=c("p"), n.iter=5000, thin = 10) # sample from posterior
  summary(y)
  plot(y)
  autocorr.plot(y)

# can also use dcat
#
#
