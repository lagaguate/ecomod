
  require(rjags)
  require(coda)
  
  loadfunctions( "bayesian") 


  # m = mortality rate
  # x = number of deaths

  model.bin = 
"
model {
  x ~ dbin(m,n)
# m ~ dunif(0.0, 1.0)  # uninformative prior
  m ~ dbeta(4.2, 33.9) # informative prior for proportions: distribution is bounded 0-1   
}
"
  
  data.bin = list( x=5, n=10 )

  m = jags.model( file=jags.setup(model.bin), data=data.bin, n.chains=1, n.adapt=1000 )  #initiate and adapt steps

  variable.names (m)
  coef(m)
  update( m, 1000)

  y = coda.samples(m, variable.names=c("m"), n.iter=5000, thin = 10) # sample from posterior
  summary(y)
  plot(y)
  autocorr.plot(y)

