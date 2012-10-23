
require(rjags)
require(coda)
  
  loadfunctions( "bayesian") 



mm = "
model {
  p ~ dpois( 1/300000 )


m = jags.model( file=jags.setup( mm ), data=dd, n.chains=1, n.adapt=1000 )  #initiate and adapt steps
coef(m)
dic(m)
update( m, 1000)

y = coda.samples(m, variable.names=c("mu", "tau", "eta"), n.iter=5000, thin = 10) # sample from posterior
densityplot(y)

geweke.plot(y)
gelman.plot(y[1])

