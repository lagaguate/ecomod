
require (arm)


n <- 100
x1 <- rnorm (n)
x2 <- rbinom (n, 1, .5)
b0 <- 1
b1 <- 1.5
b2 <- 2
y <- rbinom (n, 1, invlogit(b0+b1*x1+b2*x2))


M1 <- glm (y ~ x1 + x2, family=binomial(link="logit"))
display (M1)

M2 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"),
  prior.scale=Inf, prior.df=Inf)
display (M2) # just a test: this should be identical to classical logit

M4 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"),
  prior.scale=2.5, prior.df=1)
  # Same as M3, explicitly specifying Cauchy prior with scale 2.5
display (M4)

M6 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"),
  prior.scale=2.5, prior.df=Inf) # normal prior with scale 2.5
display (M6)


# -------------
# Dugongs model in JAGS 

source ( file.path( project.common, "functions.jags.r" ) ) 

jags.data = list(
  x = c( 1.0,  1.5,  1.5,  1.5, 2.5,   4.0,  5.0,  5.0,  7.0,
	            8.0,  8.5,  9.0,  9.5, 9.5,  10.0, 12.0, 12.0, 13.0,
	           13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
	Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
	           2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
	           2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57), 
  N = 27,
  alpha = 1, 
  beta = 1, 
  tau = 1, 
  gamma = 0.9
)


dugong.model = "
model {
  for( i in 1 : N ) {
    Y[i] ~ dnorm(mu[i], tau) 
    mu[i] <- alpha - beta * pow(gamma,x[i])	
  }
  alpha ~ dnorm(0.0, 1.0E-6) 
  beta ~ dnorm(0.0, 1.0E-6) 
  gamma ~ dunif(0.5, 1.0) 
  tau ~ dgamma(0.001, 0.001) 
  sigma <- 1 / sqrt(tau) 
  U3 <- logit(gamma)	
}
"


  # initiate and adapt steps
  
  m = jags.model( file=jags.setup( dugong.model ), data=jags.data, n.chains=3, n.adapt=10000 )  
 
  # sample from posterior
  x = jags.samples(m, variable.names=c("U3", "alpha", "beta", "tau", "gamma", "sigma"), n.iter=10000)  
  x = coda.samples(m, variable.names=c("U3", "alpha", "beta", "tau", "gamma", "sigma"), n.iter=10000)  
  
  lapply(x, FUN=function(x) apply(x, MARGIN=c(1), FUN=mean) )
  lapply(x, FUN=function(x) apply(x, MARGIN=c(1), FUN=sd) )
  
  dic.samples(m,  n.iter=10000, thin=10, type="pD")  
  coef.jags( m)
  z = as.mcmc.list (x)


mean	sd	MC_error	val2.5pc	median	val97.5pc	start	sample
	U3	1.861	0.2678	0.01189	1.321	1.865	2.37	1001	10000
	alpha	2.652	0.07094	0.003378	2.532	2.646	2.808	1001	10000
	beta	0.9729	0.07649	0.001806	0.8251	0.9711	1.129	1001	10000
	gamma	0.8623	0.03259	0.001393	0.7894	0.8658	0.9145	1001	10000
	sigma	0.0992	0.01496	1.831E-4	0.07513	0.09742	0.1339	1001	10000



# -----------------

source ( file.path( project.common, "functions.jags.r" ) ) 

jmodel = "

data{
  stdev <- sd(Y)
}
model {
  m ~ dnorm( 53, 0.04 )
  for( i in 1:10 ) {
    Y[i] ~ dnorm( m, 1/ (stdev*stdev) ) 
  }
}
"
jdata = list(
  Y = c(42,43,58,70,47,51,85,63,58,46)
)
  
  m = jags.model( file=jags.setup( jmodel ), data=jdata, n.chains=1, n.adapt=1000 )  
  x = jags.samples(m, variable.names=c("m", "stdev"), n.iter=100000)  
  
  lapply(x, FUN=function(x) apply(x, MARGIN=c(1), FUN=mean) )
  lapply(x, FUN=function(x) apply(x, MARGIN=c(1), FUN=sd) )
  
  dic.samples(m,  n.iter=10000, thin=10, type="pD")  
  coef.jags( m)
 

 
