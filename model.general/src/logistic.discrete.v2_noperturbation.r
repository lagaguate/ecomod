
# testing methods of parameter estimation for the logistic map

require(rjags)
rjags::load.module("dic")
rjags::load.module("glm")

loadfunctions( "bayesian" )

SS = co2[100:120] 
SS = co2[]
SS = Nile[]

SS = SS + runif(SS)
SSyrs = 1:length(SS)
K.max = max( SS, na.rm=TRUE) 
N = length(SS)
M = 5 # no years for projections  
yrs = start(SS)[1]:(end(SS)[1]+M)  # observation years
dSS= diff(SS)
U.sd =sd(SS, na.rm=T)
U.cv = U.sd / mean( SS, na.rm=TRUE) 

sb = list( 
  O = c(SS, rep(NA, M) ),  
  s.max = 1.5, # max value on normalized scale
  U.sd = U.sd,
  U.cv = U.cv, 
  K.max = K.max,
  N = N, 
  M = M,
  eps = 1e-3  # small non-zero number
)

jagsmod=" model {

# hyper-priors
  r.cv ~ dbeta( 1, 2 ); 
  K.cv ~ dbeta( 1, 2 );
  qO.cv ~ dbeta( 1, 2 );
  s.tau ~ dgamma(0.0001, 0.0001); 
  o.tau ~ dgamma(0.0001, 0.0001); 

# priors
  r ~ dnorm( 1, pow( r.cv, -2) ) T( 0.01, 4 ); 
  qO ~ dnorm( 1, pow( qO.cv, -2) ) T( eps, 2 ); 
  K ~ dlnorm( log( max( K.max, eps)), pow( log(K.max)*K.cv, -2) ) T(K.max*0.75, K.max*s.max); 
  
# observation model 
  for (i in 1:N) { 
    O[i] ~ dlnorm( log( max( qO * K * s[i], eps)), o.tau )  ;  
  }

# process model
  s0 ~ dunif( eps, K.max )
  s[1] ~ dlnorm( log( max( s0, eps)), s.tau ) T(, s.max) ; # process or model error
  S[1] <- qO * K * s[1]
  for(i in 2:N) {
    s[i] ~ dlnorm( log( max(s[i-1]*( 1 + r*(1-s[i-1])) , eps)), s.tau) T(, s.max) ; # process or model error
    S[i] <- qO * K * s[i]
  }

  # forecasts 
  for(i in (N+1):(N+M)) {
    s[i] ~ dlnorm( log( max(s[i-1]*( 1 + r*(1-s[i-1])) , eps)), s.tau) T(, s.max) ; # process or model error
    S[i] <- qO * K * s[i]
  }

# monitoring nodes, estimates for output
#  collapse <- 1 - step( s[N+M]-0.1 ) ; # test if s >= 0.1; collapse defined as less than 10% of K, M years into the future
  MSY <- r * K / 4  # maximum height of of the latent productivity (yield)
  BMSY <- K/2  # biomass at MSY
  FMSY <- 2 * MSY / K # fishing mortality at MSY
#  Fcrash <- 4 * MSY / K # fishing mortality at which the stock will crash
  KSD <- K*K.cv ;
  rSD <- r*r.cv ;
  sSD <- 1/sqrt(s.tau); 
  oSD <- 1/sqrt(o.tau);
# perturbation
#  F[1] <- -log( 1 - u[1] / ( s[1] + u[1] ) ) 
#  for(i in 2:(N+M)) {
#    F[i] <- -log(1 - u[i-1] / ( s[i] + u[i-1] ) ) 
#  }
  # annual change
#  sp[1] <- s[2]- s[1] + u[1]
#  for (i in 2:(N-1) ){
#    sp[i] <- (s[i+1]- s[i-1])/2 + u[i]  # linear interpolation
#  }
#  sp[N] <- s[N]- s[N-1] + u[N]
}
"


n.adapt = 6000 # burn-in  .. 4000 is enough for the full model but in case ...
n.iter = 10000 
n.chains = 3
n.thin = 20 # K and qO are highly autocorrelated ... > 100 steps required 
n.iter.final = n.iter * n.thin


fn.jags = file.path( tempdir(), "jags.model.txt" )  
cat(jagsmod, file=fn.jags)
m = jags.model( file=fn.jags, data=sb, n.chains=n.chains, n.adapt=n.adapt ) 
tomonitor =  c( "r", "K", "qO", "qU", "S", "s", "U", "u", "KSD", "rSD", "sSD", "oSD", "uSD", "sp", "F", "MSY", "BMSY", "FMSY", "Fcrash" )
tomonitor = intersect( variable.names (m), tomonitor )
jsamples = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior

# coef(m)
# graphics.off()



Ks =apply( jsamples$K, 1, mean, na.rm=T  )
Ys = apply( jsamples$S, 1, mean, na.rm=T  )
prs = seq( from=0.025, to=0.975, length.out=600)
YY =  apply( jsamples$S, 1, quantile, probs=prs, na.rm=T  )
yran = range(c(YY, sb$S, Ys ), na.rm=T )* c(0.99, 1.01)
plot( yrs, YY[1,], type="n", ylim=yran, xlab="", ylab=""  )
cols = gray.colors( floor(length( prs)/2) )
cols2 = c(cols[length(cols):1], cols )
for ( j in 1:length(prs) ) {
  lines ( yrs, YY[j,], lwd=4, col=cols2[j] )
}
abline( h=Ks )
abline( v=yrs[N] )
lines ( yrs, Ys, lwd=2, col="blue", lty="dotted" )
points( yrs, sb$O, pch=20, col="darkred" )
lines( SSyrs, SS, lwd=3, lty="dashed", col="red" )



hist( jsamples$r , freq=FALSE, breaks="fd", main="", xlab="", ylab="Density", col="lightgray", border="gray")  


hist( jsamples$K , freq=FALSE, breaks="fd", main="", xlab="", ylab="Density", col="lightgray", border="gray")  


hist( jsamples$KSD, freq=FALSE, breaks="fd", main="", xlab="", ylab="Density", col="lightgray", border="gray")  

hist( jsamples$qO , freq=FALSE, breaks="fd", main="", xlab="", ylab="Density", col="lightgray", border="gray")  


hist( jsamples$qU , freq=FALSE, breaks="fd", main="", xlab="", ylab="Density", col="lightgray", border="gray")  





plot( sb$O ~ yrs )
v="S"; lines( apply( jsamples[[v]], 1, mean, na.rm=T  ) ~ yrs )


v="K"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) ; abline( h=Ks )
v="rSD"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) 
v="KSD"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) 
v="r"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) 
v="qO"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) 
v="MSY"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) 
v="BMSY"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) 
v="FMSY"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) 
v="Fcrash"; (Ks=apply( jsamples[[v]], 1, mean, na.rm=T  )) 

v="S"; plot( apply( jsamples[[v]], 1, mean, na.rm=T  ) ~ yrs )

v="U"; plot( apply( jsamples[[v]], 1, mean, na.rm=T  ) ~ yrs )

v="s"; plot( apply( jsamples[[v]], 1, mean, na.rm=T  ) ~ yrs )

v="u"; plot( apply( jsamples[[v]], 1, mean, na.rm=T  ) ~ yrs )

v="sp"; plot( apply( jsamples[[v]], 1, mean, na.rm=T  ) )

v="F"; plot( apply( jsamples[[v]], 1, mean, na.rm=T  ) ~ yrs )




    # ----------------

    dic.samples(m, n.iter=n.iter ) # pDIC

    
    # ----------------
#    dir.output = file.path(project.datadirectory('snowcrab'),"assessments","2014")
    dir.output = tempdir()
    jsamples = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior
  
    jsamples = jags.samples(m, variable.names=tomonitor, n.iter=6000, thin=1 )
    
    gelman.plot(jsamples[["r"]])
    gelman.plot(jsamples[["K"]])
    gelman.plot(jsamples[["qO"]])  # about 6-8000 runs required to converge
    
    geweke.plot(jsamples[["r"]])


  # ------------------
  # determine autocorrelation thinning
    j.coda = coda.samples(m, variable.names=c("K", "r", "qO", "qu"), n.iter=20000) # sample from posterior
    autocorr.plot(j.coda)  
    # plot(j.coda, ask=T)
    # autocorr(j.coda, lags = c(0, 1, 5, 10, 50, 100, 200, 400, 500), relative=TRUE)

  # final sampling from the posteriors
    jsamples = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior
		
   # fnres =  file.path( tempdir(), "logistic.mcmc.rdata" ) 
   # save(jsamples, file=fnres, compress=T)
    # load( fnres )

# AKA, "biomass dynamics model" or "Schaeffer" model 
      
# based upon equations in Hilborn & Wlaters (1992) - Chapter 8
# biomass dynamics model for snow crab (AKA, surplus production, production models)
# simple Schaefer (1954) formulation / extensions by Pella-Tomlinson (1969):
# basic equation (discrete): 
# 
# B(t+1) = B(t) + R(t) + G(t) - C(t) - M(t) 
# 
# where: t is time, B is biomass, R is recruitment, G is growth, C is catch and M is natural mortality
# 
# let: P = G + R; where P is production
#
# B(t+1) = B(t) + P(t) - M(t) ; when there is no catch (C)
#
# let: S  = P - M
# 
# where S = "surplus production", the production (biomass increase) in the absence of fishing 
# that is, the amount that can be caught while maintaining constant B (at equilibrium):
#
# B(t+1) = B(t) + S(t) - C(t) 
#
# or,
#
# S = B(t+1) - B(t) + C(t)
#
# Schaefer's formulation:
#
# dB/dt = rB(1- B/K) - C ; r is intrinsic rate of increase of B, K=unfished equilibrium stock size
# 
# assuming: 
# C(t) = q * E(t) * B(t) ; where q = "catchability", E is fishing effort 
#
# thus, O ~ U = C/E = qB  ; where O = observed index of abundance 
# U is catch per unit effort or any observed index of abundance 
#
# or B = C/E/q 
#
# ( Aside: Pella-Thomlinson adds an exponent (m) to the biomass term (B -> B^m) 
# such that if m=2 it becomes the Sahfer forumaltion)
#
# Estimation possible via:
# (1) equilibrium methods (unstable); 
# (2) Regression (Schnute 1977; erratic); 
# (3) Observation error or "time-series method" (recommended by Hilborn & Walters )
#
# using method (3a): using cpue or biomass index 
#
# B(t) = B(t-1) + r * B(t-1) * ( 1 - B(t-1) / K ) - C(t-1)
#      = B(t-1) * ( 1 + r * ( 1- B(t-1)/K ) - C(t-1) 
# O(t) = q * B(t)
# e(t) = (O(t) - <O(t)> )^2 ; where < > is the estimated value of CPUE or other abundance metric (eg survey) ::: "observation error"
# minimize( sum(e(t)) ) 
#
# or,
#
# method (3b): using effort only
# C(t) = B(t) * q * E(t)
# e(t) = (C(t) - <C(t)> ) ^2 ;  where < > is again the predicted/estimated value
#
# B(0) must be estimated or one can assume:
# B(1) = C(t) / ( E(1) * q ) ; or 
# B(1) = a running average of the start of the C(1..5, etc); or 
# B(0) == K (if starting with an unfished population .. 
#
# The following uses method (3a): in a bayesian approach

