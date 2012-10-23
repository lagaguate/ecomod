
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

  biomass.logistic.jags = "
"
data {
  N <- length(O)
  M <- length(TAC)
  eps <- 1e-4  # small non-zero number
}

model {
 
  # hyper-priors
    rTAU ~ dgamma(3,3); #shape = 3 (ie. modal ) with mean = 3 * 1/0.6 = 50 ; mean should be ~ 1
    KTAU ~ dgamma(3,0.0002);  # K mean should be ~ 3/0.0002 = 15000 (with scallop)
    bTAU ~ dgamma(3,6); # modal, with mean ~ 3/6 = 0.5   
    oTAU ~ dgamma(0.001,0.001); # modal, with mean ~ 1
 
  # priors
    r ~ dnorm( 1, rTAU ); 
    K ~ dlnorm( log(K0), KTAU ) ; 
    q ~ dunif(0,10);
    
    k <- 1/K;
    # KSD <- 1/sqrt(KTAU) ;
    # rSD <- 1/sqrt(rTAU) ;
    # bSD <- 1/sqrt(bTAU); # SD of biomass
    # oSD <- 1/sqrt(oTAU);

  # observation model 
    for (i in 1:N) { 
      Omean[i] <- log(q * K * B[i]) ;
      O[i] ~ dlnorm( Omean[i], oTAU );  # observation error
    }
   
  # biomass model - normalised logistic geometric mean annual biomass 
    Bmean[1] ~ dnorm( log(B0), bTAU ) ; # ~ value of K
    for(i in 2:N) {
      Bmean[i] <- log( max(B[i-1]*( 1 + r*(1-B[i-1])) - k*C[i-1], eps));
    }
    for(i in 1:N) {
      B[i] ~ dlnorm(Bmean[i], bTAU ) ; # process or model error
    }

  # predictions
    for(i in (N+1):(N+M)) {
      Bmean[i] <- log( max(B[i-1]* (1 + r*(1-B[i-1])) - k*TAC[i-N], eps)); # biomass predictions
      B[i] ~ dlnorm(Bmean[i], bTAU ) ; # process or model error
    }

  # monitoring nodes, estimates for output
    collapse <- 1 - step( B[N+M]-0.1 ) ; # test if B >= 0.1; collapse defined as less than 10% of K, M years into the future
    MSY <- r * K / 4  # maximum height of of the latent productivity (yield)
    BMSY <- K/2  # biomass at MSY
    FMSY <- 2 * MSY / K # fishing mortality at MSY
    Fcrash <- 4 * MSY / K # fishing mortality at which the stock will crash

    # fishing mortality
    F[1] <- -log( 1 - k*C[1] / ( B[1] + k*C[1] ) ) # force first estimate assuming catches in year 0 to be similar to year 1 
    for(i in 2:N) {
      F[i] <- -log(1 - k*C[i-1] / ( B[i] + k*C[i-1] ) ) 
    }
    for(i in (N+1):(N+M)) {
      F[i] <- -log(1 - k*TAC[i-N] / ( B[i] + k*TAC[i-N] ) ) 
    }

    # annual production
    P[1] <- B[2]- B[1] + C[1]*k
    for (i in 2:(N-1) ){
      P[i] <- (B[i+1]- B[i-1])/2 + C[i]*k  # linear interpolation
    }
    P[N] <- B[N]- B[N-1] + C[N]*k

}
"


