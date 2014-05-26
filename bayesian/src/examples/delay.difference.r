

--- incomplete model ..


tuna = data.frame( matrix( c ( 
1934,  60.9, 10361,
1935,  72.3, 11484,
1936,  78.4, 11571,
1937,  91.5, 11116,
1938,  78.3, 11463,
1939, 110.4, 10528,
1940, 114.6, 10609,
1941,  76.8,  8018,
1942,  42.0,  7040,
1943,  50.1,  8441,
1944,  64.9, 10019,
1945,  89.2,  9512,
1946, 129.7,  9292,
1947, 160.2,  7857,
1948, 207.0,  8353,
1949, 200.1,  8363,
1950, 224.8,  7057,
1951, 186.0, 10108,
1952, 195.3,  5606,
1953, 140.0,  3852,
1954, 140.0,  5339,
1955, 140.9,  8191,
1956, 177.0,  6507,
1957, 163.0,  6090,
1958, 148.5,  4768,
1959, 140.5,  4982,
1960, 244.3,  6817,
1961, 230.9,  5544,
1962, 174.1,  4120,
1963, 145.5,  4368,
1964, 203.9,  4844,
1965, 180.1,  4166,
1966, 182.3,  4513,
1967, 178.9,  5292
), ncol=3, byrow=T) )


colnames(tuna) = c("yr", "catch", "cpue") 

 
  spdat = list(
    # r0 = 1,
    K0 = 20000, # carrying capacity estimate
    B0 = 0.5 # initial est of unobserved/true Biomass (scaled to 0,K) 
 )

 

  tacs = as.matrix( data.frame( 
      tn=rep( 0.5, 5 )
  ))
  
  sb = list( 
    r0 = c(1),
    K0 = c(5),  # carrying capacity estimate
    b0 = c(0.9), # initial est of unobserved/true Biomass (scaled to 0,K) 
    O = tuna$cpue, 
    C = tuna$catch,
    # mv0 =c(0,0,0),  # between region movement 0=input from gulf, 1=ntosouth, 2= south to 4x,3=4x loss 
    TAC = tacs  # TACS for prediction scenarios
  )


  require (rjags)
  # require(coda)
  
  loadfunctions( c("common", "bayesian" ) )




  m = jags.model( file=jags.setup( delay.difference.jags ), data=sb, n.chains=3, n.adapt=5000 )  

  coef(m)
  rjags::load.module("dic")
  # rjags::load.module("glm")
  
  (m.dic = dic.samples(m, n.iter=5000))
    
  tomonitor = c("b", "r", "K", "q", "MSY", "BMSY", "FMSY", "Fcrash", "F", "P", "collapse", "oSD", "pSD", "KSD", "rSD", "tac" )
  
  # as mcmc/coda object
  
  y = jags.samples(m, variable.names=tomonitor, n.iter=100000, thin = 100) # sample from posterior
     
  b = jags.extract( y, "b", mean )
  K = jags.extract( y, "K", mean ) 


  ... etc



delay.difference.jags ="
# based on approach and data in Meyer and Millar 1999. CJFAS 56:37-52
# growth: w[a] = u + v * w[a-1]; a=k, k+1, ...
#   mean body weight at age, w[a] increases linearly (Ford-Walford plot)
#   for all fish aged k and older: equal vulnerability to fishing (knife-edged selection) and equal natural mortality rate:
# survival: s[t] = S * (1-h[t])
#   where S=constant background natural survival rate, 
#   and h[t] is harvest rate at time t
#   assuming disjunct fishing and natural mortality periods (ie. short fishing season)
# recruitment: R[t] = number of fish reaching age k or weight w[k] in year t
#   assumed ~ Spawning stock biomass[t-k+1] = total biomass of age k and older after harvest in year t
#   or even other additional years
# B[t] = SUM { N[a,t] * w[a] } ; for a=k, ...
#      = SUM { N[a,t] * w[a] } + R[t]*w[k] ; for a=k+1, ... ie, splitting off the recruits
#   as, N[a,t] = s[t-1] * N[a-1,t-1] and w[a] = u+v*w[a-1] 
# B[t] = s[t-1]*u*N[t-1] + s[t-1]*v*B[t-1] + w[k]*R[t]
#   and
# N[t] = s[t]*N[t-1] + R[t]
# resulting in: 
# B[t] = s[t-1]*B[t-1] + { v*s[t-1]*B[t-1]  - v*s[t-1]*s[t-2]*B[t-2] - (w[k]-u)*s[t-1]*R[t-1] } + w[k]*R[t]
#      = surviving biomass + {growth of survivors} + new recruits biomass
# but in snow crab growth is terminal ... ! how to adjust ?

data {
  Odim <- dim(O)
  Tdim <- dim(TAC)
  N <- Odim[1] # no years with data  
  M <- Tdim[1] # no years for projections  
  R <- length(r0) # number of regions
  eps <- 1e-4  # small non-zero number
}

model {
  # -------------------  
  # hyperpriors of precision terms 
  # for these lognormal precions, assume shape = 3 (modal) with mean ~ CV 20% =sqrt(log(1+0.2^2))=0.2; 
  # or prec= 1/0.2 =5; ==> 3/0.6 = 5 
    for (j in 1:R) {
      KTAU[j] ~ dgamma(3, 0.6); # target CV ~ 0.2::   
      pTAU[j] ~ dgamma(3, 0.6); 
      oTAU[j] ~ dgamma(3, 0.6); 
      rTAU[j] ~ dgamma(3, 0.6); # target CV ~ 0.2:: curve( dgamma(x, shape=3, rate=0.5 ), 0, 10 ) 
    }
  
  # -------------------  
  # priors of key stochastic nodes for estimation
    for (j in 1:R) {
      r[j] ~ dnorm( r0[j], rTAU[j] ) T(0,) ; # try a CV of ~0.2   # curve( dnorm(x, 1, 0.2 ), 0, 3 )
      K[j] ~ dlnorm( log(K0[j]), KTAU[j] ) ; # try a CV of ~0.2  # curve( dlnorm(x, log(5), 0.2 ), 0, 10 ),  curve( dlnorm(x, log(60), 0.2 ), 0, 100 ), curve( dlnorm(x, log(1), 0.2 ), 0, 4 )
      q[j] ~ dunif (0.1, 10 ) ;  
    }

  # -------------------  
  # standardize catches and TACs, etc
    for (j in 1:R) {
      c[1:N,j] <- C[1:N,j] / K[j]
      # tac[1:M,j] <- TAC[1:M,j]/ K[j]
      
      for (i in (N+1):(N+M)) {
        tac[i-N,j] <- 0.2 * b[i-1,j]   # fixed at 20% ER of last year's biomass
      }
    
    }

  # -------------------  
  # observation model for biomass index and observation error 
    for (j in 1:R) {
    for (i in 1:N) { 
      O[i,j] ~ dlnorm( log(q[j] * K[j] * b[i,j]), oTAU[j] );
    }}

  # -------------------  
  # biomass model and process error 
    for(j in 1:R) {
      b[1,j] ~ dlnorm( log( b0[j]), pTAU[j] ) ; # biomass at first year 
      for(i in 2:N) {
        b[i,j] ~ dlnorm( log( max(b[i-1,j]*( 1 + r[j]*(1-b[i-1,j])) - c[i-1,j] , eps)), pTAU[j] ) ; 
    }}

  # -------------------  
  # forecasts
    for(j in 1:R) {
    for(i in (N+1):(N+M)) {
      b[i,j] ~ dlnorm( log( max(b[i-1,j]* (1 + r[j]*(1-b[i-1,j])) - tac[i-N,j] , eps)), pTAU[j] ) ; 
    }}

  # -------------------  
  # monitoring nodes and parameter estimates for output
    KSD <- 1/sqrt(KTAU) ;
    rSD <- 1/sqrt(rTAU) ;
    pSD <- 1/sqrt(pTAU); 
    oSD <- 1/sqrt(oTAU);
      
    for(j in 1:R) {
      collapse[j] <- 1 - step( b[N+M,j]-0.1 ) ; # test if b >= 0.1; collapse defined as less than 10% of K
      MSY[j] <- r[j] * K[j] / 4  # maximum height of of the latent productivity (yield)
      BMSY[j] <- K[j]/2  # biomass at MSY
      FMSY[j] <- 2 * MSY[j] / K[j] # fishing mortality at MSY
      Fcrash[j] <- 4 * MSY[j] / K[j] # fishing mortality at which the stock will crash
    }

    # -------------------  
    # fishing mortality
    # force first year estimate assuming catches in year 0 to be similar to year 1 
    for(j in 1:R) {
      F[1,j] <- -log( 1 - c[1,j] / (b[1,j] + c[1,j]) ) 
      for(i in 2:N) {
        F[i,j] <- -log(1 - c[i-1,j] / (b[i,j] + c[i-1,j]) ) 
      }
      for(i in (N+1):(N+M)) {
        F[i,j] <- -log(1 - tac[i-N,j] / (b[i,j] + tac[i-N,j]) ) 
      }
    }

    # -------------------  
    # annual production
    for(j in 1:R) {
      P[1,j] <- b[2,j]- b[1,j] + c[1,j]
      for (i in 2:(N-1) ){
        P[i,j] <- (b[i+1,j]- b[i-1,j])/2 + c[i,j]  # linear interpolation cancels out the b[i,j] term
      }
      P[N,j] <- b[N,j]- b[N-1,j] + c[N,j]
    }

}

"



