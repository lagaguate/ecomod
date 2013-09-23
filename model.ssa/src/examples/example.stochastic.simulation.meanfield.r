
# models of basic population dynamics using "patch models" sensu McKane et al ... 2004 ... etc.
# begin with simple probablilitic models of interactions and movemnet and then add van Kempen type expansion 
# to "mean fields" as the size of a patch increases to a large number
# AKA birth death models using master equations
 
# This method bridges mechanism to statistics and so is essentially similar to the statistical mechanics 
# approaches of physics and chemistry

# Originally developed as interactions between discrete individuals  .. can be generalized to "biomass"  
# --- ie and extensive variable

# Urn model or Markov process:
#

# Model 1: non-spatial birth-death of a signle population -- i.e., a single patch /urn 
#   governing equations:
#     A -> E              { death rate = r.death }
#     A + A -> A + E      { death/decline rate due to competition = r.competition }      
#     A + E -> A + A      { birth rate = r.birth }  

#    P.AA = U * n/N * (n-1)/(N-1)       # prob of picking AA 
#    P.AE = 2 * U* n/N * (N-n)/(N-1)   # prob of picking AE (double because AE and EA are possible)
#    P.A  = (1-U) * n/N                 # prob of picking A (single)
    
#    T.nS1_n = r.competition * P.AA + r.death * P.A   # T(n-1|n)
#    T.nA1_n = r.birth * P.AE                         # T(n+1|n)

# initial conditions:: P(n,0) = del_n  # all n0 individuals at a point location n0 at t=0 
# constraints :: T(N+1|N) = T(-1|0) = 0

# Master equation is:
# dP(n,t)/dt = T(n|n+1) P(n+1,t) + T(n|n-1) P(n-1,t) - T(n-1|n) P(n,t) - T(n+1|n) P(n,t)

# mean field (deterministic eq) : by taking the average ==> multiply by n and summing over all n 
# d<n>_dt = Sum {n=0,N} { T(n+1|n) P(n,t) } - Sum {n=0,N} {T(n-1|n) P(n,t) } 

# also as,  T(n|n+1) P(n+1,t) ->  T(n-1|n) P(n,t) 
#   and     T(n|n-1) P(n-1,t) ->  T(n+1|n) P(n,t) 

# define transition probabilities per unit time step  :: 
# "nS1" is used as "n-1"
# "nA1" is used as "n+1"

    ....

  let:
  
  m == <n> / N  (i.e. density normalized to "carrying capacity")   

  dm_dt = m*( [2*br-dr] - ( (2*br+cr)/N) * m)
        = m*(2*br-dr) * (1 - (2*br+cr)/N/(2*br-dr)* m)

  
  br = rate.birth * U / (N-1)
  cr = rate.competition * U / (N-1)
  dr = rate.death * (1-U ) / N
  
  r = (2*br-dr) 
  K = 1 / ( (2*br+cr)/N/(2*br-dr) )



#####################################


  
  # loadfunctions( "model.patch" )
  
  set.seed(2)

  rate.birth = 0.04
  rate.death = 0.03
  rate.competition = 0.01


  U = 0.5       # prob of picking a binary process

  n.replicates  = 10
  n.times = 10000  # total number of time steps
  
  N = array( 100, dim=c(n.replicates) )  # total n possible in the urn (i.e., capacity, carrying capacity)
  
  # mean field estimates
    br = rate.birth * U / (N-1)
    cr = rate.competition * U / (N-1)
    dr = rate.death * (1-U ) / N
    
    r = (2*br-dr) 
    K = 1 / ( (2*br+cr)/N/(2*br-dr) )

  n = array( 0, dim=c(n.replicates, n.times+1) )
  # n[,1] = runif(n.replicates, 0.25, 1.75 ) * N   # starting n
  n[,1] = round( K )  

  zeros = NULL


  for (ti in 1:n.times ) {
    
#     A -> E              { death rate = r.death }
#     A + A -> A + E      { death/decline rate due to competition = r.competition }      
#     A + E -> A + A      { birth rate = r.birth }  

    # selection of binary or unitary processes:
    u = rbinom( n.replicates, 1, U )
   
    # replicates with unitary processes (death)
    
      A = n[,ti]
      E = N-A
    
      rdeath = rate.death*A 
      rcompetition = rate.competition*A*A
      rbirth = 2 * rate.birth*A*E  ## 2 X due to AE and EA
     
    
    
    deaths = rbinom( n.replicates, 1, rdeath ) 

    # replicates with binary processes ( birth/growth )
    # selection of two elements from the population without replacement
    i1 = rbinom( n.replicates, 1, pmin(1, pmax(0, n[,ti]/N, na.rm=TRUE)) ) # first element 
    i2 = rbinom( n.replicates, 1, pmin(1, pmax(0, ( n[,ti]-1)/(N-1), na.rm=TRUE )) ) # second element 
    ii = i1 + i2

    births = (ii==1) * rbinom( n.replicates, 1, rbirth )
    competition = (ii==2) * rbinom( n.replicates, 1, rcompetition ) 

    dn = -(1-u)*deaths - u*competition + u*births
    
    n[,ti+1] = n[,ti] + dn
    
    zz = which( n[,ti+1] <= 0 ) 
    if (length(zz) > 0 ) {
      zeros = sort( unique( c( zeros, zz) ))
      n[zeros, ti+1] = 0
    }

  }


  plot( colMeans(n), type="l", ylim=range(n, na.rm=TRUE) )
  
  for (i in 1:n.replicates) {
    points( jitter(n[i,], amount=0.5), pch="." )
  }

  h = median( n[,n.times] ) 
  abline( h=h )
  abline( h=K)

  legend ("topright", legend=c( paste("median :", round(h)), paste("expected: ", round(K[1]) )) )

  (K[1]-h) / K[1]








#####################################3
  

# Two-species model
  
#   AA -> AE    {c11}  --- competition
#   AB -> AE    {c21}      interspecific competition
#   BA -> BE    {c12}
#   BB -> BE    {c22}
#   AE -> AA    {b1}   --- birth
#   BE -> BB    {b2}
#   A -> E      {d1}   --- death
#   B -> E      {d2}


# mean field equations:

  dNa / dt = Na * ( r1 - a11 * Na - a12 * Nb ) 
  dNb / dt = Nb * ( r2 - a21 * Na - a22 * Nb )

  # notation 
  v = {1,2} 
  vv = {11, 22}
  vw = {12, 21}

  r{v} = 2 * u * b{v} / (N-1) - (1-u) * d{v} / N   
  a{vv} = u * (2 * b{v} + c{vv} / ( N * (N-1) )
  a{vw} = 2 * u  * (b{v} + c{vw}) / (N * (N-1) )




# simulation starts

  set.seed(1)

  c11 = 0.1
  c21 = 0.05
  c12 = 0.05
  c22 = 0.1 
  b1  = 1
  b2  = 0.8
  d1  = 0.2
  d2  = 0.2
  
  U = 0.5       # prob of picking a binary process

  n.replicates  = 100
  n.times = 10000  # total number of time steps
  n.species = 2   # 1 is A, 2 is B , etc
  
  N = array( 1000, dim=c(n.replicates) )  # total n possible in the urn (i.e., capacity, carrying capacity)
  
  n = array( 0, dim=c(n.replicates, n.times+1, n.species) )  ## species A or 1

  n[,1,] = runif(n.replicates, 0.25, 0.75 ) * N   # starting n

  zeros = NULL

  for (ti in 1:n.times ) {
    
#     A -> E              { death rate = r.death }
#     A + A -> A + E      { death/decline rate due to competition = r.competition }      
#     A + E -> A + A      { birth rate = r.birth }  

    # selection of binary or unitary processes:
    u = rbinom( n.replicates, 1, U )
   
    # replicates with unitary processes (death)
    deaths = rbinom( n.replicates, 1, rate.death ) 

    # replicates with binary processes ( birth/growth )
    # selection of two elements from the population without replacement
    i1 = rbinom( n.replicates, 1, pmin(1, pmax(0, n[,ti]/N, na.rm=TRUE)) ) # first element 
    i2 = rbinom( n.replicates, 1, pmin(1, pmax(0, ( n[,ti]-1)/(N-1), na.rm=TRUE )) ) # second element 
    ii = i1 + i2

    births = (ii==1) * rbinom( n.replicates, 1, rate.birth )
    competition = (ii==2) * rbinom( n.replicates, 1, rate.competition ) 

    dn = -(1-u)*deaths - u*competition + u*births
    
    n[,ti+1] = n[,ti] + dn
    
    zz = which( n[,ti+1] <= 0 ) 
    if (length(zz) > 0 ) {
      zeros = sort( unique( c( zeros, zz) ))
      n[zeros, ti+1] = 0
    }

  }


  plot( colMeans(n), type="l", ylim=range(n) )
  for (i in 1:n.replicates) points( jitter(n[i,], amount=0.5), pch="." )

  h = median( n[,n.times] ) 
  abline( h=h )
  abline( h=K)

  legend ("topright", legend=c( paste("median :", round(h)), paste("expected: ", round(K[1]) )) )

  (K[1]-h) / K[1]



