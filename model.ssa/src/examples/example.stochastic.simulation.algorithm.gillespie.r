 
#     A -> 0              { death rate = r.death }
#     A + A -> A       { death/decline rate due to competition = r.competition }      
#     A  -> A + A      { birth rate = r.birth }  


# Simulation using Gillespue algorithm of the master equation for simple birth-death process


  set.seed(2)

  rate.birth = 0.3
  rate.death = 0.1
  rate.competition = 0.01

  A.max = 75
  
  n.replicates  = 10
  n.times = 5000  # total number of time steps
  n.reactions = 3
  
  N = array( A.max, dim=c(n.replicates) )  # total n possible in the urn (i.e., capacity, carrying capacity)

  # Gillespie approach
    
  tim = array( 0, dim=c(n.replicates, n.times+1) )
  n = array( 0, dim=c(n.replicates, n.times+1) )
  # n[,1] = runif(n.replicates, 0.25, 1.75 ) * N   # starting n
  n[,1] = round( runif(n.replicates) * 1.5 * A.max )
  
  rdn = c(-1,-1,1)  # step sizes in each reaction: death, competition, birth 
  rn = rdn * 0  # number of reaction 

  for (ri in 1:n.replicates) {
    for (ti in 1:n.times ) {
      A = n[ri,ti]
      
      # number of reactions 
      rn[1] = rate.death*A 
      rn[2] = rate.competition*A*A
      rn[3] = rate.birth*A  ## 2 X due to AE and EA
      rtotal = sum(rn) # total number of reactions
      
      # time to next reaction 
      tau = rexp( 1, 1/rtotal )  #-- ie, assumption of a pure Poisson random process
      # alternate parameterization: r1 = runif(1); tau = (1/rtotal) * log(1/ r1 )
      tim[ri,ti+1] = tim[ri,ti] + tau
      
      rc = runif(1)*rtotal
      rci = findInterval( rc, cumsum(rn), rightmost.closed=TRUE  ) + 1
      n[ri,ti+1] = n[ri,ti] + rdn[rci]
    }
  }

  plot (0,0, xlim=c(0, max(tim)), ylim=c(0,max(n)) ) 

  for (ri in 1:n.replicates) {
    points( n[ri,] ~ tim[ri,], col=ri, pch="." ) 
  }



  
#     A -> 0              { death rate = r.death }
#     A + A -> A + E      { death/decline rate due to competition = r.competition }      
#     A -> A + A      { birth rate = r.birth }  


# now same as above using the GillespieSSA library
  require(GillespieSSA)

  rate.birth = 0.3
  rate.death = 0.1
  rate.competition = 0.01

  A.max = 75
  A0 = A.max
  x0 = c( A=A0[1] ) # starting state vector
  parms = c( rate.death=rate.death, rate.compeition=rate.competition, rate.birth=rate.birth  ) # per captia reaction probabilities per small increment of day
  a  = c("rate.death*A", "rate.competition*A*A", "2*rate.birth*A")   # propensities (relative number of possible reactions in a given time step 
  nu = matrix( c( 
    -1, -1, 1
  ), nrow=1, byrow=TRUE)  # rows are species (A,E), cols =reactions (death, comp, birth)
  tf = 365 # days

  out <- ssa( x0=x0, a=a, nu=nu, parms=parms, tf=tf )
  ssa.plot(out)
  legend( "topleft", legend= round( mean( out$data[, "A"], trim=.05 ),2) )
 



