
params.size.taxa = function(P, tDist="poisson", tMean=5, tSD=1, tDecayC=4 ) {
  # number of taxa in each size-bin 
  # determine biomass size classes (g wet weight, midpoints on a log scale)
  # declare number of compartments within each size compartment
  # and then populate them
  P$tDist = tDist
  P$tMean = tMean
  P$tSD = tSD
  P$tDecayC = tDecayC

  # define the exponential distribution locally as it is not used elsewhere
  exponential.distribution = function(n, num0, base, decay, sd ) {
    out = rep(1, n)
    out[1] = num0
    for (i in 2:n) out[i] = floor( out[i-1] * (base^decay) )
    return( out)
  }

  n0 = switch( P$tDist,
      uniform=rep(P$tMean, n=P$wN),
      randomuniform=P$tMean * runif(n=P$wN) + 1,
      poisson=rpois( lambda=P$tMean, n=P$wN ) ,
      normal=floor( rnorm( n=P$wN, mean=P$tMean, sd=P$tSD ) ) ,
      exponential=exponential.distribution( n=P$wN, num0=100, base=P$wBase, sd=P$tSD, decay=P$tDecayC ) # num0 number at the smallest size
    )
    n0 [ n0<0 ] = 1
    P$size = c( rep( P$w, n0 ), NA, NA )  # the 2 NA's are for Autotrophs and Detritus
    P$sizeX = P$wBase^P$size
    P$sizeN = length( P$sizeX )
  return(P)
}
 

