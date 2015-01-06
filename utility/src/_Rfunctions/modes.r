
modes = function( Z, density.factor=5, kernal.bw.method="SJ-ste"  ) {
  
  debug = FALSE
  if (debug) {
    # construct something that looks like a profile 
    Z = - 50 * cos( seq(-pi/2, pi/2, by=.02) )
    mid = trunc( length(Z) / 2 )
    Z = c( Z[1:mid], Z[mid] + ( runif( length(Z) )-0.5)*5, Z[mid:length(Z)] )  
    Z = max(Z) -Z
    plot(Z)
    modes( Z)
  }

  
  # simple determination from histogram
  N = length(Z) 
  nb = trunc(N/5)
  Zh = hist( Z, breaks=nb, plot =FALSE)
  Zh.min = min( Zh$counts[ Zh$counts>0 ] )
  Zh.threshold = density.factor * Zh.min  # heuristically density.factor=5 seems to work well ... ~ chi-squared argument for  nonrandom frequencies
  
  Z.peak = which.max(Zh$counts)
  Z.mode = Zh$mids[ Z.peak ]
  for ( Zlb in Z.peak:1) if ( Zh$counts[ Zlb ] < Zh.threshold ) break() # left
  for ( Zub in Z.peak:length(Zh$counts) ) if ( Zh$counts[ Zub ] < Zh.threshold ) break() # left

  # adjust locations
  Zlb = Zlb + 1 
  Zub = Zub - 1

  Z.mode.group.i = Zlb:(Zub+1) 
  Z.mode.group = range( Zh$breaks[ Z.mode.group.i ])
  Z.mode.sd = sd( Z[ Zlb:Zub ] , na.rm=TRUE )  ## SD 
  res = data.frame( cbind( mode=Z.mode, sd=Z.mode.sd, lb=Z.mode.group[1], ub=Z.mode.group[2] ))

  # kernel density based approach
  require(numDeriv)
  u = density(Z,  kernel="gaussian", bw=kernal.bw.method )
  dZ  = numDeriv::grad( approxfun( u$x, u$y ), u$x, method="simple" )
  dZ[ length(dZ) ] = dZ[ length(dZ)-1 ]
  ddZ = numDeriv::grad( approxfun( u$x, dZ ), u$x, method="simple" )     
  ddZ[ length(ddZ) ] = ddZ[ length(ddZ)-1 ]
  
  # inflection pts as lb, ub :: ddZ's maxima 
  eps = sd( ddZ ) / 4
  # peak1 
  p1 = which.max( ddZ)
  for ( p1lb in p1:1) if ( ddZ[p1lb] <= eps ) break() # left
  for ( p1ub in p1:length(ddZ) ) if ( ddZ[p1ub] <= eps ) break() # right

  ddZ[ p1lb:p1ub] = 0
  # remove p1 and find peak2
  p2 = which.max ( ddZ ) 
  for ( p2lb in p2:1) if ( ddZ[p2lb] <= eps ) break() # left
  for ( p2ub in p2:length(ddZ) ) if ( ddZ[p2ub] <= eps ) break() # right

  inflection.x = range( c( p1lb, p1ub, p2lb, p2ub ) )
  inflection.y = u$x[ inflection.x ]
  kd.mode = u$x[ which.max( u$y ) ]
  
  kd.sd = sd(Z[Z>=inflection.y[1] & Z<=inflection.y[2] ] )

  res = rbind(res,  cbind( mode=kd.mode, sd=kd.sd, lb=inflection.y[1], ub=inflection.y[2] ))
  
  # use the simple histogram method to verify the kernel density method 
  # and use the latter unless it deviates too much from the hsimple method
  if ( abs(res[2,"mode"] - res[1, "mode"] > res[1, "sd"] ) ) {
    optimal = 1  # too strong a difference ... assume simple is better
  } else {
    optimal = 2 # kerneldensity
  }
  res = rbind( res, res[optimal,] )
  rownames(res) = c("simple", "kerneldensity", "optimal")

  return(res)

}



