
modes = function( Z, density.factor=5, kernal.bw.method="sj"  ) {
  # find the largest mode 
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
  Z = Z[ which(is.finite(Z) ) ]
  N = length(Z) 
  nb = min( 50, trunc(N/4) )
  Zh = hist( Z, breaks=nb, plot =FALSE)
  Zh.max = max( Zh$counts )
  Zh.min = min( Zh$counts[ Zh$counts>0 ] )
  Zh.rare.data = min(1, Zh$counts[ Zh$counts > 0 & Zh$counts < median(Zh$counts) ], na.rm=TRUE )
  Zh.med = median( Zh.rare.data )  ## "random" low number count ~ EPS ... density factor X EPS == significantly large number 
  
  Zh.threshold = Zh.med * density.factor

  Z.peak = which.max(Zh$counts)
  Z.mode = Zh$mids[ Z.peak ]
  for ( Zlb in Z.peak:1) if ( Zh$counts[ Zlb ] < Zh.threshold ) break() # left
  for ( Zub in Z.peak:length(Zh$counts) ) if ( Zh$counts[ Zub ] < Zh.threshold ) break() # left
  Zlb = max( 1, Zlb , na.rm=TRUE )
  Zub = min( length(Zh$counts), Zub, na.rm=TRUE )

  Z.mode.group.i = Zlb:(Zub+1) 
  Z.mode.group = range( Zh$breaks[ Z.mode.group.i ])
  Z.mode.sd = sd( Z[ Zlb:Zub ] , na.rm=TRUE )  ## SD 
  res = data.frame( cbind( mode=Z.mode, sd=Z.mode.sd, lb=Z.mode.group[1], ub=Z.mode.group[2] ))


  # kernel density based approach
  require(numDeriv)

  u = try( density(Z, kernel="gaussian", bw=kernal.bw.method ) , silent = TRUE )
  
  if ("try-error" %in% class(u) )  {
    rownames(res) = c("simple")
    return(res)
  }

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


  ## hybrid approach using smoothed kernal density rather than counts
  u.max = max( u$y )
  u.min = min( u$y[ u$y>0 ] )
  u.rare.data = min( 1/N,  u$y[ u$y > 0 & u$y < median(u$y ) ] , na.rm=TRUE )
  u.med = median( u.rare.data  )  ## "random" low number ~ EPS ... density factor X EPS == significantly large number 
   
  u.threshold = u.med * density.factor

  Zu.peak = which.max(u$y)
  Zu.mode = u$x[ Zu.peak ]
  for ( Zulb in Zu.peak:1) if ( u$y[ Zulb ] < u.threshold ) break() # left
  for ( Zuub in Zu.peak:length(u$y) ) if ( u$y[ Zuub ] < u.threshold ) break() # left
 
  Zulb = max( 1, Zulb - 1, na.rm=TRUE )
  Zuub = min( length(u$y), Zuub + 1, na.rm=TRUE )


  Zu.mode.group.i = Zulb:(Zuub+1) 
  Zu.mode.group = range( u$x[ Zu.mode.group.i ])
  Zu.mode.sd = sd( u$y[ Zulb:Zuub ] , na.rm=TRUE )  ## SD 
  
  res = rbind( res, cbind( mode=Zu.mode, sd=Zu.mode.sd, lb=Zu.mode.group[1], ub=Zu.mode.group[2] ))
  res = rbind( res, rep(NA, 4) )

  rownames(res) = c("simple", "kerneldensity", "hybrid", "mean")
  for (i in 1:4 ) {
    res["mean", i] = mean( res[,i], trim =0.1, na.rm=TRUE )   
  }

  return(res)

}



