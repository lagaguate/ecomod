
bottom.contact.smooth = function( sm, tdif.min, tdif.max, target.r2=0.9, filter.quants=c(0.025, 0.975) )  {
  ## Smooth method: smooth data to remove local trends and compute first derivatives 

  res = c(NA, NA)

  # use only the subset with data for this step
  names( sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed
  N = nrow(sm)

  sm$dZ = grad( approxfun( sm$ts, sm$Z ), sm$ts, method="simple" )
  ii = which( !is.finite( sm$dZ ) )
  sm$dZ[ii] = sm$dZ[ min(ii)-1 ]    
  # last element(s) are NA's .. copy the next to last value into it
  # Too noisy to use 2nd derivative ///
  # sm$ddZ = grad( approxfun( sm$ts, sm$dZ ), sm$ts, method="simple" )
  # sm$ddZ[N] = sm$ddZ[ N-1 ]    
  
  sm$dZ.smoothed.spline = interpolate.xy.robust( sm[, c("ts", "dZ")], target.r2=target.r2, probs=filter.quants, method="smooth.spline" )
  sm$dZ.smoothed.loess  = interpolate.xy.robust( sm[, c("ts", "dZ")],  target.r2=target.r2, method="loess"  )
   
  vrs = c( "dZ.smoothed.spline", "dZ.smoothed.loess" )
  cors = data.frame( vrs=vrs, stringsAsFactors=FALSE )
  cors$corel = 0

  for (v in 1:length(vrs)) {
    u = cor(  sm[,vrs[v] ], sm[, "dZ"], use="pairwise.complete.obs" )
    if (u>0.999) u = 0
    cors[ v, "corel"]  = u 
  }

  best = cors[  which.max( cors$corel), "vrs" ]
  sm$dZ.smoothed =  sm[,best] 

  dZ.modes = modes( sm$dZ.smoothed )
  # now using (smoothed) first derivative determine inflection points (crossing of the (near)-zero line)

  nb =  trunc( nrow(sm) / 4)
  hh = hist(  sm$dZ.smoothed, breaks =, plot=FALSE) 

  mm = trunc( N/2 )
  mmed = median( sm$Z, na.rm=TRUE )
  mmsd = sd( sm$Z, na.rm=TRUE )  ## SD 
  
  # left side
  if ( abs( sm$Z[1] - mmed ) < mmsd ) {
    # assuming it is truncated without the left tail ..
    k0 = 1
  } else {
    for( k0 in 1:mm) if ( sm$dZ.smoothed[k0] < dZ.modes["simple", "ub"] ) break()  ## note this is inverted on purpose (ub for lefttail)
  }

  if ( abs( sm$Z[N] - mmed ) < mmsd ) {
    # assuming it is truncated without the right tail ..
    k1 = N
  } else {
    for( k1 in N:mm) if ( sm$dZ.smoothed[ k1 ] >  dZ.modes["simple", "lb"]  )  break()  ## note this is inverted on purpose (lb for right tail)
  }

  duration =  as.numeric( difftime(sm$timestamp[k1], sm$timestamp[k0], units="mins" ) )
  if ( duration > tdif.min & duration < tdif.max ) res =  c( sm$timestamp[k0], sm$timestamp[k1] )    
  
  return(res)
}


