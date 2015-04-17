
bottom.contact.smooth = function( sm, bcp )  {
  ## Smooth method: smooth data to remove local trends and compute first derivatives 

  res = list( bc0=NA, bc1=NA)

  if(0) {
    load("~/ecomod/groundfish/data/nets/Scanmar/bottom.contact/results/bc.NED2014102.8.rdata")
    sm =data.frame( Z=bc$Z)
    sm$timestamp=bc$timestamp
    sm$ts=bc$ts
    good = bc$good 
    sm$Z[ !good] = NA
    sm =bc$plotdata 
  }

  names( sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed
  N = nrow(sm)
  
  # expand tails a bit more:
  Z0 = modes( sm$Z )
  aoi.i = which( sm$Z > (Z0$lb2 + bcp$depth.range[1]/2) &  sm$Z < (Z0$ub2 + bcp$depth.range[2]/2 ) )
  
  ml0 = min(aoi.i + 5 ) ## move away from edge to avoid strange curvatures due to being on the tail
  mr0 = max(aoi.i - 5 )
  
  #sm$Z.smoothed = interpolate.xy.robust( sm[, c("ts", "Z")], 
  #      target.r2=bcp$smooth.target.r2, probs=bcp$smooth.filter.quants, method="moving.window" ) 
  
  sm$dZ = grad( approxfun( sm$ts, sm$Z, rule=2 ), sm$ts, method="simple" )

  # Too noisy to use 2nd derivative ///
  # sm$ddZ = grad( approxfun( sm$ts, sm$dZ ), sm$ts, method="simple" )
  # sm$ddZ[N] = sm$ddZ[ N-1 ]
  
  # remove high freq variation
  sm$dZ.smoothed = interpolate.xy.robust( sm[, c("ts", "dZ")], 
        target.r2=bcp$smooth.target.r2, probs=bcp$smooth.filter.quants, method="sequential.linear" ) 

  dZ.modes = modes( sm$dZ )
  ub =  dZ.modes$ub2
  lb =  dZ.modes$lb2 

  # now using (smoothed) first derivative determine inflection points (crossing of the (near)-zero line)
  iib = which( sm$dZ.smoothed < ub & sm$dZ.smoothed > lb )
  ib = min(iib): max(iib)

  ng = trunc( length(ib) / 10 ) # small chunk close to candidate area of interest
  ml = ib[ ng ] 
  mr = ib[ (length(ib) - ng) ] 

  #browser()

  mwin = - bcp$smooth.windowsize : bcp$smooth.windowsize
  
  # left side
  if ( abs( sm$Z[1] - Z0$mode ) < Z0$sd ) {
    # assuming it is truncated without the left tail ..
    k0 = 1
  } else {
    for( k0 in ml0:ml) {
      ri = k0+mwin
      ri = ri[ ri >= 1 & ri <=N ]
      nri = length(ri)
      if ( nri > 3 ) {
        nx = length( which( sm$dZ.smoothed[ri] < ub ) )
        prx = nx / nri
        if (prx > bcp$smooth.threshold) break()
      }
    }

  }

  if ( abs( sm$Z[N] - Z0$mode ) < Z0$sd ) {
    # assuming it is truncated without the right tail ..
    k1 = N
  } else {
    
    for( k1 in mr0:mr) {
      ri = k1+mwin
      ri = ri[ ri >= 1 & ri <=N ]
      nri = length(ri)
      if ( nri > 3 ) {
        nx = length( which( sm$dZ.smoothed[ri] > lb ) )
        prx = nx / nri
        if (prx > bcp$smooth.threshold) break()
      }
    }
  }

  res =  list( bc0=sm$timestamp[k0], bc1=sm$timestamp[k1] )    
  
  return(res)
}


