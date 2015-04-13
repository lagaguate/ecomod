
bottom.contact.smooth = function( sm, bcp )  {
  ## Smooth method: smooth data to remove local trends and compute first derivatives 

  res = c(NA, NA)

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
  
  Zmodes0 = modes( sm$Z )

  # create a variable with any linear or quadratic trend in the depths removed as this can increase the precision of the method
  ib = which( sm$Z > Zmodes0["mean", "lb"] & sm$Z < Zmodes0["mean", "ub"] )
  ib.range = max(ib) - min(ib)
  ib.guess = min(ib) : max(ib)
  botlm = lm( Z ~ ts +I(ts^2) + I(ts^3), data= sm[ ib.guess, ], na.action="na.omit")  
  sm$Z = sm$Z - predict( botlm, newdata=sm )

  sm$dZ = grad( approxfun( sm$ts, sm$Z ), sm$ts, method="simple" )
  ii = which( !is.finite( sm$dZ ) )
  sm$dZ[ii] = sm$dZ[ min(ii)-1 ]    
  # last element(s) are NA's .. copy the next to last value into it

  # Too noisy to use 2nd derivative ///
  # sm$ddZ = grad( approxfun( sm$ts, sm$dZ ), sm$ts, method="simple" )
  # sm$ddZ[N] = sm$ddZ[ N-1 ]
  
  sm$dZ.smoothed = interpolate.xy.robust( sm[, c("ts", "dZ")], 
        target.r2=bcp$smooth.target.r2, probs=bcp$smooth.filter.quants, method=bcp$smooth.dZ.method, 
        inla.h=bcp$noisefilter.inla.h, inla.diagonal=bcp$noisefilter.inla.diagonal )
 
  dZ.modes = modes( sm$dZ.smoothed )

  # now using (smoothed) first derivative determine inflection points (crossing of the (near)-zero line)
  mm = trunc( N/2 )
  ml = trunc(mm/3) # 1/6
  mr = trunc(N-mm/3) # #/6 
  mmed = median( sm$Z, na.rm=TRUE )
  mmsd = 2 * sd( sm$Z, na.rm=TRUE )  ## SD 

  ub =  dZ.modes["mean", "ub"] 
  lb =  dZ.modes["mean", "lb"] 

  mwin = -bcp$smooth.windowsize : bcp$smooth.windowsize
  # left side
  if ( abs( sm$Z[1] - mmed ) < mmsd ) {
    # assuming it is truncated without the left tail ..
    k0 = 1
  } else {
    for( k0 in ml:1) {
      ri = k0+mwin
      ri = ri[ ri >= 1 & ri <=N ]
      nri = length(ri)
      if ( nri > 3 ) {
        nx = length( which( sm$dZ.smoothed[ri] > ub ) )
        prx = nx / nri
        if (prx > bcp$smooth.threshold) break()
      }
    }

  }

  if ( abs( sm$Z[N] - mmed ) < mmsd ) {
    # assuming it is truncated without the right tail ..
    k1 = N
  } else {
    
    for( k1 in mr:N) {
      ri = k1+mwin
      ri = ri[ ri >= 1 & ri <=N ]
      nri = length(ri)
      if ( nri > 3 ) {
        nx = length( which( sm$dZ.smoothed[ri] < lb ) )
        prx = nx / nri
        if (prx > bcp$smooth.threshold) break()
      }
    }
  }

  res =  c( sm$timestamp[k0], sm$timestamp[k1] )    
  
  return(res)
}


