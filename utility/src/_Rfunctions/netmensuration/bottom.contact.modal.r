  
bottom.contact.modal = function( sm, bcp ) {
  # Modal method: gating by looking for modal distribution and estimating sd of the modal group  
  # until a target number of breaks, nbins with valid data are found
  # see density and bw.SJ for more options

  res = c(NA, NA)

  names(sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed
  N = nrow(sm) 
  if ( N < 50 ) return(res)  # insufficient data
 
  # first pass to crudely determine location of mode
  Zmodes0 = modes( sm$Z )

  # create a variable with any linear or quadratic trend in the depths removed as this can increase the precision of the method
  ib = which( sm$Z > Zmodes0["mean", "lb"] & sm$Z < Zmodes0["mean", "ub"] )
  ib.range = max(ib) - min(ib)
  ib.guess = min(ib) : max(ib)
  botlm = lm( Z ~ ts +I(ts^2) + I(ts^3), data= sm[ ib.guess, ], na.action="na.omit")  
  sm$Zr = sm$Z - predict( botlm, newdata=sm )

  #second pass using the initial estimates as a filter .. operating on residuals
  Zrmodes0 = modes( sm$Zr[ib.guess] ) 

  r0 =1
  r1 =N
  # catch situations where there is no variance in data due to only bottom-track being recorded and truncated: 
  # no discrimination possible
  if ( !is.finite(Zrmodes0[ "mean", "sd" ]) || Zrmodes0[ "mean", "sd" ] < 1e-3) {
    oo = which ( abs( sm$Zr - Zrmodes0[ "mean", "mode" ] ) < 1 )  # anything within 1 m of the "mode" 
    if (length(oo) > 50) {
      rr = range(oo)
      res = c( sm$timestamp[rr[1]], sm$timestamp[rr[2]] )
    }
    return(res) 
  }
 
  #final pass using the initial estimates as a filter .. operating on residuals
  
  mwin = -bcp$modal.windowsize: bcp$modal.windowsize

  i.mod0 = which( sm$Zr > Zrmodes0["mean", "lb"] & sm$Zr < Zrmodes0["mean", "ub"]   ) 
  Zrmodes = modes( sm$Zr[i.mod0] ) 
  i.mod = which( sm$Zr > Zrmodes["mean", "lb"] & sm$Zr < Zrmodes["mean", "ub"]   ) 

  if (length(i.mod) > 0 ) {
    if ( abs( sm$Zr[1] - Zrmodes[ "mean", "mode" ] ) <   bcp$modal.sd.multiplier* Zrmodes[ "mean", "sd" ] ) {
      r0 = 1 # nothing to do .. assuming it is truncated without the left tail ..
    } else {
      for ( r0 in min(i.mod):1 ){
        ri = r0+mwin
        ri = ri[ ri >= 1 & ri <=N ]
        nri = length(ri)
        if ( nri > 3 ) {
          nx = length( which( sm$Zr[ri] < Zrmodes["mean", "lb"] | sm$Zr[ri] < Zrmodes["mean", "ub"] )  )
          prx = nx / nri
          if (prx > bcp$modal.threshold) break()
        }
      }
    }
    if ( abs( sm$Zr[N] - Zrmodes[ "mean", "mode" ] ) <   bcp$modal.sd.multiplier * Zrmodes[ "mean", "sd" ] ) {
      r1 = N # nothing to do ... assuming it is truncated without the right tail ..
    } else {
      for ( r1 in max(i.mod):N ){
        ri = r1+mwin
        ri = ri[ ri >= 1 & ri <=N ]
        nri = length(ri)
        if ( nri > 3 ) {
          nx = length( which( sm$Zr[ri] < Zrmodes["mean", "lb"] | sm$Zr[ri] < Zrmodes["mean", "ub"] )  )
          prx = nx / nri
          if (prx > bcp$modal.threshold) break()
        }
      }
    }
  }

  res = c( sm$timestamp[r0], sm$timestamp[r1] )

  return(res)
}

