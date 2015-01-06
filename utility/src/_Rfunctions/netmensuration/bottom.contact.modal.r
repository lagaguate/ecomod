  
bottom.contact.modal = function( sm, tdif.min, tdif.max, density.factor=5, kernal.bw.method="SJ-ste" ) {
  
  # Modal method: gating by looking for modal distribution and estimating sd of the modal group in the 
  # data specified by the indices, aoi
  # until a target number of breaks, nbins with valid data are found
  # see density and bw.SJ for more options

  res = c(NA, NA)

  names(sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed
  Zmodes = modes( sm$Z, density.factor=density.factor, kernal.bw.method=kernal.bw.method )
  N = nrow(sm) 

  r0 =1
  r1 =N

  # catch situations where there is no variance in data due to only bottom-track being recorded and truncated: 
  if ( Zmodes[ "optimal", "sd" ] < 1e-3) {
    # no discrimination possible, so default to start and end indices and test for acceptable time duration .. use defaults
    duration = as.numeric( difftime( sm$timestamp[r1],  sm$timestamp[r0], units="mins" ) )
    if ( duration > tdif.min & duration < tdif.max ) res = c( sm$timestamp[r0], sm$timestamp[r1] )
    return(res)
  } 
 
  i.mod = which( sm$Z > Zmodes["optimal", "lb"] & sm$Z < Zmodes["optimal", "ub"]   ) 
  if (length(i.mod) > 0 ) {
    if ( abs( sm$Z[1] - Zmodes[ "optimal", "mode" ] ) < Zmodes[ "optimal", "sd" ] ) {
      r0 = 1 # nothing to do .. assuming it is truncated without the left tail ..
    } else {
      r0 = min( i.mod ) 
    }
    if ( abs( sm$Z[N] - Zmodes[ "optimal", "mode" ] ) <  Zmodes[ "optimal", "sd" ] ) {
      r1 = N # nothing to do ... assuming it is truncated without the right tail ..
    } else {
      r1 = max( i.mod )
    }
  }

  duration = as.numeric( difftime( sm$timestamp[r1],  sm$timestamp[r0], units="mins" ) )
  if ( duration > tdif.min & duration < tdif.max ) res = c( sm$timestamp[r0], sm$timestamp[r1] )

  return(res)
}

