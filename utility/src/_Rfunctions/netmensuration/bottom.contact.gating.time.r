
bottom.contact.gating.time = function( Zt, good, time.gate ) {
  
  ## Preliminary gating: simple range limits (gating) of time stamps
  i = which( Zt < time.gate$t0 | Zt > time.gate$t1 )
  
  if (length(i) > 0) good[i] = FALSE
  return(good)

}


