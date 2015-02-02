
bottom.contact.gating.time = function( Zt, good, time.gate, min.n=50 ) {
  ## Preliminary gating: simple range limits (gating) of time stamps
  i = which( Zt < time.gate$t0 | Zt > time.gate$t1 )
  good0 = good
  if (length(i) > 0) good[i] = FALSE
  if (length( which( good)) < min.n )  good = good0
  return(good)

}


