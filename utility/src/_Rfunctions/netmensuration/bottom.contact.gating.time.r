
bottom.contact.gating.time = function( Zt, good, time.gate, min.n=50 ) {
  ## Preliminary gating: simple range limits (gating) of time stamps
  
  # only gate if both time stamps are within the data range
  # there are (many) cases where timestamps are incorrect due to errors in entry or timezone issues
  out = good
  Ztrange = range(Zt, na.rm=TRUE)
  if ( ( time.gate$t0 > Ztrange[1] & time.gate$t0 < Ztrange[2] ) & ( time.gate$t1 > Ztrange[1] & time.gate$t1 < Ztrange[2] ) ) {
    i = which( Zt < time.gate$t0 | Zt > time.gate$t1 )
    good0 = good
    if (length(i) > 0) good[i] = FALSE
    grange = range( Zt[ which(good) ], na.rm= TRUE )
    if ( difftime( grange[2], grange[1], units="mins" ) < min.n ) out = good
  }
  return(good)

}


