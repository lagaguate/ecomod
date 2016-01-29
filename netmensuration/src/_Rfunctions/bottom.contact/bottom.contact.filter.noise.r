bottom.contact.filter.noise = function( x, good, bcp ) {

  ##--------------------------------
  # First, a simple high pass filter to remove noisy data (very large extreme fluctuations)
  # Then, truncation of data based upon variance of central region to permit use of more fine-scale methods 
  # to determine anomlaous data
  fr = range( which(good) )
  aoi = fr[1]:fr[2]
   
  # do not operate upon depth ... operate upon Z and Z.smoothed
  x$Z = NA
  x$Z[aoi] = x$depth[aoi] 

  # filter out based on localized (adjacent) deviations
  x$sm.seq = x$Z
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")], method="sequential.linear",  
      trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants )
  kk = x$Z - x$sm.seq
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ !good] = NA
  }

  # filter out based upon local-mean estimates
  x$sm.win = x$Z
  x$sm.win[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")], method="moving.window",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window )
  kk = x$Z - x$sm.win
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ !good] = NA
  }


  # filter out based upon local-variance estimates
  x$sm.var = x$sm.win  # use the moving average solution as a basis
  x$sm.var[aoi] = interpolate.xy.robust( x[aoi, c("ts", "sm.win")], method="local.variance",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window )
  kk = x$Z - x$sm.var
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ !good] = NA
  }

 
  # filter out based on localized (adjacent) deviations on the moving average ... in case there are any left over 
  x$sm.seq = x$sm.win
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")], method="sequential.linear",  
      trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants )
  kk = x$Z - x$sm.seq
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ !good] = NA
  }

  # direct range limits of best idea of "bottom"
  i = which.quantile ( x$Z, probs=bcp$noisefilter.quants, inside=FALSE )   ## assymetrical trim:: trimming shallow end
  if (length(i) > 0) {
    good[i] = FALSE
    x$Z[ !good] = NA
  }


# last step ... finalize id of good data and then smooth over the raw data
# return tails to improve fit of curvature at touchdown and lift-off
  rgood = range( which( good) ) 
  ngood = abs( diff( rgood)  )
  buf = trunc(ngood/5)
  
  leftt = max( 1, trunc(fr[1] - fr[1]/2) ) : (min(which(good)) + buf)  # add a buffer
  rightt = (max(which(good)) - buf ) : min( fr[2] + trunc( (nrow(x)-fr[2])/2 ), nrow(x) )   # add a buffer

  # these Z are the highest probability of good clean data .. return to the data for futher processing
  x$Z.smoothed = x$Z
  
  x$sm2 = x$depth
  uu = which( is.finite( x$Z))
  if (length(uu) >  1) {
    dr = range( uu)
    vv = min(dr):max(dr)
    x$sm2[vv] = x$Z[vv]
  }

  x$sm2[leftt] = interpolate.xy.robust( x[leftt, c("ts", "sm2")],  method="sequential.linear", 
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants )
  x$sm2[leftt[1]] = 0 # force tails to surface .. helps tails to go in right direction when not enough data retained
  x$sm2[leftt] = interpolate.xy.robust( x[leftt, c("ts", "sm2")],  method="loess", 
        probs=bcp$noisefilter.quants, target.r2=0.5, trim=0.25 )
 
  x$sm2[rightt] = interpolate.xy.robust( x[rightt, c("ts", "sm2")],  method="sequential.linear", 
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants )
  x$sm2[rightt[length(rightt)] ] = 0 # force tails to surface .. helps tails to go in right direction when not enough data retained
  x$sm2[rightt] = interpolate.xy.robust( x[rightt, c("ts", "sm2")],  method="loess", 
        probs=bcp$noisefilter.quants, target.r2=0.5, trim=0.25 )
 
  kk = x$depth - x$sm2
  i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0 ) {
    good [ i]  = FALSE
    x$sm2[ i ] = NA
  }



 # zdr = range( x$Z, na.rm=TRUE )  + c(-5, +5) * sd( x$Z, na.rm=TRUE )  
 # iz  = which( x$Z.smoothed < zdr[1] | x$Z.smoothed > zdr[2]  )
  
 # if (length( iz>0))  x$Z.smoothed[ iz] = 0
  x$Z.smoothed[leftt] = x$sm2[leftt] 
  x$Z.smoothed[rightt] = x$sm2[rightt] 
  x$Z.smoothed[1] = x$Z.smoothed[nrow(x)] = 0# force tails to surface .. helps tails to go in right direction when not enough data retained
  x$Z.smoothed = interpolate.xy.robust( x[, c("ts", "Z.smoothed")], method="simple.linear" )
  
  kk = x$depth - x$Z.smoothed
  i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) # NOTE inside=TRUE here
  if (length(i) > 0 ) {
    good [ i]  = TRUE
    # x$Z.smoothed[ !good ] = NA
  }


  # construct filtered depth vector (Z) with smoothed estimates where not good
  x$ZZ  = x$Z.smoothed
  uu = which( is.finite( x$Z))
  if (length(uu) >  1) {
    dr = range( uu)
    vv = min(dr):max(dr)
    x$ZZ[vv] = x$Z[vv]
  }

  sm = x$ZZ

  sm = try( interpolate.xy.robust( x[, c("ts", "ZZ")], method="inla",
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, 
      inla.h=bcp$noisefilter.inla.h, inla.diagonal=bcp$inla.diagonal ), silent=TRUE )
  
  if ( class( sm ) %in% "try-error" ) { 
    sm = try( interpolate.xy.robust( x[, c("ts", "ZZ")], method="loess", 
        probs=bcp$noisefilter.quants,  target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim ),  silent=TRUE )
  }
  if ( class( sm ) %in% "try-error" ) { 
    sm = try( interpolate.xy.robust( x[, c("ts", "ZZ")], method="smooth.spline", 
        target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window ),  silent=TRUE )
  }
  if ( class( sm ) %in% "try-error" ) { 
    sm = try( interpolate.xy.robust( x[, c("ts", "ZZ")], method="moving.window", 
        target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window ),  silent=TRUE )
  }
 
  kk = x$depth - x$sm
  i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) 
  if (length(i) > 0 ) {
    good[] = FALSE
    good[i]  = TRUE
  }

  return( list( depth.smoothed=sm, good=good ) )
}