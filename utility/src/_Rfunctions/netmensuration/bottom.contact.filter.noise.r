 
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


# last step ... finalize id of good data and then smooth over the raw data
  outsideofaoi = setdiff(1:nrow(x), aoi )
  #x$Z[outsideofaoi] = NA

  x$Z.smoothed = x$depth
  x$Z.smoothed[ outsideofaoi] =NA

  x$Z.smoothed[aoi] = x$sm.win[aoi]  # basis is the moving averge which is the most stable 
  x$Z.smoothed[ good ] = x$Z[good]  # these Z are the highest probability of good clean data .. return to the data for futher processing
  x$Z.smoothed[ good ] = interpolate.xy.robust( x[ good, c("ts", "Z.smoothed")], method="loess", 
      trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, target.r2=0.95 )
  x$Z.smoothed[outsideofaoi] = NA
  
  kk = x$depth - x$Z.smoothed
  i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) 
  if (length(i) > 0 ) good [ i]  = TRUE

  # return tails to improve fit of curvature at touchdown and lift-off
  leftt = 1 : (min( which( good) ) - 1)    
  rightt = ( 1 + max( which( good) ) ): nrow(x)

  x$ztails = x$depth 
  x$ztails = interpolate.xy.robust( x[, c("ts", "ztails")], method="sequential.linear",  
      trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants )
  x$ztails = interpolate.xy.robust( x[, c("ts", "ztails")], method="moving.window",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window )

  x$ztails = interpolate.xy.robust( x[, c("ts", "ztails")], method="local.variance",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window )

  x$ztails[1] = x$ztails[nrow(x)] = 0# force tails to surface .. helps tails to go in right direction when not enough data retained

  x$Z.smoothed[ leftt ] = interpolate.xy.robust( x[leftt, c("ts", "ztails")], method="loess",
      trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, target.r2=0.5 )   # make it a bit smoother than data
  
  x$Z.smoothed[ rightt ] = interpolate.xy.robust( x[rightt, c("ts", "ztails")], method="loess",
      trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, target.r2=0.5 )

  kk = x$depth - x$Z.smoothed
  i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) 
  if (length(i) > 0 ) good [ i]  = TRUE

 
  x$depth[!good] = NA
  x$depth[1] = x$depth[nrow(x)] = 0 # force tails to surface
# TEL2004529.20 TEL2004529.34
  
  sminla = try( interpolate.xy.robust( x[, c("ts", "depth")], method="inla",
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, 
      inla.h=bcp$noisefilter.inla.h, inla.diagonal=bcp$inla.diagonal ), silent=TRUE )
  if ( ! class( sminla) %in% "try-error" ) { 
    x$Z.smoothed  = sminla
  } else { 
    x$Z.smoothed = interpolate.xy.robust( x[, c("ts", "depth")], method="loess", 
        probs=bcp$noisefilter.quants,  target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim )
  }

# final tweaks .. order is important  

  # high freq noise still observed on a handful of cases .. smooth via running average again
  x$Z.smoothed = interpolate.xy.robust( x[, c("ts", "Z.smoothed")], method="moving.window", 
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window )

  # high freq noise still observed on a handful of cases .. smooth via running average again
  x$Z.smoothed = interpolate.xy.robust( x[, c("ts", "Z.smoothed")], method="smooth.spline",
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window )

  kk = x$depth - x$Z.smoothed
  i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) 
  if (length(i) > 0 )     good [ i]  = TRUE

  return( list( depth.smoothed=x$Z.smoothed, good=good ) )
}

