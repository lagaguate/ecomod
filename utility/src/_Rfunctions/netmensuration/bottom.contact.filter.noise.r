 
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
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, method="sequential.linear" )
  kk = x$Z - x$sm.seq
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ !good] = NA
  }

  # filter out based upon local-mean estimates
  x$sm.win = x$Z
  x$sm.win[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window, method="moving.window" )
  kk = x$Z - x$sm.win
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ !good] = NA
  }


  # filter out based upon local-variance estimates
  x$sm.var = x$sm.win  # use the moving average solution as a basis
  x$sm.var[aoi] = interpolate.xy.robust( x[aoi, c("ts", "sm.win")],  target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window, method="local.variance" )
  kk = x$Z - x$sm.var
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ !good] = NA
  }

 
  # filter out based on localized (adjacent) deviations on the moving average ... in case there are any left over 
  x$sm.seq = x$sm.win
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, method="sequential.linear" )
  kk = x$Z - x$sm.seq
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ !good] = NA
  }


# last step ... finalize id of good data and then smooth over the raw data
  
  x$Z.smoothed =x$sm.win  # basis is the moving averge which is the most stable 
  x$Z.smoothed[ good ] = x$Z[good]  # these Z are the highest probability of good clean data .. return to the data for futher processing
  x$Z.smoothed = interpolate.xy.robust( x[, c("ts", "Z.smoothed")], method="loess", target.r2=0.95, probs=0.01 )
  kk = x$depth - x$Z.smoothed
    i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) 
    if (length(i) > 0 ) {
      good.final = rep( FALSE, nrow(x) )
      good.final [ i]  = TRUE
      good.final = good.final | good  # or is adding
    }  
    
    x$depth[!good.final] = NA

    sminla = try( interpolate.xy.robust( x[, c("ts", "depth")],  target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, method="inla", inla.h=bcp$noisefilter.inla.h, inla.diagonal=bcp$inla.diagonal ), silent=TRUE )
    if ( ! class( sminla) %in% "try-error" ) { 
      x$Z.smoothed  = sminla
    } else { 
      x$Z.smoothed = interpolate.xy.robust( x[, c("ts", "depth")], method="loess", probs=bcp$noisefilter.quants,  target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim )
    }
    

  return( list( depth.smoothed=x$Z.smoothed, good=good.final ) )
}

