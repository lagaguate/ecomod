 
bottom.contact.filter.noise = function( x, good, bcp ) {

  ##--------------------------------
  # First, a simple high pass filter to remove noisy data (very large extreme fluctuations)
  # Then, truncation of data based upon variance of central region to permit use of more fine-scale methods 
  # to determine anomlaous data
  fr = range( which(good) )
  aoi = fr[1]:fr[2]
   
   
  # do not operate upon depth ... operate upon Z and Z.smoothed
  x$depth0 = x$depth
  x$Z = NA
  x$Z[aoi] = x$depth[aoi] 

  if ( exists( "double.depth.sensors", bcp) ) {
    oi  = interpolate.xy.robust( x[aoi, c("ts", "Z")], method="loess", trim=0.1, probs=c(0.05, 0.95),  target.r2=0.9 )
    oo = which( (x$Z - oi) > 0) 
    x$Z[oo] = NA
    x$depth[oo] = NA
  }

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
  outsideofaoi = setdiff(1:nrow(x), aoi )
  #x$Z[outsideofaoi] = NA
  x$depth[ outsideofaoi] =NA

  x$Z.smoothed = x$depth
  x$Z.smoothed[aoi] = x$sm.win[aoi]  # basis is the moving averge which is the most stable 
  x$Z.smoothed[ good ] = x$Z[good]  # these Z are the highest probability of good clean data .. return to the data for futher processing
  x$Z.smoothed[ good ] = interpolate.xy.robust( x[ good, c("ts", "Z.smoothed")], method="loess", 
                                               trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, target.r2=0.95 )
  x$Z.smoothed[outsideofaoi] = NA
 
  # return tails to improve fit of curvature at touchdown and lift-off
  leftt = 1 : min( which( good)) 
  rightt = max( which( good) ) : nrow(x)

  #dx = range( x$Z[good], na.rm=TRUE )
  #oo = which( x$depth0 > dx[2]  & x$depth0 < dx[1]  )
  #if (length(oo) > 0) x$depth0[ oo] = NA  # remove likely errors
  
  x$Z.smoothed[ leftt ] = interpolate.xy.robust( x[leftt, c("ts", "depth0")], method="loess",
                                                 trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, target.r2=0.9 )
  x$Z.smoothed[ rightt ] = interpolate.xy.robust( x[rightt, c("ts", "depth0")], method="loess",
                                                 trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, target.r2=0.9 )

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

# final tweaks .. order is important  

  # high freq noise still observed on a handful of cases .. smooth via running average again
  x$Z.smoothed = interpolate.xy.robust( x[, c("ts", "Z.smoothed")],  target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window, method="moving.window" )

  # high freq noise still observed on a handful of cases .. smooth via running average again
  x$Z.smoothed = interpolate.xy.robust( x[, c("ts", "Z.smoothed")],  target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window, method="smooth.spline" )

  return( list( depth.smoothed=x$Z.smoothed, good=good.final ) )
}

