 
bottom.contact.filter.noise = function( x, good, bcp ) {

  # First, a simple high pass filter to remove noisy data (very large extreme fluctuations)
  # Then, truncation of data based upon variance of central region to permit use of more fine-scale methods 
  # to determine anomlaous data

  # 1. check range of the data and recheck using modal filter of the bottom part of the "good" area
  fr = range( which(good) )
  aoi = fr[1]:fr[2]
  amod = modes( x$depth[ aoi] ) # first est of bottom
  abot = which (x$depth >= (amod$lb2 ) & x$depth <= ( amod$ub2 ) )
  x$Zdetrended = x$depth - predict( lm( depth ~ ts, data=x[abot,], na.action=na.omit ), newdata=x ) # remove linear trend 
  aoi.modes = modes( x$Zdetrended ) 
  aoi.bottom =  which ( x$Zdetrended>= 0.9*(aoi.modes$lb ) & x$Zdetrended <= 1.1*( aoi.modes$ub ) ) 
  aoi = min( aoi.bottom) :  max(aoi.bottom )  # redo as occasionally wrong
  Zupp = 0  # fictional surface value ..
  i = which( x$depth <= Zupp )
  if (length(i)> 0) good[i] = FALSE
  x$depth[ !good] = NA

  # do not operate upon depth any further ... operate upon Z and Z.smoothed
  x$Z = x$depth


  # 2. focus upon aoi: identify data that are likely noise and mark them
  #  filter localized (adjacent) deviations
  x$sm.seq = x$Z
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")], method="sequential.linear",  
      trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants )
  kk = x$Z - x$sm.seq
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ aoi[i]] = NA
  }

  # filter out based upon local-mean estimates
  x$sm.win = x$Z
  x$sm.win[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")], method="moving.window",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window )
  kk = x$Z - x$sm.win
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ aoi[i]] = NA
  }

  # filter out based upon local-variance estimates
  x$sm.var = x$sm.win  # use the moving average solution as a basis
  x$sm.var[aoi] = interpolate.xy.robust( x[aoi, c("ts", "sm.win")], method="local.variance",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window )
  kk = x$Z - x$sm.var
  i = which.quantile ( kk[aoi], probs=bcp$noisefilter.quants, inside=FALSE ) 
  if (length(i) > 0) {
    good[aoi[i]] = FALSE
    x$Z[ aoi[i]] = NA
  }

# 3. finalize id of good data and create a smooth based upon the good data
#   by this point, the Z are the highest probability of good clean data .. 
#   operate upon finding a smooth return them to the data for futher processing
  x$Z.smoothed = x$depth  # begin with raw data
  x$Z.smoothed[aoi] = x$Z[aoi] # replace bottom part with filtered data

# smooth the tails
  buff = 30
  leftt = 1 : (min(aoi+buff) )  # add a buffer
  rightt = (max(aoi)-buff  ) :  nrow(x)   # add a buffer
  if (length( leftt) > buff ) {
    # force tails to surface .. helps tails to go in right direction when not enough data retained
    x$Z.smoothed[leftt] = interpolate.xy.robust( x[leftt, c("ts", "Z.smoothed")],  method="sequential.linear", 
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants )
    x$Z.smoothed[1] = Zupp 
    x$Z.smoothed[leftt] = interpolate.xy.robust( x[leftt , c("ts", "Z.smoothed")], method="linear" )
    x$Z.smoothed[1] = Zupp 
    x$Z.smoothed[leftt] = interpolate.xy.robust( x[leftt , c("ts", "Z.smoothed")], method="loess" )
  }

  if (length( rightt) > buff ) {
    x$Z.smoothed[rightt] = interpolate.xy.robust( x[rightt, c("ts", "Z.smoothed")],  method="sequential.linear", 
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants )
    x$Z.smoothed[nrow(x)] = Zupp
    x$Z.smoothed[rightt] = interpolate.xy.robust( x[rightt, c("ts", "Z.smoothed")], method="linear" )
    x$Z.smoothed[nrow(x)] = Zupp
    x$Z.smoothed[rightt] = interpolate.xy.robust( x[rightt, c("ts", "Z.smoothed")], method="loess" )
  }

  tails = c( 1:(min(aoi)-1), (max(aoi)+1):nrow(x) ) 
  if (length( tails ) > 0 ) {
    kk = x$depth[tails] - x$Z.smoothed[tails]
    i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=FALSE ) 
    if (length(i) > 0 ) {
      good [ tails[i] ]  = FALSE
      x$Z[ tails[i] ] = NA
    }
  }

  # final smooth with all data back together ...
  sm = x[ , c("ts", "Z")]
  uu = which( !is.finite( sm$Z ))
  if (length(uu) >  1) {
    sm$Z[uu] = x$Z.smoothed[uu]
  }

  
 if (0) {
  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
   test = try( interpolate.xy.robust( sm[aoi,], method="sequential.linear", trim=0.025), silent=TRUE )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) sm$Z[aoi]=test 
  }

  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
    test = NULL
    test = try( interpolate.xy.robust( sm[aoi,], method="loess", trim=0.025, target.r2=0.95 ),  silent=TRUE ) 
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) sm$Z[aoi]=test 
  }

  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) { 
    test = NULL
    test = try( interpolate.xy.robust( sm[aoi,], method="moving.window", trim=0.025), silent=TRUE )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) sm$Z[aoi]=test 
  }
 }

  test = NULL
  sm$Z = try( interpolate.xy.robust( sm, method="sequential.linear", trim=0.025), silent=TRUE )
  sm$Z[1] = sm$Z[nrow(sm)] = Zupp 
  test = try( interpolate.xy.robust( sm, method="loess", trim=0.05), silent=TRUE )
  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
    if ( cor( sm$Z, test, use="pairwise.complete.obs" ) > 0.99 ) {
      test = try( interpolate.xy.robust( sm, method="loess", trim=0.1), silent=TRUE )
    }
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) sm$Z=test 
  }

  kk = x$depth - sm$Z
  i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) 
  if (length(i) > 0 ) {
    good[] = FALSE
    good[i]  = TRUE
    x$Z[!good] = NA
  }
  return( list( depth.smoothed=sm$Z, good=good, depth.filtered=x$Z ) )
}

