 
bottom.contact.filter.noise = function( x, good, bcp ) {

  # First, a simple high pass filter to remove noisy data (very large extreme fluctuations)
  # Then, truncation of data based upon variance of central region to permit use of more fine-scale methods 
  # to determine anomlaous data

  # do not operate upon depth (keep as reference)... operate upon Z and Z.smoothed
  x$Z = x$depth
  nx = nrow(x)

  # 1. check range of the data and recheck using modal filter of the bottom part of the "good" area
  fr = range( which(good) )
  aoi = fr[1]:fr[2]
  mm = modes( x$depth[aoi] )
  mm.aoi =  which ( x$depth >= mm$lb2 & x$depth <= mm$ub2  ) # fisrt estimate of bottom
  
  # remove linear trend to make bottom contact times more precise
  zlm = predict( lm ( depth ~ ts, x[mm.aoi,], na.action=na.exclude ) )
  zdetrended = zlm - x$depth[mm.aoi]
  zd = modes ( zdetrended )
  mm.good = which( zdetrended >= zd$lb2 & zdetrended <= zd$ub2 )  
  mm.bad = setdiff( 1:length(mm.aoi), which( zdetrended >= zd$lb2 & zdetrended <= zd$ub2 )  )

  if ( length( mm.good ) > 0 ) {
    good[ mm.aoi[mm.good] ] = TRUE
    good[ setdiff( 1:nx,  mm.aoi[mm.good]) ] = FALSE 
  }

  if (length( mm.bad )) {
    bad = range ( mm.aoi[mm.bad], na.rm=TRUE )
    if (length( which( is.finite( bad) ) ) ==2 ) {
      good[ c(1:bad[1], bad[2]:nx) ] = FALSE
    }
  }

  aoi = min( mm.aoi ) : max( mm.aoi )
  if (length( aoi) < 30 ) return (NULL)

  Zupp = bcp$depth.range[1] + mm$mode  # fictional surface value ..

  # 2. focus upon aoi: identify data that are likely noise and mark them
  #  filter localized (adjacent) deviations
  tm0 = x[aoi, c("ts", "Z")]
 
  tm = tm0
  tm$Z[ !good[aoi]] = NA
  test = try( interpolate.xy.robust( tm, method="sequential.linear",  
      trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants ), silent =TRUE  )
  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
    kk = x$Z[aoi] - test
    i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=FALSE ) 
    j = setdiff(1:nx , i )
    if (length(i) > 0) good[aoi[i]] = FALSE
    if (length(j) > 0) good[aoi[j]] = TRUE
    x$Z[ !good] = NA
  }

  # filter out based upon local-mean estimates
  tm = tm0
  tm$Z[ !good[aoi] ] = NA
  test = try( interpolate.xy.robust( tm, method="moving.window",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window ), silent =TRUE  )
  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
    kk = x$Z[aoi] - test
    i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=FALSE ) 
    j = setdiff(1:nx, i )
    if (length(i) > 0) good[aoi[i]] = FALSE
    if (length(j) > 0) good[aoi[j]] = TRUE
    x$Z[ !good] = NA
  }

  # filter out based upon local-variance estimates
  tm = tm0
  tm$Z[ !good[aoi] ] = NA
  test = try( interpolate.xy.robust( tm, method="local.variance",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window ) )
  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
    kk = x$Z[aoi] - test
    i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=FALSE ) 
    j = setdiff(1:nx , i )
    if (length(i) > 0) good[aoi[i]] = FALSE
    if (length(j) > 0) good[aoi[j]] = TRUE
    x$Z[ !good] = NA
  } 


  # filter out based upon local-mean estimates
  tm = tm0
  tm$Z[ !good[aoi] ] = NA
  test = try( interpolate.xy.robust( tm, method="linear",  
      target.r2=bcp$noisefilter.target.r2, trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window ), silent =TRUE  )
  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
    kk = x$Z[aoi] - test
    i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=FALSE ) 
    j = setdiff(1:nx, i )
    if (length(i) > 0) good[aoi[i]] = FALSE
    if (length(j) > 0) good[aoi[j]] = TRUE
    x$Z[ !good] = NA
  }


# 3. finalize id of good data and create a smooth based upon the good data
#   by this point, the Z are the highest probability of good clean data .. 
#   operate upon finding a smooth return them to the data for futher processing
  x$Z.smoothed = x$depth  # begin with raw data
  x$Z.smoothed[!good[aoi]] = NA


# smooth the tails
  buff = 5 ## ~ sec
  leftt = fr[1] : (min(aoi+buff) )  # add a buffer
  rightt = (max(aoi)-buff  ) :  fr[2]   # add a buffer
  tails = c( fr[1]:min(aoi), max(aoi):fr[2] ) 
  
  if (length( leftt) > buff ) {
    # force tails to surface .. helps tails to go in right direction when not enough data retained
    x$Z.smoothed[ fr[1] ] = Zupp 
    test = try( interpolate.xy.robust( x[leftt, c("ts", "Z.smoothed")],  method="sequential.linear",
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants ), silent =TRUE  )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 3)  ) ) x$Z.smoothed[leftt] = test
 
    x$Z.smoothed[ fr[1] ] = Zupp 
    test = try( interpolate.xy.robust( x[leftt, c("ts", "Z.smoothed")],  method="linear",
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants ), silent =TRUE  )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 3)  ) ) x$Z.smoothed[leftt] = test
 
    x$Z.smoothed[ fr[1] ] = Zupp 
    test = try( interpolate.xy.robust( x[leftt, c("ts", "Z.smoothed")],  method="moving.window",
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants ), silent =TRUE  )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 3)  ) ) x$Z.smoothed[leftt] = test
    
  }

  if (length( rightt ) > buff ) {
    x$Z.smoothed[ fr[2] ] = Zupp
    test = try( interpolate.xy.robust( x[rightt, c("ts", "Z.smoothed")],  method="sequential.linear", 
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants ), silent =TRUE )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 3)  ) ) x$Z.smoothed[rightt] = test
     
    x$Z.smoothed[ fr[2] ] = Zupp
    test = try( interpolate.xy.robust( x[rightt, c("ts", "Z.smoothed")],  method="linear", 
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants ), silent =TRUE )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 3)  ) ) x$Z.smoothed[rightt] = test
    
    x$Z.smoothed[ fr[2] ] = Zupp
    test = try( interpolate.xy.robust( x[rightt, c("ts", "Z.smoothed")],  method="moving.window", 
        trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants ), silent =TRUE )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 3)  ) ) x$Z.smoothed[rightt] = test
      
  }

  if (length( tails ) > 0 ) {
    kk = x$depth[tails] - x$Z.smoothed[tails]
    i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=FALSE ) 
    if (length(i) > 0 ) good [ tails[i] ]  = FALSE
    j = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) 
    if (length(j) > 0 ) good [ tails[j] ]  = TRUE

  }

  # final smooth with all data back together ...
  sm = x[ , c("ts", "Z")] 
  
  uu = which( !good)
  if (length(uu) >  1) {
    sm$Z[uu] = x$Z.smoothed[uu]
  }

  test = try( interpolate.xy.robust( sm, method="loess", trim=0.01 ), silent=TRUE )
  if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
    sm$Z = test
    sm$Z[ fr ] = Zupp 
  }
 
  if ( cor( sm$Z, x$Z, use="pairwise.complete.obs" ) > 0.99 ) {
    sm$Z = jitter(sm$Z)
    test = try( interpolate.xy.robust( sm, method="loess", trim=0.05), silent=TRUE )
    if ( ! ( class( test ) %in% "try-error" | ( length( which(is.finite(test))) < 30)  ) ) {
      sm$Z=test 
    }
  }

  kk = x$depth - sm$Z
  i = which.quantile ( kk, probs=bcp$noisefilter.quants, inside=TRUE ) 
  if (length(i) > 0 ) good[i]  = TRUE
  x$Z[good] = x$depth [good] 
  j = setdiff(1:nx, i )
  if (length(j) > 0) good[j] = FALSE
 
  return( list( depth.smoothed=sm$Z, good=good, depth.filtered=x$Z ) )
}

