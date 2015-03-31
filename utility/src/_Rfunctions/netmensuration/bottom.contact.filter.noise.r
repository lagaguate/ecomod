 
bottom.contact.filter.noise = function( x, good, bcp ) {

  out = NULL

  ##--------------------------------
  # First, a simple high pass filter to remove noisy data (very large extreme fluctuations)
  # Then, truncation of data based upon variance of central region to permit use of more fine-scale methods 
  # to determine anomlaous data
  OinRange = c(NA, NA)
  OinRange.indices = NA

  fr = range( which(good) )
  aoi = fr[1]:fr[2]
   
  # do not operate upon depth ... operate upon Z and Z.smoothed
  x$Z = NA
  x$Z[aoi] = x$depth[aoi]

  # filter out based on localized (adjacent) deviations
  x$sm.seq = x$Z
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, method="sequential.linear" )
  kk = x$Z - x$sm.seq
  qnts = quantile( kk[aoi], probs=bcp$noisefilter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - bcp$noisefilter.eps.depth )  
  qnts[2] = max( qnts[2],   bcp$noisefilter.eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  x$Z[ !good] = NA

  if (0) {
    points(depth ~ ts, x[ good,], col="orange",pch=20, cex=0.5 )
    points(depth ~ ts, x[ !good,], col="black",pch=20, cex=1 )
  }


  ## -----------------------------------
  ## A variance based-criterion for gating 
  # compute SD in the area of interest and compare with a lagged process to 
  # start from centre and move left and continue until sd of residuals begins to deviate sustantially
  # from centre to left 
 
  aoi.range = range( which( good )  )
  aoi.min = aoi.range[1]
  aoi.max = aoi.range[2]
  aoi.mid = trunc( mean( aoi.range ) ) # approximate midpoint
  aoi = aoi.min:aoi.max

  # determine approximate midpoint of the bottom track
  depths.threshold = mean( x$Z[aoi], na.rm=TRUE ) ## some deep value to initiate search for modal depth
  aoi.depths = x$Z[aoi]
  depths = modes( aoi.depths [ which( aoi.depths > depths.threshold )]  )
  oo = which( x$Z > depths["simple", "lb"] & x$Z < depths["simple", "ub"] )
  if (length(oo)> 10 ) {
    bot.range = range( oo) ## first pass (fast/rough) estimate of likely range of bottom indices
    aoi.mid = trunc( mean( range( oo)) ) 
  } 
    
  aoi.sd = sd( x$Z, na.rm=TRUE )  ## SD 
  buffer = trunc(length(aoi)/10) # additional points to add beyond midpoint to seed initial SD estimates
  if(any((aoi.mid-buffer)<1)) buffer = aoi.mid-1 #keep the index positive
  duration = 0 
  
  target.sd = aoi.sd * bcp$noisefilter.sd.multiplier
  Ndata0 = length(aoi)
  Ndata = 0 
  
  while (Ndata < Ndata0 ) {
    for ( j0 in aoi.mid:aoi.min  ) {#  begin from centre to right 
      sdtest = sd(( x$Z[ (aoi.mid + buffer):j0]), na.rm=T)
      if ( is.na(sdtest) ) next()
      if ( sdtest  >= target.sd ) break()
    }

    for ( j1 in aoi.mid: aoi.max ) {  #  begin from centre to right
       sdtest =  sd(( x$Z[ (aoi.mid - buffer):j1]), na.rm=T)
      if ( is.na(sdtest) ) next()
      if ( sdtest >= target.sd ) break()
    }
    duration = as.numeric( x$timestamp[j1] - x$timestamp[j0]) 
    if ( is.finite(duration) && duration > (bcp$tdif.min ) & duration < (bcp$tdif.max)  ) {  
      OinRange = c( x$timestamp[j0], x$timestamp[j1] )
      OinRange.indices = which( x$timestamp >= OinRange[1] &  x$timestamp <= OinRange[2] )
      OinRange.indices.not = which( x$timestamp < OinRange[1] |  x$timestamp > OinRange[2] )
      if (length(OinRange.indices.not)>0) good[ OinRange.indices.not ] = FALSE
      break()
    }  
    Ndata = length(unique( c( aoi.mid:j1, j0:aoi.mid) ) )
  }



  # filter out based upon local-variance estiamtes
  x$sm.var = x$Z
  x$sm.var[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  trim=bcp$noisefilter.trim, mv.win=bcp$noisefilter.var.window, method="local.variance" )
  kk = x$Z - x$sm.var
  qnts = quantile( kk[aoi], probs=bcp$noisefilter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - bcp$noisefilter.eps.depth )  
  qnts[2] = max( qnts[2],   bcp$noisefilter.eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  x$Z[ !good] = NA

  if (0) {
    points(depth ~ ts, x[ good,], col="orange",pch=20, cex=0.5 )
    points(depth ~ ts, x[ !good,], col="black",pch=20, cex=1 )
  }

  # do this a second time .. 
  # filter out based on localized (adjacent) deviations
  x$sm.seq = x$Z
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  trim=bcp$noisefilter.trim, probs=bcp$noisefilter.quants, method="sequential.linear" )
  kk = x$Z - x$sm.seq
  qnts = quantile( kk[aoi], probs=bcp$noisefilter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - bcp$noisefilter.eps.depth )  
  qnts[2] = max( qnts[2],   bcp$noisefilter.eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  x$Z[ !good] = NA

  if (0) {
    points(depth ~ ts, x[ good,], col="orange",pch=20, cex=0.5 )
    points(depth ~ ts, x[ !good,], col="black",pch=20, cex=1 )
  }



  ## ------------------------------
  #  filter data using some robust methods that look for small-scaled noise and flag them
  
  x$Z[ !good] = NA
  x$Z.smoothed = x$Z
  x$sm.loess = x$sm.inla = NA

  x$sm.inla[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  target.r2=bcp$noisefilter.target.r2, probs=bcp$noisefilter.quants, method="inla", inla.h=bcp$noisefilter.inla.h, inla.diagonal=bcp$inla.diagonal )
  kk = x$Z - x$sm.inla
  qnts = quantile( kk[aoi], probs=bcp$noisefilter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - bcp$noisefilter.eps.depth )  # eps m fluctuation as being insignificant error
  qnts[2] = max( qnts[2],   bcp$noisefilter.eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  
  
  x$sm.loess[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  target.r2=bcp$noisefilter.target.r2, method="loess"  )
  kk = x$Z - x$sm.loess
  qnts = quantile( kk[aoi], probs=bcp$noisefilter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - bcp$noisefilter.eps.depth )  
  qnts[2] = max( qnts[2],   bcp$noisefilter.eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
 
  vrs = c( "sm.loess", "sm.inla")
  cors = data.frame( vrs=vrs, stringsAsFactors=FALSE )
  cors$corel = 0
  for (v in 1:length(vrs)) {
    u = cor(  x[ good, vrs[v] ], x[ good, "Z"], use="pairwise.complete.obs" )
    cors[ v, "corel"]  = u 
  }
  best = cors[ which.max( cors$corel ), "vrs" ]
  
  x$Z.smoothed = x[,best] 
  if (all(is.na( x$Z.smoothed[aoi]) ) )  x$Z.smoothed = x$Z # give up

  # last go: based upon mode of differences from smoothed data
  kk = x$Z - x$Z.smoothed
  qnts = quantile( kk[aoi], probs=bcp$noisefilter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - bcp$noisefilter.eps.depth )  # eps m fluctuation as being insignificant error
  qnts[2] = max( qnts[2],   bcp$noisefilter.eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  ii =  which(kk < qnts[2]  & kk > qnts[1] )
  if (length(ii) > 0) good[ii] = TRUE
  x$Z[ !good] = NA


  # this is the last step where all uncertain data have been removed and we redo an interpolating model using inla
  x$Z.smoothed[aoi] = interpolate.xy.robust( x[aoi, c("ts", "Z")],  target.r2=bcp$noisefilter.target.r2.final, probs=bcp$noisefilter.quants, method=bcp$noisefilter.method, inla.h=bcp$noisefilter.inla.h, inla.diagonal=bcp$inla.diagonal )
  kk = x$depth - x$Z.smoothed
  qnts = quantile( kk[aoi], probs=bcp$noisefilter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - bcp$noisefilter.eps.depth )  # eps m fluctuation as being insignificant error
  qnts[2] = max( qnts[2],   bcp$noisefilter.eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  ii =  which(kk < qnts[2]  & kk > qnts[1] )
  if (length(ii) > 0) good[ii] = TRUE

  return( list( depth.smoothed=x$Z.smoothed, good=good, variance.method=OinRange, variance.method.indices=OinRange.indices ) )

}

