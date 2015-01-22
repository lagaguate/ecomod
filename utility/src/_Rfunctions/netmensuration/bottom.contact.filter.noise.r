 
bottom.contact.filter.noise = function( x, good, tdif.min, tdif.max, eps.depth=3,  
      smoothing = 0.9, filter.quants=c(0.025, 0.975), sd.multiplier=3 ) {

  ##--------------------------------
  # First, a simple high pass filter to remove noisy data (very large extreme fluctuations)
  # Then, truncation of data based upon variance of central region to permit use of more fine-scale methods 
  # to determine anomlaous data
  OinRange = c(NA, NA)
  OinRange.indices = NA

  fr = range( which(good) )
  aoi = fr[1]:fr[2]
   
  x$sm.seq = NA
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth")],  probs=filter.quants, method="sequential.linear" )
  i = which( x$sm.seq != x$depth )
  if (length(i) > 0) good[i] = FALSE
  x$depth[ !good] = NA
  #if (plot.data) points(depth ~ ts, x[ good,], col="orange",pch=20, cex=0.2 )


  ## -----------------------------------
  ## A variance based-criterion for gating 
  # compute SD in the area of interest and compare with a lagged process to 
  # start from centre and move left and continue until sd of residuals begins to deviate sustantially
  # from centre to left 
 
  aoi.range = range( which( good )  )
  aoi.min = aoi.range[1]
  aoi.max = aoi.range[2]
  aoi = aoi.min:aoi.max

  # determine approximate midpoint of the bottom track
  depths.threshold = mean( x$depth[aoi], na.rm=TRUE ) ## some deep value to initiate search for modal depth
  aoi.depths = x$depth[aoi]
  depths = modes( aoi.depths [ which( aoi.depths > depths.threshold )]  )
  oo = which( x$depth > depths["simple", "lb"] & x$depth < depths["simple", "ub"] )
  
  aoi.mid = trunc( mean( aoi.range ) ) # approximate midpoint
  if (length(oo)> 10 ) {
    bot.range = range( oo) ## first pass (fast/rough) estimate of likely range of bottom indices
    aoi.mid = trunc( mean( range( oo)) ) 
  } 
    

  aoi.sd = sd( x$depth[ aoi ], na.rm=TRUE )  ## SD 
  buffer = trunc(length(aoi)/10) # additional points to add beyond midpoint to seed initial SD estimates
  if(any((aoi.mid-buffer)<1)) buffer = aoi.mid-1 #keep the index positive
  duration = 0 
  
  target.sd = aoi.sd * sd.multiplier
  Ndata0 = length(aoi)
  Ndata = 0 
  
  while (Ndata < Ndata0 ) {
    for ( j0 in aoi.mid:aoi.min  ) {#  begin from centre to right 
      sdtest = sd(( x$depth[ (aoi.mid + buffer):j0]), na.rm=T)
      if ( is.na(sdtest) ) next()
      if ( sdtest  >= target.sd ) break()
    }

    for ( j1 in aoi.mid: aoi.max ) {  #  begin from centre to right
       sdtest =  sd(( x$depth[ (aoi.mid - buffer):j1]), na.rm=T)
      if ( is.na(sdtest) ) next()
      if ( sdtest >= target.sd ) break()
    }
    duration = as.numeric( x$timestamp[j1] - x$timestamp[j0]) 
    if ( duration > (tdif.min ) & duration < (tdif.max)  ) {  
      OinRange = c( x$timestamp[j0], x$timestamp[j1] )
      OinRange.indices = which( x$timestamp >= OinRange[1] &  x$timestamp <= OinRange[2] )
      OinRange.indices.not = which( x$timestamp < OinRange[1] |  x$timestamp > OinRange[2] )
      if (length(OinRange.indices.not)>0) good[ OinRange.indices.not ] = FALSE
      break()
    }  
    Ndata = length(unique( c( aoi.mid:j1, j0:aoi.mid) ) )
  }


  ## ------------------------------
  #  filter data using some robust methods that look for small-scaled noise and flag them
  
  x$depth[ !good] = NA
  x$depth.smoothed = x$depth
  x$sm.loess = x$sm.inla = x$sm.spline= NA

  x$sm.inla[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth.smoothed")],  target.r2=smoothing, probs=filter.quants, method="inla"  )
  kk = x$depth - x$sm.inla
  qnts = quantile( kk[aoi], probs=filter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - eps.depth )  # eps m fluctuation as being insignificant error
  qnts[2] = max( qnts[2],   eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  x$depth.smoothed[ !good] = NA  # i.e. sequential deletion of depths
  
  
  x$sm.loess[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth.smoothed")],  target.r2=smoothing, method="loess"  )
  kk = x$depth - x$sm.loess
  qnts = quantile( kk[aoi], probs=filter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - eps.depth )  # hard code 1 m fluctuation as being insignificant error
  qnts[2] = max( qnts[2],   eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  x$depth.smoothed[ !good] = NA
  
  # update with bad data removed .. first with sequential method again to id strong discontinuities and then re-smooth
  x$sm.seq = NA
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth.smoothed")],  probs=filter.quants, method="sequential.linear" )
  i = which( x$sm.seq != x$depth.smoothed )
  if (length(i) > 0) good[i] = FALSE
  x$depth.smoothed[ !good] = NA
  
  x$sm.inla[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth.smoothed")],  target.r2=smoothing, probs=filter.quants, method="inla"  )
  x$sm.loess[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth.smoothed")],  target.r2=smoothing, method="loess"  )
  

  vrs = c( "sm.loess", "sm.inla")
  cors = data.frame( vrs=vrs, stringsAsFactors=FALSE )
  cors$corel = 0
  for (v in 1:length(vrs)) {
    u = cor(  x[ good, vrs[v] ], x[ good, "depth"], use="pairwise.complete.obs" )
    if ( u > 0.999 ) u=0 # a degenerate solution ... 
    cors[ v, "corel"]  = u 
  }
  best = cors[ which.max( cors$corel ), "vrs" ]
  x$sm.spline[aoi] =  interpolate.xy.robust( x[aoi, c("ts", best )], target.r2=smoothing, probs=filter.quants, method="smooth.spline" )
  kk = x$depth - x$sm.spline
  qnts = quantile( kk[aoi], probs=filter.quants, na.rm=TRUE ) 
  qnts[1] = min( qnts[1], - eps.depth )  # hard code 1 m fluctuation as being insignificant error
  qnts[2] = max( qnts[2],   eps.depth )
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) good[i] = FALSE
  x$depth.smoothed[ !good] = NA
  x$depth[ !good] = NA
  

  # finalize solutions based upon priority of reliability
  vrs = c( "sm.spline", "sm.loess", "sm.inla")
  cors = data.frame( vrs=vrs, stringsAsFactors=FALSE )
  cors$corel = 0
  for (v in 1:length(vrs)) {
    u = cor(  x[ good, vrs[v] ], x[ good, "depth"], use="pairwise.complete.obs" )
    if ( u > 0.999 ) u=0 # a degenerate solution ... 
    cors[ v, "corel"]  = u 
  }
  best = cors[ which.max( cors$corel ), "vrs" ]
  x$depth.smoothed = x[,best] 
  if (all(is.na( x$depth.smoothed[aoi]) ) )  x$depth.smoothed = x$depth # give up

  return( list( depth.smoothed=x$depth.smoothed, good=good, variance.method=OinRange, variance.method.indices=OinRange.indices ) )

}

