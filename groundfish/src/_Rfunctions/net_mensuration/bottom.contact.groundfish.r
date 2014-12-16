bottom.contact.groundfish = function(id, x, tdif.min=15, tdif.max=45, depthproportion=0.5, smoothing = 0.9, plot.data=FALSE, user.interaction=FALSE ) {
  
  debug = FALSE
  if (debug) {
    tdif.min = 15  # min time difference (minutes) .. including tails
    tdif.max = 45  # min time difference (minutes) .. including tails
    depthproportion=0.5 # depthproportion controls primary (coarse)gating
    plot.data=TRUE 
    smoothing = 0.9
    user.interaction=FALSE # is you want to try to manually determine end points too
    x =  master[ii,c("depth", "timestamp")]
  }
   
  O = list()  # output list
  O$id = id
  O$filtered = rep(TRUE, nrow(x)) # rows that will contain data that passes each step of data quality checks
  O$linear.method = c(NA, NA)
  O$smooth.method = c(NA, NA)
  O$variance.method = c(NA, NA)
  O$modal.method = c(NA, NA)
  O$manual.method = c(NA , NA)
  
  ##--------------------------------
  # sort in case time is not in sequence
  # timestamps have frequencies higher than 1 sec .. duplciates are created and this can pose a problem
  x = x[order( x$timestamp ) ,]
  x$ts = as.numeric(x$timestamp)  # in seconds 
  x$ts = x$ts - min(x$ts) 
  
  ##--------------------------------
  ## Preliminary gating: simple range limits (gating) of depths to remove real extremes
  # eliminate records with NA for depth  
  i = which(!is.finite(x$depth))
  if (length(i) > 0) O$filtered[i] = FALSE
  
  ##--------------------------------
  # eliminiate shallow records due to operation at sea level
  i = which(x$depth < 10)
  if (length(i) > 0) O$filtered[i] = FALSE

  ##--------------------------------
  ## Filtering based on median depth
  mediandepth = median( x$depth[ O$filtered ], na.rm=TRUE)
  x$depth.diff = abs( x$depth - mediandepth )
  i = which( x$depth.diff > 30 )
  if (length(i) > 0) O$filtered[i] = FALSE

  ##--------------------------------
  # eliminiate records that are shallower than a given percentage of the median depth
  i = which(  x$depth < (depthproportion * mediandepth ) )
  if (length(i) > 0) O$filtered[i] = FALSE

  
  ##--------------------------------
  # 1st pass gating is finished ::: save (register them) and continue
  # plot of data after intial gating of depths
  x$depth0 = x$depth
  x$depth[ !O$filtered ] = NA

  if (plot.data) {
    ts.range = x$ts[range (which( O$filtered )) ]
    depth.range = range(x$depth, na.rm=TRUE)
    plot(depth~ts, x, sub=O$id, ylim=c(depth.range[2],depth.range[1]), xlim=ts.range, pch=20, cex=0.1 )
    legendtext = NULL
    legendcol = NULL
    legendpch = NULL
  }

    
  ##--------------------------------
  # 2nd pass -- a simple high pass filter to remove noisy data (very large extreme fluctuations)
  fr = range( which(O$filtered) )
  aoi = fr[1]:fr[2]
  x$sm.seq = NA
  x$sm.seq[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth")],  probs=c(0.025,0.975), method="sequential.linear" )
  i = which( x$sm.seq != x$depth )
  if (length(i) > 0) O$filtered[i] = FALSE
  x$depth[ !O$filtered] = NA
  if (plot.data) points(depth ~ ts, x[ O$filtered,], col="orange",pch=20, cex=0.2 )


  ## -----------------------------------
  ## 3rd pass: use a variance based gating 
  # compute SD in the area of interest and compare with a lagged process to 
  # start from centre and move left and continue until sd of residuals begins to deviate sustantially
  # from centre to left 
  
  aoi.range = range( which( O$filtered )  )
  aoi.mid = trunc( mean( aoi.range ) ) # approximate midpoint
  aoi.min = aoi.range[1]
  aoi.max = aoi.range[2]
  aoi = aoi.min:aoi.max
  
  aoi.sd = sd( x$depth[ aoi ], na.rm=TRUE )  ## SD 
  sd.multiplier = seq( 3, 1, by=-0.1 ) 
  buffer = 10 # additional points to add to seed initial SD estimates
  duration = 0 
  
  for ( sm in sd.multiplier ) {
    target.sd = aoi.sd * sm
    for ( j0 in aoi.mid:aoi.min  ) {#  begin from centre to right 
      sdtest = sd(( x$depth[ (aoi.mid + buffer):j0]), na.rm=T)
      if ( sdtest  >= target.sd ) break()
    }
    for ( j1 in aoi.mid: aoi.max ) {  #  begin from centre to right
      sdtest =  sd(( x$depth[ (aoi.mid - buffer):j1]), na.rm=T)
      if ( sdtest >= target.sd ) break()
    }
    duration = as.numeric( x$timestamp[j1] - x$timestamp[j0]) 
    if ( duration > (tdif.min - 5) & duration < (tdif.max+5)  ) {  # add along tails to have enough data for analysis
      O$variance.method = c( x$timestamp[j0], x$timestamp[j1] )
      O$variance.method.indices = which( x$timestamp >= O$variance.method[1] &  x$timestamp <= O$variance.method[2] )
      break()
    }  
  }
    if (plot.data & all(is.finite(O$variance.method))  ) {
      mcol = "gray"
      points( depth~ts, x[ O$variance.method.indices, ], pch=20, col=mcol, cex=0.2)
      abline (v=x$ts[min(O$variance.method.indices)], col=mcol, lty="dotted")
      abline (v=x$ts[max(O$variance.method.indices)], col=mcol, lty="dotted")
      legendtext = c( legendtext, paste( "variance:   ", round( duration, 2) ) )
      legendcol = c( legendcol, mcol)
      legendpch =c( legendpch, 20 ) 
    }


  ## ------------------------------
  # Last pass ... filter data using some robust methods
  
  x$depth.smoothed = x$depth
  x$sm.loess = x$sm.inla = x$sm.spline= NA

  x$sm.inla[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth.smoothed")],  target.r2=0.9, probs=c(0.025,0.975), method="inla"  )
  kk = x$depth - x$sm.inla
  qnts = quantile( kk[aoi], probs=c(0.025, 0.975), na.rm=TRUE ) 
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) O$filtered[i] = FALSE
  x$depth.smoothed[ !O$filtered] = NA  # i.e. sequential deletion of depths

  
  x$sm.loess[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth.smoothed")],  target.r2=0.9, method="loess"  )
  kk = x$depth - x$sm.loess
  qnts = quantile( kk[aoi], probs=c(0.025, 0.975), na.rm=TRUE ) 
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) O$filtered[i] = FALSE
  x$depth.smoothed[ !O$filtered] = NA
 

  # input to smoooth.spline must not have NA's .. use inla's predictions
  method ="sm.inla"
  if (any( !is.finite( x$sm.inla[aoi]) )) method= "sm.loess"
  x$sm.spline[aoi] =  interpolate.xy.robust( x[aoi, c("ts", method )], target.r2=0.9, probs=c(0.025,0.975), method="smooth.spline" )
  kk = x$depth - x$sm.spline
  qnts = quantile( kk[aoi], probs=c(0.025, 0.975), na.rm=TRUE ) 
  i = which(kk > qnts[2]  | kk < qnts[1] )
  if (length(i) > 0) O$filtered[i] = FALSE
  x$depth.smoothed[ !O$filtered] = NA
 

  # finalize solutions based upon priority of reliability
  x$depth[ !O$filtered ] = NA
  x$depth.smoothed[aoi] = interpolate.xy.robust( x[aoi, c("ts", "depth")],  target.r2=0.9, probs=c(0.025,0.975), method="inla"  )
  if (any( !is.finite( x$depth.smoothed[aoi]) )) {
    x$depth.smoothed[aoi] =  interpolate.xy.robust( x[aoi, c("ts", "depth")],  target.r2=0.9, method="loess"  )
  }
  if (any( !is.finite( x$depth.smoothed[aoi]) )) {
    x$depth.smoothed[aoi] = interpolate.xy.robust( x[aoi, c("ts", "sm.inla")], target.r2=0.9, probs=c(0.025,0.975), method="smooth.spline" )
  }
  if (any( !is.finite( x$depth.smoothed[aoi]) )) {
    x$depth.smoothed[aoi] = interpolate.xy.robust( x[aoi, c("ts", "sm.seq")], target.r2=0.9, probs=c(0.025,0.975), method="smooth.spline" )
  }


  if ( cor( x[aoi, "depth.smoothed"], x$depth[aoi], use="pairwise.complete.obs" ) > 0.99 )  {
    # this means that no real solution/degenerate solution was found by inla ... use a spline
    x$depth.smoothed[aoi] =  interpolate.xy.robust( x[aoi, c("ts", "depth.smoothed")],  target.r2=0.9, method="loess"  )
  }
  
  if (all(is.na( x$depth.smoothed[aoi]) ) ) {
    x$depth.smoothed = x$depth # give up
  }


  ## ------------------------------
  ## Gating complete .. finalise data before starting time stamps for bottom contact .. reset aoi
  # record variance-based gating rejections
  i = setdiff( 1:nrow(x) , O$variance.method.indices )
  if (length(i) > 0) O$filtered[i] = FALSE
  x$depth[ !O$filtered ] = NA
  
  aoi.range = range( which( O$filtered )  )
  aoi.mid = trunc( mean( aoi.range ) ) # approximate midpoint
  aoi.min = aoi.range[1]
  aoi.max = aoi.range[2]
  aoi = aoi.min:aoi.max


  ## ------------------------------
  # Now that data is more or less clean ... 
  # Remove any linear trend in the depths as this can increase the precision of the the following methods
  depthtrend.smoothed = lm( depth.smoothed ~ ts, data=x, weights=depth^2, na.action="na.omit")  # deeper weights have higher influence (reduce influence of tails )
  x$depth.residual = x$depth.smoothed - predict( depthtrend.smoothed, newdata=x ) + median( x$depth.smoothed, na.rm=TRUE )


  ##--------------------------------
  # Modal method: gating by looking for modal distribution and estimating sd of the modal group 
  # by removing small frequencies (5 or less) being ignored 
  # breaks defines how many columns you want in your histogram
  # expectation from uniform random is 1/2 of the threshold for testing that the freqeuncy belongs to the mode
  # ensure that high pass-filter-based rejections and simple gated filters are used

  nbins0 = 10:5  # range of target nbins>threshold to attempt in order to identify the mode 
  aoi.sd = sd( x$depth.residual[ aoi ], na.rm=TRUE )  ## SD 
  aoi.med = median( x$depth.residual[ aoi ], na.rm=TRUE )
  duration = 0
  for ( nbins in nbins0 ) {
    for ( nb in seq( from=200, to=20, by=-2 ) ) {
      h = hist( x$depth.residual[ aoi ], breaks=nb, plot=FALSE)  
      i = which(h$density > 0.05 )   
      if (length (i) <= nbins) {
        aoi.mod = which( x$depth.residual > h$mids[min(i)] & x$depth.residual < h$mids[max(i)]  ) 
        if (length(aoi.mod)>0 & all(is.finite( aoi.mod ) ) ) {
          if ( abs( x$depth.residual[ aoi.min ] - aoi.med ) < aoi.sd ) {
            # assuming it is truncated without the left tail ..
            i0 = aoi.min
          } else {
            i0 = min( aoi.mod ) 
          }
          if ( abs( x$depth.residual[ aoi.max ] - aoi.med ) < aoi.sd ) {
            # assuming it is truncated without the right tail ..
            i1 = aoi.max
          } else {
            i1 = max( aoi.mod )
          }
        }
        duration = as.numeric( x$timestamp[i1] - x$timestamp[i0]) 
        break() # target number of good bins
      }
    }
    if ( duration > tdif.min & duration < tdif.max ) {
      # Do some rough filtering to remove sets without sufficient data
      O$modal.method = c( x$timestamp[i0], x$timestamp[i1] )
      O$modal.method.indices = which( x$timestamp >= O$modal.method[1] &  x$timestamp <= O$modal.method[2] )
      break()
    } 
  }
    if (plot.data & all(is.finite(O$modal.method)) ) {
      mcol = "red" # colour for plotting
      points( depth~ts, x[O$modal.method.indices,], col=mcol, pch=20, cex=0.2)       
      abline (v=x$ts[min(O$modal.method.indices)], col=mcol, lty="dashed")
      abline (v=x$ts[max(O$modal.method.indices)], col=mcol, lty="dashed")
      legendtext = c( legendtext, paste( "modal:   ", round( duration, 2) ) )
      legendcol = c( legendcol, mcol)
      legendpch =c( legendpch, 20) 
    }

   
  ## ---------------------------- 
  ## Smooth method: smooth data to remove local trends and compute first derivatives and smooth again
  x2 = x[aoi,]
  # x2$depth.smoothed = interpolate.xy.robust( x2[, c("ts", "depth.smoothed")], target.r2=0.9, method="loess" )
  fun = approxfun( x2$ts, x2$depth.smoothed )
  x2$slopes = grad( fun, x2$ts, method="simple" )
  x2$slopes[ nrow(x2) ] = x2$slopes[ nrow(x2)-1 ]  # last element is an NA .. copy the next to last value into it  
  x2$slopes.smoothed = interpolate.xy.robust( x2[, c("ts", "slopes")], target.r2=0.9, probs=c(0.025,0.975), method="smooth.spline" )

  # now using (smoothed) first derivative determine inflection points (crossing of the zero line)
  eps = quantile( x2$slopes.smoothed, probs=c(0.1, 0.9), na.rm=TRUE ) # ie. what is greater than normal magnitudes of slope fluctuations 
#  print(eps)

  m0 = 1
  m1 = nrow(x2)
  mm = trunc( mean( c(m0, m1) ) )
  mmed = median( x2$depth.smoothed, na.rm=TRUE )
  mmsd = sd( x2$depth.smoothed, na.rm=TRUE )  ## SD 
  # left side
  if ( abs( x2$depth.smoothed[m0] - mmed ) < mmsd ) {
    # assuming it is truncated without the left tail ..
    k0 = m0
  } else {
    for( k0 in m0:mm)  {
      if (!is.finite( x2$slopes.smoothed[ k0 ] )) next()
      if ( x2$slopes.smoothed [ k0 ] < eps[2] ) break()
    }
  }
  if ( abs( x2$depth.smoothed[m1] - mmed ) < mmsd ) {
    # assuming it is truncated without the right tail ..
    k1 = m1
  } else {
    for( k1 in m1:mm) {
      if (!is.finite( x2$slopes.smoothed[ k1 ] )) next()
      if ( x2$slopes.smoothed[ k1 ] > eps[1] )  break()
    }
  }

  duration = as.numeric( x2$timestamp[k1] - x2$timestamp[k0]) 
  if ( duration > tdif.min & duration < tdif.max ) {
    O$smooth.method =  c( x2$timestamp[k0], x2$timestamp[k1] )    # x2 is correct
    O$smooth.method.indices = which( x$timestamp >= O$smooth.method[1] &  x$timestamp <= O$smooth.method[2] ) # x correct
  } 
  
    if (plot.data & all(is.finite(O$smooth.method)) ) {
      mcol = "blue"
      points( depth~ts, x[O$smooth.method.indices,], col=mcol, pch=20, cex=0.2)   
      abline (v=x$ts[min(O$smooth.method.indices)], col=mcol, lty="dashed")
      abline (v=x$ts[max(O$smooth.method.indices)], col=mcol, lty="dashed")
      legendtext = c(legendtext, paste( "smooth:   ", round(duration, 2)) )
      legendcol = c( legendcol, mcol)
      legendpch =c( legendpch, 20) 
    }


  ## ---------------------------
  ## Linear method: looking at the intersection of three lines (up, bot and down)
  
    ## at least one solution required to continue  (2 means a valid start and end)
     # ID best model based upon time .. furthest up a tail is best 
  
    res = rbind( 
      range(O$smooth.method.indices),
      range(O$modal.method.indices)
    )
    oo = which( !is.finite(res))
    if (length(oo)>0) res[oo] = NA

    left = median(res[,1], na.rm=TRUE)
    right = median( res[,2], na.rm=TRUE) 

    # find the row numbers which corresponds to each part of the curve, decent, bottom and ascent
    bot = left:right  # blended estimate of fishing events
    down = aoi.min:left 
    up =  right:aoi.max 
    #  compute linear models for each section
    botlm2 = lm(depth.residual~ts, x[bot, ], na.action = "na.exclude")
    #  right tail
    if (( length( which( is.finite( x$depth.residual[bot] ))) > 10) & (length( which( is.finite(x$depth.residual[up]))) > 5 )) {
      uplm2 = lm(depth.residual~ts, x[up, ], na.action = "na.exclude")
      cm <- rbind(coef(botlm2),coef(uplm2)) # Coefficient matrix
      i2=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
      ii = which(x$ts >i2[1]) 
      if (length(ii)>0) {
        ri2 = min( ii )
      } else {
        ri2 = aoi.max
      }
    } else {
      ri2 = aoi.max
    }
  
    # left tail
    if (( length( which( is.finite( x$depth.residual[bot] ))) > 10) & ( length( which( is.finite( x$depth.residual[down]))) > 5 )) {
      downlm2 =lm(depth.residual~ts, x[down, ], na.action = "na.exclude")
      # find where the sections intersect
      cm <- rbind(coef(botlm2),coef(downlm2)) # Coefficient matrix
      i1=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
      ii = which(x$ts < i1[1])
      if (length(ii)>0) { 
        ri1 = max(ii)
      } else {
        ri1 = aoi.min 
      }
    } else {
      ri1 = aoi.min
    }
  
    duration = as.numeric( x$timestamp[ri2] - x$timestamp[ri1]) 
    if ( duration > tdif.min & duration < tdif.max ) {
      O$linear.method =  c( x$timestamp[ri1], x$timestamp[ri2] )
      O$linear.method.indices = which( x$timestamp >= O$linear.method[1] &  x$timestamp <= O$linear.method[2] ) 
    } 

    if (plot.data  & all(is.finite(O$linear.method)) ) {
        mcol ="green"
        points( depth~ts, x[O$linear.method.indices,], col=mcol, pch=20, cex=0.2)      
        abline (v=x$ts[min(O$linear.method.indices)], col=mcol)
        abline (v=x$ts[max(O$linear.method.indices)], col=mcol)
        legendtext = c( legendtext, paste( "linear: ", round( duration, 2) ) )
        legendcol = c( legendcol, mcol)
        legendpch =c( legendpch, 20) 
        #abline (reg=botlm2, col="gray")      
        #abline (reg=uplm2, col="gray")      
        #abline (reg=downlm2, col="gray")      
    }

   
  if ( user.interaction  ) { 
    print( "Click with mouse on start and stop locations now.")          
    useridentified = locator( n=2, type="o", col="cyan")
    u.ts0 = which.min( abs(x$ts-useridentified$x[1] ))
    u.ts1 = which.min( abs(x$ts-useridentified$x[2] ))
    O$manual.method = c( x$timestamp[u.ts0], x$timestamp[ u.ts1 ]  )
    O$manual.method.indices = which( x$timestamp >= O$manual.method[1] &  x$timestamp <= O$manual.method[2] ) 
    tdif = abs( as.numeric(diff(O$manual.method)) )
    tdifflinear = round( tdif, 2)
    legendtext = c( legendtext, paste( "manual: ", tdifflinear  ) ) 
    legendcol = c( legendcol, "cyan")
    legendpch =c( legendpch, 20) 
  }
  
  if (plot.data) {
    legend( "top", legend=legendtext, col=legendcol, pch=legendpch )
  }
  
  if ( user.interaction  ) { 
    outdir = getwd()
    dev.copy2pdf( file=file.path( outdir, paste(id, "pdf", sep="." ) ) )
  }

  methods = c("manual.method", "smooth.method", "modal.method", "linear.method" )
  standard =  which( methods=="manual.method")
  direct = which( methods!="manual.method")

  tmp = as.data.frame(O[methods], stringsAsFactors=FALSE )
  tmp = as.data.frame( t(tmp), stringsAsFactors=FALSE  )
  colnames(tmp) =c("start", "end" )
  tmp$start = ymd_hms( tmp$start)
  tmp$end = ymd_hms( tmp$end)
  
  means = as.data.frame( cbind( as.POSIXct( mean( tmp[ direct, "start" ], na.rm=TRUE ), origin = "1970-01-01" ), 
                 as.POSIXct( mean( tmp[ direct, "end" ], na.rm=TRUE ), origin = "1970-01-01")   ) , stringsAsFactors=FALSE )

  rownames( means) = "means"
  colnames(means) = c("start", "end" )
  means$start = as.POSIXct( means$start, origin = "1970-01-01" )  # means only of direct methods
  means$end = as.POSIXct( means$end, origin = "1970-01-01" )

  if ( all(is.na( tmp[ standard, c("start", "end")] ) ) ) {
    # no manual standard .. use mean as the standard
    O$bottom0 = means$start
    O$bottom1 = means$end
    O$bottom0.sd = sd(  tmp[ direct, "start" ], na.rm=TRUE ) # in secconds
    O$bottom1.sd = sd(  tmp[ direct, "end" ], na.rm=TRUE )
    O$bottom0.n = length( which( is.finite( tmp[ direct, "start" ] )) )
    O$bottom1.n = length( which( is.finite( tmp[ direct, "end" ] )) )
    O$bottom.diff = O$bottom1 - O$bottom0
  } else {
    # over-ride all data and use the manually determined results
    O$bottom0 = tmp[ standard, "start" ]
    O$bottom1 = tmp[ standard, "end" ]
    O$bottom0.sd = -1
    O$bottom1.sd = -1
    O$bottom0.n = -1
    O$bottom1.n = -1
    O$bottom.diff = O$bottom1 - O$bottom0
  }
 
  tmp = rbind( tmp, means)
  tmp$diff = tmp$end - tmp$start
  tmp$start.bias = tmp$start - O$bottom0
  tmp$end.bias   = tmp$end - O$bottom1

  O$summary = tmp
  O$aoi.sd = aoi.sd 
  O$aoi = aoi 
  O$aoi.n = length(aoi)

  O$signal2noise = (O$aoi.n - O$noise.n) / O$aoi.n  # not really signal to noise but rather  % informations 
  
  #x11(); plot( slopes ~ ts, x2 )
  lines( depth.smoothed ~ ts, x2, col="brown" )
  points( depth0~ts, x[!O$filtered,], col="red", cex=0.7 )  
  print( O$summary)

  return( O )

}



