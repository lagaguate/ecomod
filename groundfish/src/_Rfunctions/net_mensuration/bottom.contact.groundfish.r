bottom.contact.groundfish = function(x, tdif.min=15, tdif.max=50, depthproportion=0.5, nbins=7, plot.data=TRUE ) {
  
  debug = FALSE
  if (debug) {
    nbins= 7 # nbins is the number of bins required to get a target mode
    tdif.min = 15  # min time difference (minutes) .. including tails
    tdif.max = 50  # min time difference (minutes) .. including tails
    depthproportion=0.5 # depthproportion controls primary (coarse)gating
    plot.data=TRUE
    x = mm
  }
  
   
  O = list()  # output list
  O$id = unique( x$id)
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
  # 2nd pass -- a high pass filter -- remove high freq variations relative to random normal expectations of iid
  x$weight = 1 / x$depth.diff^2
  i = which( x$weight >= Inf)
  if (length(i)>0) x$weight[i] = max(x$weight[ which( x$weight < Inf) ], na.rm=TRUE )
  
  res = NA
  for( sp in seq( 0.35, 0.02, by=-0.01 ) ) {
    lmod = try( loess( depth~ts, data=x[O$filtered,], weights=x$weight[O$filtered], 
      span=sp, control=loess.control(surface="direct") ), silent=TRUE )
    if ( "try-error" %in% class(lmod) ) next()
    x$depth.loess = NA
    x$depth.loess[O$filtered] = predict( lmod ) 
    lmtest = try( lm( depth.loess ~ depth, x[O$filtered,] ), silent=TRUE) 
    if ( "try-error" %in% class(lmtest) ) next()
    lsumm = summary( lmtest )
    if (lsumm$r.squared > 0.95 ) {
      res = sp
      x$diff.loess = x$depth - x$depth.loess
      quants = quantile( x$diff.loess, probs=c(0.025, 0.975 ), na.rm=TRUE )
      i =  which( x$diff.loess <= quants[1] | x$diff.loess >= quants[2] )
      if (length(i) > 0) O$filtered[i] = FALSE
      break()
    }
  }
  
  if (plot.data) {
    points(depth~ts, x[ O$filtered, ], pch=20, col="magenta", cex=0.1)
  }

 
  ## -----------------------------------
  ## 3rd and last pass: use a variance based gating 
  # compute SD in the area of interest and compare with a lagged process to 
  # start from centre and move left and continue until sd of residuals begins to deviate sustantially
  # from centre to left 
 
  # keep everything except the 10th percentile and lower  this is very permissive ..
  
  AOIvar = which( O$filtered ) 
  AOIvar.mid = trunc( median(AOIvar ) ) # approximate midpoint
  AOIvar.min = min( AOIvar )
  AOIvar.max = max( AOIvar )
  AOI.sd = sd( x$depth[ O$filtered ] )  ## SD 
  sd.multiplier = seq( 3, 1, by=-0.2 ) 
  
  buffer = 10 # additional points to add to seed initial SD estimates

  duration = 0 
  for ( sm in sd.multiplier ) {
    target.sd = AOI.sd * sm
    for ( j0 in AOIvar.mid:AOIvar.min  ) {#  begin from centre to right 
      sdtest = sd(( x$depth[ (AOIvar.mid + buffer):j0]), na.rm=T)
      if ( sdtest  >= target.sd ) break()
    }
    for ( j1 in AOIvar.mid: AOIvar.max ) {  #  begin from centre to right
      sdtest =  sd(( x$depth[ (AOIvar.mid - buffer):j1]), na.rm=T)
      if ( sdtest >= target.sd ) break()
    }
    duration = as.numeric( x$timestamp[j1] - x$timestamp[j0]) 
    if ( duration > tdif.min & duration < tdif.max ) {
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


  # -------------------------------
  # record variance-based gating rejections
  i = setdiff( 1:nrow(x) , O$variance.method.indices )
  if (length(i) > 0) O$filtered[i] = FALSE


  ## ------------------------------
  ## Gating complete .. finalise data before starting time stamps for bottom contact
  x$depth[ !O$filtered ] = NA


  ##--------------------------------
  # Modal method: gating by looking for modal distribution and estimating sd of the modal group 
  # by removing small frequencies (5 or less) being ignored 
  # breaks defines how many columns you want in your histogram
  # expectation from uniform random is 1/2 of the threshold for testing that the freqeuncy belongs to the mode
  # ensure that high pass-filter-based rejections and simple gated filters are used

  # Determine area of interest (AOImod) -- most likely area of bottom contact

  # minval.modal is the min number of counts in a freq bin to be considered non-random (significant) 
  # ..based upon Chi-squared logic any count <= 5 is essentially indistinguishable from random noise
  minval.modal=5 
  nbins0 = 10:5  # range of target nbins>threshold to attempt in order to identify the mode 
  
  duration = 0
  for ( nbins in nbins0 ) {
    for ( nb0 in seq( from=300, to=10, by=-1 ) ) {
      h0 = hist( x$depth[ O$filtered ], breaks=nb0, plot=FALSE)  
      j0 = which(h0$counts >= minval.modal)  
      if (length (j0) <= nbins) {
        nb0 = nb0+1  
        break() # target number of good bins
      }
    }
    for ( nb1 in seq( from=10, to=300, by=1 ) ) { # do it from the other direction
      h1 = hist( x$depth[ O$filtered ], breaks=nb1, plot=FALSE)  
      j1 = which(h1$counts >= minval.modal) 
      if (length (j1) >= nbins ) {
        nb1 = nb1 - 1
        break() # target number of good bins
      }
    }
    if ( length(j0) <= nbins | length(j1) >= nbins ) {
      # redo hist with median value of the nbreaks as we have a potential solution
      nb = trunc( mean( nb0, nb1, na.rm=TRUE) ) 
      h = hist( x$depth[ O$filtered ], breaks=nb, plot=FALSE)  
      i = which(h$counts > minval.modal )  
      AOImod = which( x$depth > h$mids[min(i)] & x$depth < h$mids[max(i)]  ) 
      i0 = min( AOImod ) 
      i1 = max( AOImod )
      duration = as.numeric( x$timestamp[i1] - x$timestamp[i0]) 
      if ( duration > tdif.min & duration < tdif.max ) {
        # Do some rough filtering to remove sets without sufficient data
        O$modal.method = c( x$timestamp[i0], x$timestamp[i1] )
        O$modal.method.indices = which( x$timestamp >= O$modal.method[1] &  x$timestamp <= O$modal.method[2] )
        break()
      } 
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
  ## Smooth method: smooth data to remove local trends and compute first derivatives
  x2 = x[ O$filtered, ]  # copy and subset to simplify
  v = inla( depth ~ f(ts, model="rw2" ) , data=x2 )
  x2$fitted = v$summary.random$ts[["mean"]]  # posterior means .. removes some high freq noise
  
  AOI.mod.sd = sd( x2$depth, na.rm=TRUE) 
  fun = approxfun( x2$ts, x2$fitted )
  x2$slopes = NA
  x2$slopes = grad( fun, x2$ts, method="simple" )
  x2$slopes[ nrow(x2) ] = x2$slopes[ nrow(x2)-1 ]  # last element is an NA .. copy the next to last value into it  
  # using the same strategy as in primary gating 
  for( sp in seq( 0.24, 0.02, by=-0.02 ) ) {
    lmod = try( loess( slopes~ts, data=x2,  
      span=sp, control=loess.control(surface="direct") ), silent=TRUE )
    if ( "try-error" %in% class(lmod) ) next()
    x2$slopes.loess = NA
    x2$slopes.loess = predict( lmod ) 
    lmtest = try( lm( slopes.loess ~ slopes, x2 ), silent=TRUE) 
    if ( "try-error" %in% class(lmtest) ) next()
    lsumm = summary( lmtest )
    if (lsumm$r.squared > 0.5 ) {  # cannot be too extrmeme here as it is already highly noisy
      x2$slopes = x2$slopes.loess  # use the smoothed version if possible
      print(sp)
      break()
    }
  }
 
  # now using (smoothed) first derivative determine inflection points (crossing of the zero line)
  mod0 = 1
  mod1 = nrow(x2) 
  midpoint = trunc( mod1/2 )
  if ( abs( x$depth[mod0] - mediandepth ) < AOI.mod.sd ) {
    # assuming it is truncated without the left tail ..
    k0 = mod0 
  } else {
    for( k0 in mod0:midpoint)  {
      if (!is.finite( x2$slopes[ k0 ] )) next()
      if ( x2$slopes[ k0 ] <= 0) break()
    }
  }
  if ( abs( x2$depth[mod1] - mediandepth ) < AOI.mod.sd ) {
    # assuming it is truncated without the right tail ..
    k1 = mod1 
  } else {
    for( k1 in mod1:midpoint) {
      if (!is.finite( x2$slopes[ k1 ] )) next()
      if ( x2$slopes[ k1 ] >= 0)  break()
    }
  }

  duration = as.numeric( x2$timestamp[k1] - x2$timestamp[k0]) 
  if ( duration > tdif.min & duration < tdif.max ) {
    O$smooth.method =  c( x2$timestamp[k0], x2$timestamp[k1] )  
    O$smooth.method.indices = which( x$timestamp >= O$smooth.method[1] &  x$timestamp <= O$smooth.method[2] ) 
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
  
  if (length( which( is.finite( c( O$smooth.method, O$modal.method)) )) > 2 ) {  
    ## at least one solution required to continue  (2 means a valid start and end)
   # ID best model based upon distance of the medians to 
    dev.smooth   = median( x$depth[O$smooth.method.indices], na.rm=TRUE) 
    dev.modal    = median( x$depth[O$modal.method.indices], na.rm=TRUE )
    dev.blend    = mean( c(dev.smooth, dev.modal) , na.rm=TRUE) 
  
    results = c( dev.smooth, dev.modal, dev.blend ) 
    method = order( results) 
    
    res = rbind( 
      range(O$smooth.method.indices),
      range(O$modal.method.indices),
      trunc( c(mean( c( min(O$smooth.method.indices), min(O$modal.method.indices ) )), 
               mean( c( max(O$smooth.method.indices ), max(O$modal.method.indices) ))))
    )

    for ( m in method ) {
      if (!is.finite(m)) next()
      # find the row numbers which corresponds to each part of the curve, decent, bottom and ascent
      fishing = res[ m,] 
      mod0 = min( which( O$filtered ) )
      mod1 = max( which( O$filtered ) )
      bot = fishing[1]: fishing[2]  # blended estimate of fishing events
      down = mod0:( fishing[1] - 1) 
      up = ( fishing[2] + 1): mod1 
      #  compute linear models for each section
      botlm2 = lm(depth~ts, x[bot, ], na.action = "na.exclude")
      #  right tail
      if (( length( which( is.finite( x$depth[bot] ))) > 10) & (length( which( is.finite(x$depth[up]))) > 5 )) {
        uplm2 = lm(depth~ts, x[up, ], na.action = "na.exclude")
        cm <- rbind(coef(botlm2),coef(uplm2)) # Coefficient matrix
        i2=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
        ri2 = min( which(x$ts >i2[1]) )
      } else {
        ri2 = mod1
      }
    
      # left tail
      if (( length( which( is.finite( x$depth[bot] ))) > 10) & ( length( which( is.finite( x$depth[down]))) > 5 )) {
        downlm2 =lm(depth~ts, x[down, ], na.action = "na.exclude")
        # find where the sections intersect
        cm <- rbind(coef(botlm2),coef(downlm2)) # Coefficient matrix
        i1=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
        ri1 = max(which(x$ts <i1[1]))
      } else {
        ri1 = mod0
      }
    
      duration = as.numeric( x$timestamp[ri2] - x$timestamp[ri1]) 
      if ( duration > tdif.min & duration < tdif.max ) {
        O$linear.method =  c( x$timestamp[ri1], x$timestamp[ri2] )
        O$linear.method.indices = which( x$timestamp >= O$linear.method[1] &  x$timestamp <= O$linear.method[2] ) 
      } 
    } # end if enough data
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
 
  if (plot.data) {
    legend( "top", legend=legendtext, col=legendcol, pch=legendpch )
  }

  O$bottom0 = as.POSIXct( mean( c(O$linear.method[1], O$modal.method[1],  O$smooth.method[1]), na.rm=TRUE), origin = "1970-01-01" )
  O$bottom1 = as.POSIXct( mean( c(O$linear.method[2], O$modal.method[2],  O$smooth.method[2]), na.rm=TRUE), origin = "1970-01-01" )
  O$bottom0.sd = sd( as.numeric( c( O$linear.method[1], O$modal.method[1], O$smooth.method[1] ) ), na.rm=TRUE )
  O$bottom1.sd = sd( as.numeric( c( O$linear.method[2], O$modal.method[2], O$smooth.method[2] ) ), na.rm=TRUE )
  O$bottom0.n = length( which( is.finite( c( as.numeric( c( O$linear.method[1], O$modal.method[1], O$smooth.method[1]) )))) )
  O$bottom1.n = length( which( is.finite( c( as.numeric( c( O$linear.method[2], O$modal.method[2], O$smooth.method[2] ) )))) )
  return( O )

}



