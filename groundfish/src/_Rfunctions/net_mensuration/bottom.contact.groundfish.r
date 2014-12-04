bottom.contact.groundfish = function(x, n.req=30,  depthproportion=0.5, minval.modal=5, nbins=7, plot.data=TRUE ) {
  
  debug = FALSE
  if (debug) {
    nbins= 7 # nbins is the number of bins required to get a target mode
    n.req= 30 # n.req controls min number required before attempting analysis
    depthproportion=0.5 # depthproportion controls primary (coarse)gating
    minval.modal=5 # minval.modal is the min number of counts in a freq bin to be considered non-random (significant)
    plot.data=TRUE
    x = mm
  }
  
  
  # load in libraries
  require(numDeriv)
  require(mgcv)
  require(INLA)
  
  O = list()  # output list
  O$linear.method = c(NA, NA)
  O$smooth.method = c(NA, NA)
  O$variance.method = c(NA, NA)
  O$modal.method = c(NA, NA)
  
  # in case time is not in sequence
  x = x[order( x$timestamp ) ,]
  
  ##--------------------------------
  # timestamps have frequencies higher than 1 sec .. duplciates are created and this can pose a problem
  x$ts = as.numeric(x$timestamp)  # in seconds 
  x$ts = x$ts - min(x$ts) 
  
  O$filtered = rep(TRUE, nrow(x)) # rows that will contain data that passes each step of data quality checks
  
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
  dmax = mediandepth + 50
  baddata = which( x$depth > dmax   )
  if (length(baddata) > 0) O$filtered[baddata] = FALSE
  
  if (plot.data) {
    depth.range = range(x$depth[ O$filtered ], na.rm=TRUE)
    plot(depth~ts, x[ O$filtered,], sub=id, ylim=c(depth.range[2],depth.range[1]), pch=20, cex=0.1 )
    legendtext = NULL
    legendcol = NULL
    legendpch = NULL
  }

  dmin = trunc( depthproportion * mediandepth )
  baddata = which(  x$depth < dmin  )
  if (length(baddata) > 0) O$filtered[baddata] = FALSE
  
  # 1st pass gating is finished ::: save and continue
  x$depth[ !O$filtered ] = NA
  if (plot.data) {
    points(depth~ts, x, sub=id, pch=20, col="cyan2", cex=0.1)
  }

  ##--------------------------------
  # 2nd pass -- a high pass filter -- remove high freq variations relative to random normal expectations of iid
  x$ts2 = x$ts
  v=inla(depth ~ f(ts2, model="iid") + f(ts, model="rw2"),data=x )  # iid, and rw2
  dev = v$summary.random$ts2[["mean"]] # residuals
  
  quants = quantile( dev, probs=c(0.025, 0.975), na.rm=TRUE)
  bad = which( dev <= quants[1] | dev >= quants[2] )
  if (length(bad) > 0) O$filtered[bad] = FALSE
 
  if (plot.data) {
    points(depth~ts, x[ O$filtered, ], sub=id, pch=20, col="magenta", cex=0.1)
  }

 
  ## -----------------------------------
  ## 3rd and last pass: use a variance based gating 
  # compute SD in the area of interest and compare with a lagged process to 
  # start from centre and move left and continue until sd of residuals begins to deviate sustantially
  # from centre to left 
 
  # keep everything except the 10th percentile and lower  this is very permissive ..
  depththreshold = quantile( x$depth[O$filtered], probs= 0.1, na.rm=TRUE )
  AOIvar = which( x$depth > depththreshold ) 
  target.sd = 2 * sd( x$depth[min(AOIvar):max(AOIvar)], na.rm=TRUE )  ## 2 *SD  ~ 95CI
  nx2 = trunc( median(AOIvar ) ) # midpoint
  left = nx2:1
  for ( j0 in left  ) {
    sdtest = sd(( x$depth[ (nx2+10):j0]), na.rm=T)
    if ( sdtest  >= target.sd ) break()
  }
  # end time .. begin from centre to right 
  right = nx2: max( which(O$filtered), na.rm=TRUE)
  for ( j1 in right ) {
    sdtest =  sd(( x$depth[ (nx2-10):j1]), na.rm=T)
    if ( sdtest >= target.sd ) break()
  }
  if ( (j1-j0) > n.req) {
    O$variance.method = c( x$timestamp[j0], x$timestamp[j1] )
    O$variance.method.indices = which( x$timestamp >= O$variance.method[1] &  x$timestamp <= O$variance.method[2] )
    tdif = abs( as.numeric(diff(O$variance.method)) )
    if (length(tdif)>0 && ( tdif < 15 | tdif > 40) ) O$variance.method = c(NA, NA)
    if (plot.data) {
      tdiffvariance = round(tdif,2)
      abline (v=x$ts[min(O$variance.method.indices)], col="gray")
      abline (v=x$ts[max(O$variance.method.indices)], col="gray")
      legendtext = c( legendtext, paste( "variance:   ", tdiffvariance) )
      legendcol = c( legendcol, "gray")
      legendpch =c( legendpch, 19 ) 
      points(depth~ts, x[ O$variance.method.indices, ], sub=id, pch=20, col="gray", cex=0.2)
    }
  }
  bad = setdiff( 1:nrow(x) , O$variance.method.indices )
  if (length(bad) > 0) O$filtered[bad] = FALSE


  ## Gating complete .. finalise data before starting time stamps for bottom contact
  O$filtered.good = which( O$filtered )   # store row indices 
  x$depth[ !O$filtered ] = NA
  if (plot.data) {
    points(depth~ts, x[ O$filtered, ], sub=id, pch=20, col="magenta", cex=0.2)
  }



  ##--------------------------------
  # Modal method: gating by looking for modal distribution and estimating sd of the modal group 
  # by removing small frequencies (5 or less) being ignored 
  # breaks defines how many columns you want in your histogram
  # expectation from uniform random is 1/2 of the threshold for testing that the freqeuncy belongs to the mode
  # ensure that high pass-filter-based rejections and simple gated filters are used

  # Determine area of interest (AOImod) -- most likely area of bottom contact
  for ( nb0 in seq( from=300, to=10, by=-1 ) ) {
    h0 = hist( x$depth[ O$filtered ], breaks=nb0, plot=FALSE)  
    j0 = which(h0$counts >= minval.modal)  # chi-squared logic ..  any count <= 5 is essentially indistinguishable from random noise
    if (length (j0) <= nbins) {
      nb0 = nb0+1  
      break() # target number of good bins
    }
  }
  
  # do it from the other direction
  for ( nb1 in seq( from=10, to=300, by=1 ) ) {
    h1 = hist( x$depth[ O$filtered ], breaks=nb1, plot=FALSE)  
    j1 = which(h1$counts >= minval.modal)  # chi-squared logic ..  any count <= 5 is essentially indistinguishable from random noise
    if (length (j1) >= nbins ) {
      nb1 = nb1 - 1
      break() # target number of good bins
    }
  }
  
  if ( length(j0) <= nbins | length(j1) >= nbins ) {
    # redo hist with median value of the nbreaks
    nb = trunc( mean( nb0, nb1, na.rm=TRUE) ) 
    h = hist( x$depth[ O$filtered ], breaks=nb, plot=FALSE)  
    i = which(h$counts > minval.modal)  # chi-squared logic ..  any count <= 5 is essentially indistinguishable from random noise
 
    AOImod = which( x$depth > h$mids[min(i)] & x$depth < h$mids[max(i)]  ) # usuaully a less permissive solution 
    i0 = min( AOImod ) 
    i1 = max( AOImod )
    # Do some rough filtering to remove sets without sufficient data
    O$modal.method = c( x$timestamp[i0], x$timestamp[i1] )
    O$modal.method.indices = which( x$timestamp >= O$modal.method[1] &  x$timestamp <= O$modal.method[2] )
    tdif = abs( as.numeric(diff(O$modal.method)) )
    if (length(tdif)>0 && ( tdif < 15 | tdif > 40) ) O$modal.method = c(NA, NA)
    if (plot.data) {
      points(x$ts[O$modal.method.indices],  x$depth[O$modal.method.indices], col="red", pch=20, cex=0.2)       
      tdiffmodal = round(tdif,2)
      abline (v=x$ts[min(O$modal.method.indices)], col="red")
      abline (v=x$ts[max(O$modal.method.indices)], col="red")
      legendtext = c( legendtext, paste( "modal:   ", tdiffmodal) )
      legendcol = c( legendcol, "red")
      legendpch =c( legendpch, 22) 
    }
  } 

  
   
  ## ---------------------------- 
  ## Smooth method: smooth data to remove local trends and compute a detrended residual using INLA
  # operate on a copy of the data as it need direct manipulation 
  x2 = x[ O$filtered, ]
  x2$ts1 = x2$ts
  v = inla( depth ~ f(ts,model="rw2"), data=x2 )
  x2$depth.predicted = v$summary.random[["ts"]][["mean"]]   
  fun = approxfun( x2$ts1, x2$depth.predicted )
    
  # first, recenter the smooths 
  slopes = grad( fun, x2$ts, method="simple" )
  
  # looking for modal distribution and estimating sd of the modal group of slopes
  quants.slopes = quantile( slopes, probs=c(0.025, 0.975), na.rm=TRUE)
  slopes[ slopes < quants.slopes[1]] = NA
  slopes[ slopes > quants.slopes[2]] = NA

  # now using (smoothed) first derivative determine inflection points (crossing of the zero line)
  nd = length(slopes) 
  for( k0 in min( O$filtered.good):nd)  {
    if (!is.finite( slopes[ k0 ] )) next()
    if (slopes[ k0 ] <= 0) break()
  }
  for( k1 in max( O$filtered.good):1) {
    if (!is.finite( slopes[ k1 ] )) next()
    if (slopes[ k1 ] >= 0)  break()
  }
  O$smooth.method =  c( x2$timestamp[k0], x2$timestamp[k1] )  # on x2 is correct
  O$smooth.method.indices = which( x$timestamp >= O$smooth.method[1] &  x$timestamp <= O$smooth.method[2] )  # on x1 is correct as it must relate to main data
  tdif = abs( as.numeric(diff(O$smooth.method)) )
  if (length(tdif)>0 && (tdif < 15 | tdif > 40) ) O$smooth.method = c(NA, NA)
  if (plot.data) {
    points(x$ts[O$smooth.method.indices],  x$depth[O$smooth.method.indices], col="blue", pch=24, cex=0.2)   
    tdiffsmooth = round(tdif,2)
    abline (v=x$ts[min(O$smooth.method.indices)], col="blue")
    abline (v=x$ts[max(O$smooth.method.indices)], col="blue")
    legendtext = c(legendtext, paste( "smooth:   ", tdiffsmooth) )
    legendcol = c( legendcol, "blue")
    legendpch =c( legendpch, 24) 
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
    best =NA
    best = which.min( results) # deepest median is assumed to be the "best" 
    # find the row numbers which corresponds to each part of the curve, decent, bottom and ascent
    if (length(best)==1 ) {
      fishing = switch( best,
        range(O$smooth.method.indices),
        range(O$modal.method.indices),
        trunc( c( 
          mean( c( min(O$smooth.method.indices), min(O$modal.method.indices ) )), 
          mean( c( max(O$smooth.method.indices ), max(O$modal.method.indices) )) 
        ))
      )
  
      bot = fishing[1]: fishing[2]  # blended estimate of fishing events
      down = 1:( fishing[1] - 1) 
      up = ( fishing[2] + 1): max(O$filtered.good) 
      #  compute linear models for each section
      botlm2 = lm(depth~ts, x[bot, ], na.action = "na.exclude")
      #  right tail
      if (( length( which( is.finite( x$depth[bot] ))) > n.req) & (length( which( is.finite(x$depth[up]))) > 5 )) {
        uplm2 = lm(depth~ts, x[up, ], na.action = "na.exclude")
        cm <- rbind(coef(botlm2),coef(uplm2)) # Coefficient matrix
        i2=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
        ri2 = min( which(x$ts >i2[1]) )
      } else {
        ri2 = max( O$filtered.good, na.rm=TRUE )
      }
    
      # left tail
      if (( length( which( is.finite( x$depth[bot] ))) > n.req) & ( length( which( is.finite( x$depth[down]))) > 5 )) {
        downlm2 =lm(depth~ts, x[down, ], na.action = "na.exclude")
        # find where the sections intersect
        cm <- rbind(coef(botlm2),coef(downlm2)) # Coefficient matrix
        i1=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
        ri1 = max(which(x$ts <i1[1]))
      } else {
        ri1 = min( O$filtered.good, na.rm=TRUE )
      }
    
      O$linear.method =  c( x$timestamp[ri1], x$timestamp[ri2] )
      O$linear.method.indices = which( x$timestamp >= O$linear.method[1] &  x$timestamp <= O$linear.method[2] ) 

      tdif = abs( as.numeric(diff(O$linear.method)) )
      if (length(tdif)>0 && (tdif < 15 | tdif > 40) ) O$linear.method = c(NA, NA)
        
      if (plot.data) {
        points(x$ts[O$linear.method.indices],  x$depth[O$linear.method.indices], col="green", pch=22, cex=0.2)      
        tdifflinear = round( tdif, 2)
        abline (v=x$ts[min(O$linear.method.indices)], col="green")
        abline (v=x$ts[max(O$linear.method.indices)], col="green")
        legendtext = c( legendtext, paste( "linear: ", tdifflinear) )
        legendcol = c( legendcol, "green")
        legendpch =c( legendpch, 20) 
        #abline (reg=botlm2, col="gray")      
        #abline (reg=uplm2, col="gray")      
        #abline (reg=downlm2, col="gray")      
      }
    }
  } # end if enough data
  
  if (plot.data) {
    legend( "top", legend=legendtext, col=legendcol, pch=legendpch )
  }

  O$bottom0.mean = as.POSIXct( mean( c(O$linear.method[1], O$modal.method[1],  O$smooth.method[1]), na.rm=TRUE), origin = "1970-01-01" )
  O$bottom1.mean = as.POSIXct( mean( c(O$linear.method[2], O$modal.method[2],  O$smooth.method[2]), na.rm=TRUE), origin = "1970-01-01" )
  O$bottom0.sd = sd( as.numeric( c( O$linear.method[1], O$modal.method[1], O$smooth.method[1] ) ), na.rm=TRUE )
  O$bottom1.sd = sd( as.numeric( c( O$linear.method[2], O$modal.method[2], O$smooth.method[2] ) ), na.rm=TRUE )
  O$bottom0.n = length( which( is.finite( c( as.numeric( c( O$linear.method[1], O$modal.method[1], O$smooth.method[1]) )))) )
  O$bottom1.n = length( which( is.finite( c( as.numeric( c( O$linear.method[2], O$modal.method[2], O$smooth.method[2] ) )))) )
  return( O )
}



