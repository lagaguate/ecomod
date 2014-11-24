bottom.contact.groundfish = function(x, n.req=30,  depthproportion=0.5, nbins=c(5,10) , minval.modal=5 ) {
  
  # n.req=30; nbins=c(5,10)
  
  # load in libraries
  require(numDeriv)
  require(mgcv)
  require(INLA)
  
  O = list(  comments=NA )  # output list
  
  # in case time is not in sequence
  x = x[order( x$timestamp ) ,]
  
  # timestamps have frequencies higher than 1 sec .. duplciates are created and this can pose a problem
  x$ts = as.numeric(x$timestamp)  # in seconds 
  x$ts = x$ts - min(x$ts) 
  
  O$filtered = 1:nrow(x) # list of rows that will contain data that passes each step of data quality checks
  
  

  ## Preliminary gating: simple range limits (gating) of depths to remove real extremes
  
  # eliminate records with NA for depth  
  i = which(!is.finite(x$depth))
  if (length(i) > 0) O$filtered[i] = NA
  
  # eliminiate shallow records due to operation at sea level
  i = which(x$depth < 10)
  if (length(i) > 0) O$filtered[i] = NA

  

  # eliminate a range around the median value 
  mediandepth = quantile( x$depth, probs=c(0.5), na.rm=TRUE)
  dmax = mediandepth + 50
  baddata = which( x$depth > dmax   )
  if (length(baddata) > 0) O$filtered[baddata] = NA
  
  depth.range=range(x$depth[ O$filtered], na.rm=TRUE)
  plot(depth~ts, x[ O$filtered,], sub=id, ylim=c(depth.range[2],depth.range[1]), pch=20)

  dmin = trunc( depthproportion * mediandepth )
  baddata = which(  x$depth < dmin  )
  if (length(baddata) > 0) O$filtered[baddata] = NA
  
  x$depth[ which(!is.finite(O$filtered))] = NA
  
  O$ndat = nrow(x)

  
  points(depth~ts, x, sub=id, pch=".", col="orange")
  
  if (O$ndat < n.req) {
    O$comments="Not enough data"
    return(O)
  }

  
  # high pass filter 
  
  v=inla(depth~f(ts,model="rw2"),data=x )
  predicted.values = v$summary.random[[1]][["mean"]]
  dev = x$depth - predicted.values 
  
  quants = quantile( dev, probs=c(0.025, 0.975), na.rm=TRUE)
  bad = which( dev <= quants[1] | dev >= quants[2] )
  O$filtered[ bad] = NA
  x$depth[ which(!is.finite(O$filtered))] = NA
 
  debug=FALSE
  if (debug) {
  v=inla(depth~f(ts,model="rw2"),data=x )
  predicted.values = v$summary.random[[1]][["mean"]]
  dev = x$depth - predicted.values 
  
  quants = quantile( dev, probs=c(0.025, 0.975), na.rm=TRUE)
  bad = which( dev <= quants[1] | dev >= quants[2] )
  O$filtered[ bad] = NA
  x$depth[ which(!is.finite(O$filtered))] = NA
  }


## Variance method: variance based gating using 3SD -- ie ~ 99% of population
    # compute SD in the area of interest and compare with a lagged process to 
    # determine first estimate of fishing range based upon SD in depth data
    # Prefilter to remove any data with extreme deviance from a smooth trend (a high-pass filter)
  
  variance.method = "off"
  if (variance.method=="on") {
      v=inla(depth~f(ts,model="rw2"),data=x )
      predicted.values = v$summary.random[[1]][["mean"]] + mean(x$depth, na.rm=T)
      deviance = x$depth - predicted.values
      
      quants = quantile( deviance, probs=c(0.025, 0.975), na.rm=TRUE)
      O$variance.bad = which( deviance <= quants[1] | deviance >= quants[2] )
      O$variance.good = setdiff( 1:O$ndat, O$variance.bad)
      
      target.sd =  max( 0.1, sd (x[O$variance.good], na.rm=T ), na.rm=T ) 
      
      # start from centre and move left and continue until sd of residuals begins to deviate sustantially
      
      nx2 = trunc( O$ndat/2) # midpoint
      # from centre to left 
      left = setdiff( nx2:1, O$variance.bad)
      for ( j0 in left  ) {
        if ( sd(( x$depth[ (nx2+10):j0]), na.rm=T) > target.sd ) {
          j0 = j0 - 1      
          break()
        }
      }
      
      # end time .. begin from centre to right 
      right = setdiff( nx2:O$ndat, O$variance.bad)
      for ( j1 in right ) {
        if ( sd(( x$depth[ (nx2-10):j1]), na.rm=T) > target.sd ) {
          j1 = j1 + 1
          break()
        }
      }
      
      if ( (j1-j0) < n.req) {
        O$comments="not enough data for estimating fishing method 1"
        return(O)
      } 
      
      O$variance.method = c( x$timestamp[j0], x$timestamp[j1] )
      O$variance.method.indices = which( x$timestamp >= O$variance.method[1] &  x$timestamp <= O$variance.method[2] )
      
  }
  


## Modal method: gating by looking for modal distribution and estimating sd of the modal group 
  # by removing small frequencies (5 or less) being ignored 
  # breaks defines how many columns you want in your histogram
  # expectation from uniform random is 1/2 of the threshold for testing that the freqeuncy belongs to the mode
  # ensure that high pass-filter-based rejections and simple gated filters are used

  # minval.modal = 5 # assuming a chi-square approach where any count <= 5 is essentially indistinguishable from a random number
  nbreaks =  seq( from=250, to=20, by=-2 )
  h = NULL
  for ( nb in nbreaks) {
    h = hist( x$depth[ na.omit(O$filtered) ], breaks=nb, plot=FALSE)  
    minval = min(h$counts[h$counts>0]) # chi-squared logic
    i = which(h$counts > minval.modal )
    if (length (i) > nbins[1]  & length (i) < nbins[2] )  break() # target number of good bins
  }
  
  if (is.null(h)) {
    O$comments = "No model solution found"
    return(O)
  }
      
    
  O$modal.method.filtered.indices = which( x$depth > h$mids[min(i)] & x$depth < h$mids[max(i)]  ) 
  
  # indices of x where the data distribution suggests we have bottom contact
  i0 = min( O$modal.method.filtered.indices ) 
  i1 = max( O$modal.method.filtered.indices )
  
  # points(depth~ts, x[O$modal.method.filtered.indices,], col="green", pch=20)
  
  if (length( i0:i1 ) < n.req) {
    O$comments="Not enough depth data near the median"
    return(O)  # no data 
  }
  
  # Do some rough filtering to remove sets wihout sufficient data
 
  if (length( which(is.finite(x$depth) ) )< n.req) {
    O$comments="Not enough depth data"
    return(O)  # no data 
  }
 
  
  O$modal.method = c( x$timestamp[i0], x$timestamp[i1] )
  O$modal.method.indices = which( x$timestamp >= O$modal.method[1] &  x$timestamp <= O$modal.method[2] )
  
  badlist = setdiff( 1:nrow(x), O$modal.method.filtered.indices)
  O$filtered[ badlist ] = NA  

  points(x$ts[O$modal.method.indices],  x$depth[O$modal.method.indices], col="red", pch=20)       
  tdiffmodal = round(as.numeric(diff(O$modal.method) ),2)
  abline (v=x$ts[min(O$modal.method.indices)], col="red")
  abline (v=x$ts[max(O$modal.method.indices)], col="red")

  
  if ( (i1-i0) < n.req) {
    O$comments="not enough data for estimating fishing method 1"
    return(O)
  } 
  
  
  
  
## Smooth method: smooth data to remove local trends and compute a detrended residual using INLA
    # Calculate smooth ..remove high frequency varaitions
    
    # tails  = c( 1:(min(O$filtered, na.rm=T)-1), (max(O$filtered, na.rm=T) +1):O$ndat)
    tokeep = unique( c( O$filtered) )
    todrop = setdiff( 1:nrow(x) , tokeep)

    xcopy = x
    xcopy$depth[todrop] = NA
    xcopy$ts2 =xcopy$ts

    v = inla( depth ~ ts2 + f(ts,model="rw2"), data=xcopy )
    
    O$depths.predicted = v$summary.random[[1]][["mean"]]   
    
    fun = approxfun( x$ts, O$depths.predicted )
    
    # first, recenter the smooths 
    slopes = grad( fun, x$ts, method="simple" )

    
    # looking for modal distribution and estimating sd of the modal group of slopes
    quants.slopes = quantile( slopes, probs=c(0.1, 0.9), na.rm=TRUE)
    slopes[ slopes < quants.slopes[1]] = NA
    slopes[ slopes > quants.slopes[2]] = NA

    # now using (smoothed) first derivative determine inflection points (crossing of the zero line)
    nd = length(slopes) 

    k0=NA
    for( k0 in min(O$filtered, na.rm=T):nd) {
      if (!is.finite( slopes[ k0 ] )) next()
      if (slopes[ k0 ] < 0) {
        k0 = k0 - 1
        break()
      }
    }
    
    k1 = NA
    for( k1 in max(O$filtered, na.rm=T):1) {
      if (!is.finite( slopes[ k1 ] )) next()
      if (slopes[ k1 ] > 0) {
        k1 = k1 +1
        break()
      }
    }
    
    O$smooth.method =  c( x$timestamp[k0], x$timestamp[k1] )
    O$smooth.method.indices = which( x$timestamp >= O$smooth.method[1] &  x$timestamp <= O$smooth.method[2] ) 

    points(x$ts[O$smooth.method.indices],  x$depth[O$smooth.method.indices], col="blue", pch=24)   
    tdiffsmooth = round(as.numeric(diff(O$smooth.method)),2)
    abline (v=x$ts[min(O$smooth.method.indices)], col="blue")
    abline (v=x$ts[max(O$smooth.method.indices)], col="blue")

    if ( (k1-k0) < n.req) {
      O$comments="not enough data for estimating smooth method"
    } 
  
  
  
  
  
## Linear method: looking at the intersection of three lines (up, bot and down)
  
    if(!is.na(O$comments)){
      return(O)  # no data 
    }

    
    # ID best model based upon distance of the start/end depth to the median depth of the bottom track
    meddepth = median(x$depth[O$filtered], na.rm=TRUE)  # current.best.estimate.of.depth
    dev.smooth =  meddepth - ( x$depth[min(O$smooth.method.indices)]+ x$depth[max(O$smooth.method.indices)] )/2
    dev.modal =  meddepth - ( x$depth[min(O$modal.method.indices)]+ x$depth[max(O$modal.method.indices)] )/2
    dev.blend = (dev.smooth + dev.modal) / 2
  
    results = c( dev.smooth, dev.modal, dev.blend) 
    best = which.min( results)

    # find the row numbers which corresponds to each part of the curve, decent, bottom and ascent
    
    fishing = switch( best,
      range(O$smooth.method.indices),
      range(O$modal.method.indices),
      trunc( c( mean( c( min(O$smooth.method.indices), min(O$modal.method.indices ))), 
            mean( c( max(O$smooth.method.indices ), max(O$modal.method.indices))) ) )
    )

  
    bot = fishing[1]: fishing[2]  # blended estiamte of fishing events
    down = 1:( fishing[1] - 1) 
    up = ( fishing[2] + 1):O$ndat

    # data checks
  
    if( length( which( is.finite( x$depth[bot] ))) < n.req ){
      O$comments="Not enough depth data in linear method"
      return(O)  # no data 
    }
    
    if( length( which( is.finite( x$depth[up] ))) < 5 ){
      O$comments="Not enough depth data in linear method for up direction"
      return(O)  # no data 
    }
    if(length( which( is.finite( x$depth[down] ))) < 5 ){
      O$comments="Not enough depth data in linear method for down direction"
      return(O)  # no data 
    }

    # as above return tails to the filtered data

    tails  = c( 1:(min(O$filtered, na.rm=T)-1), (max(O$filtered, na.rm=T) +1):O$ndat)
    tokeep = unique( c( tails, O$filtered) )
    todrop = setdiff( 1:nrow(x) , tokeep)
    
    xcopy = x
    xcopy$depth[todrop] = NA

   
    # compute linear models for each section
    botlm=  lm(depth~ts, xcopy[bot, ], na.action = "na.exclude")
    downlm= lm(depth~ts, xcopy[down, ], na.action = "na.exclude")
    uplm=   lm(depth~ts, xcopy[up, ], na.action = "na.exclude")
    
    # compute weights
    wbot = 1/(rstandard(botlm))^2
    wdown = 1/(rstandard(downlm))^2
    wup = 1/(rstandard(uplm))^2
  
    # removing infinite value due to division by small number
    wbot[which(!is.finite(wbot))] = max(wbot, na.rm=TRUE)
    wdown[which(!is.finite(wdown))] = max(wdown, na.rm=TRUE)
    wup[which(!is.finite(wup))] = max(wup, na.rm=TRUE)
    
    botlm2= lm(depth~ts, xcopy[bot, ], na.action = "na.exclude", weights = wbot)
    
    downlm2= lm(depth~ts, xcopy[down, ], na.action = "na.exclude", weights = wdown)
    uplm2= lm(depth~ts, xcopy[up, ], na.action = "na.exclude", weights = wup)
    
    # find where the sections intersect
    cm <- rbind(coef(botlm2),coef(downlm2)) # Coefficient matrix
    i1=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
    ri1 = max(which(x$ts <i1[1]))
    
    cm <- rbind(coef(botlm2),coef(uplm2)) # Coefficient matrix
    i2=c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
    ri2 = min(which(x$ts >i2[1]))
  

    O$linear.method =  c( x$timestamp[ri1], x$timestamp[ri2] )
    O$linear.method.indices = which( x$timestamp >= O$linear.method[1] &  x$timestamp <= O$linear.method[2] ) 


    # done .. now some plots

    points(x$ts[O$linear.method.indices],  x$depth[O$linear.method.indices], col="green", pch=22)      
    tdifflinear = round(as.numeric(diff(O$linear.method)),2)
    abline (v=x$ts[min(O$linear.method.indices)], col="green")
    abline (v=x$ts[max(O$linear.method.indices)], col="green")

    legendtext = c(paste( "modal:   ", tdiffmodal),
                   paste( "linear:   ", tdifflinear),
                   paste( "smooth: ", tdiffsmooth) )
               
    legend( "topleft", legend=legendtext, col=c("red", "green", "blue"), pch=c(20,22,24) )
    #abline (reg=botlm2, col="gray")      
    #abline (reg=uplm2, col="gray")      
    #abline (reg=downlm2, col="gray")      

O$filtered.data = x
  return( O )

  
}



