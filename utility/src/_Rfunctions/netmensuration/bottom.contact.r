
bottom.contact = function( id="noid", x, tdif.min=3, tdif.max=15, depthproportion=0.5, smoothing = 0.9, eps.depth=1, 
        filter.quants=c(0.025, 0.975), sd.multiplier=3, depth.min=20, depth.range=c(-30,30), 
        user.interaction=FALSE, setdepth=NULL,  time.gate=NULL, inla.h=0.05, inla.diagonal=0.05 ) {
  
  #require(lubridate) 
  #require( numDeriv ) 

  debug = FALSE
  if (debug) {
    x = mm
    tdif.min = 15  # min time difference (minutes) .. including tails
    tdif.max = 45  # min time difference (minutes) .. including tails
    depthproportion=0.6 # depthproportion controls primary (coarse)gating
    eps.depth=4  # for noise filtering  .. ignore variations less than this threshold
    filter.quants=c(0.025, 0.975)
    sd.multiplier=5
    depth.min=10
    depth.range=c(-50, 50)
    smoothing = 0.9
    filter.quants=c(0.025, 0.975)
    setdepth=NULL
    user.interaction=FALSE # is you want to try to manually determine end points too
  }
  
  n.min.required = 30 

  O = list()  # output list
  O$id = id
  O$good = rep(TRUE, nrow(x)) # rows that will contain data that passes each step of data quality checks

  if(sum(!is.na(x$depth)) < n.min.required ) return( NULL )

  ##--------------------------------
  # sort in case time is not in sequence
  # timestamps have frequencies higher than 1 sec .. duplciates are created and this can pose a problem
  x = x[order( x$timestamp ) ,]
  x$ts = as.numeric( difftime( x$timestamp, min(x$timestamp), units="secs" ) )
  
  # O$Z = x$depth  ## copy of depth data before manipulations ... not used? 
  
  O$plotdata = x   # save incoming data after creation of time stamps and ordering 


  ##--------------------------------
  # basic depth gating
  
  if( !any(x$depth>depth.min)) return( NULL ) 

  # simple time-based gating ..
  if (!is.null(time.gate)) {
    O$good = bottom.contact.gating.time ( Zt=x$timestamp, good=O$good, time.gate=time.gate, min.n=(tdif.max+5) )
    x$depth[ !O$good ] = NA
  }
 
  # simple depth-based gating
  O$good = bottom.contact.gating.depth ( Z=x$depth, good=O$good, depth.min=depth.min, depth.range=depth.range,
    depthproportion=depthproportion, setdepth=setdepth )
  x$depth[ !O$good ] = NA

  
  # time and depth-based gating
  x.timerange = range( x$timestamp, na.rm=TRUE )
  x.dt = difftime( x.timerange[2], x.timerange[1], units="mins" )

  if ( x.dt > (tdif.max+10) ) {   # +10 = 5 min on each side
    # data vector is too long ... truncate 
    if (is.null(setdepth)) {
      zx = which.max( x$depth )
      shallow = x$depth[zx] / 4 
    } else {
      zx = median( which( x$depth < (setdepth + 5 ) & x$depth > (setdepth - 5 ) ) )  # median location bounded byu +/- 5 m
      shallow = setdepth / 4 
    }
    ss = which( x$depth < shallow ) 
    if (length( ss) > 30 ) {
      x$depth[ss] = NA
      x$timestamp[ss] = NA
    }
    timerange = new_interval(  (x$timestamp[zx] -dminutes( tdif.max+5) ),  ( x$timestamp[zx]+ dminutes( tdif.max+5) ) )
    oo = which( ! x$timestamp %within% timerange)
    O$good[oo] = FALSE
    x$depth[ !O$good ] = NA
  }

  ## ------------------------------
  # Some filtering of noise from data and further focus upon area of interest based upon time and depth if possible
   
  if(sum(x$depth-min(x$depth,na.rm=T),na.rm=T)==0) return( NULL )
  if(sum(O$good)==0) return( NULL )
  
  
  # variance gating attempt
  O$variance.method = c(NA, NA)
  O$variance.method.indices = NA
  x$depth0 = x$depth  # for plotting only, later of the unfiltered data
  x$depth.smoothed  = x$depth

  res = NULL
  res = try( 
    bottom.contact.filter.noise ( x, O$good, tdif.min, tdif.max, eps.depth=eps.depth,
      smoothing = smoothing, filter.quants=filter.quants, sd.multiplier=sd.multiplier, inla.h=inla.h, inla.diagonal=inla.diagonal ), silent =TRUE)
  
  if ( ! "try-error" %in% class( res) )  {
    x$depth.smoothed = res$depth.smoothed
    O$good = res$good
    O$variance.method = res$variance.method
    O$variance.method.indices  = res$variance.method.indices
    x$depth[ !O$good ] = NA
  }
    
  ## NOTE::: From this point on, O$good is now complete 
  ## -- it contains indices of time and depth-based gating as well as the variance based gating

 
  # Now that data is more or less clean ... 
  # create a variable with any linear trend in the depths removed as this can increase the precision of some of 
  # the following methods
  ib = range( which(O$good) )
  ib.n = ib[2] - ib[1] # this is the length from variance gating ... trim 1/3 from left and right
  ib.buf = trunc( ib.n / 5 )
  ib.guess = ( ib[1] + ib.buf) : ( ib[2] - ib.buf )
  depthtrend.smoothed = lm( depth.smoothed ~ ts, data=x[ib.guess,], na.action="na.omit")  # deeper weights have higher influence (reduce influence of tails )
  x$depth.residual = x$depth.smoothed - predict( depthtrend.smoothed, newdata=x ) + median( x$depth.smoothed, na.rm=TRUE )
  
  # finalize selection of area of interest (based upon gating, above)
  aoi.range = range( which( O$good )  )
  aoi.mid = trunc( mean( aoi.range ) ) # approximate midpoint
  aoi.min = aoi.range[1]
  aoi.max = aoi.range[2]
  O$aoi = aoi.min:aoi.max  # stored for use in the linear method where we need to recover the left-most index 
  
  stop() ## fix the following ...

  O$aoi.good = O$aoi[which( O$good)]  # used for indexing good values within aoi
  sm0=x[ O$aoi, ]  # used for methods that require only data from the area of interest 


  ##--------------------------------
  # Modal method: gating by looking for modal distribution and estimating sd of the modal group in the data 
  # first by removing small densities ( 1/(length(i)/nb)  ) and by varying the number of breaks in the histogram
  # until a target number of breaks, nbins with valid data are found
  # use the depth.residual as smoothed one has insufficient variation

  O$modal.method = c(NA, NA)
  O$modal.method.indices = NA
  sm0 = sm0[ , c("depth.residual", "timestamp", "ts" ) ]  # send filtered data ... continuity not important
  sm0$depth.residual[ !smgood] = NA 
  res = NULL
  res = try( bottom.contact.modal( sm=sm0, density.factor=5, kernal.bw.method="SJ" ), silent=TRUE )
    if ( ! "try-error" %in% class( res) ) {
      if ( all(is.finite(res) ) ) {
        duration =  as.numeric( difftime( res[1], res[2], units="mins" ) )
        if ( duration > tdif.min & duration < tdif.max ) {
          O$modal.method = res
          O$modal.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
        }
      }
    }  
    

  ## ---------------------------- 
  ## Smooth method: using smoothed data (slopes are too unstable with raw data), 
  ## compute first derivatives to determine when the slopes inflect 

  O$smooth.method = c(NA, NA)
  O$smooth.method.indices = NA
  sm0 = sm0[, c("depth.smoothed", "timestamp", "ts")]  # Send all data within the aoi ... filtered ? --- check this
  res = NULL
  res = try( 
    bottom.contact.smooth( sm=sm0, target.r2=smoothing, filter.quants=filter.quants ) , silent =TRUE)
    if ( ! "try-error" %in% class( res) ) {
      if ( all(is.finite(res) ) ) {
        duration =  as.numeric( difftime( res[1], res[2], units="mins" ) )
        if ( duration > tdif.min & duration < tdif.max ) {
          O$smooth.method = res
          O$smooth.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
        }
      }
    }  
    

  ## ---------------------------- 
  ## Incremental method: using unsmoothed data compute slopes in a moving window to determine when the slopes cross 
  ## the modal bounds of the distribution of slopes 

  O$incremental.method = c(NA, NA)
  O$incremental.method.indices = NA
  sm1 = sm0[ , c("depth", "timestamp", "ts") ]
#   sm1$depth [ !O$good=] = NA
  res = NULL
  res = try( bottom.contact.incremental( sm=sm1, good=O$good, nx=10 ) , silent =TRUE)
  if ( ! "try-error" %in% class( res) ) {
    if ( all(is.finite( res ) ) ) {
      duration =  as.numeric( difftime( res[1], res[2], units="mins" ) )
      if ( duration > tdif.min & duration < tdif.max ) {
        O$incremental.method = res
        O$incremental.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
      }
    }
  }  
    


  ## ---------------------------- 
  ## Intersect method: looking at the intersection of a perpendicular line onto the trajectory of the profile
  O$intersect.method = c(NA, NA) 
  O$intersect.method.indices = NA
     
  if ( FALSE ) {  # turn off for now .. not working reliably
    res = NULL
    res = try( bottom.contact.intersect( sm=sm0[, c("depth", "timestamp", "ts")] ), silent=TRUE ) 
    if ( ! "try-error" %in% class( res) ) {
      if ( all(is.finite( res ) ) ) {
        duration =  as.numeric( difftime( res[1], res[2], units="mins" ) )
        if ( duration > tdif.min & duration < tdif.max ) {
          O$intersect.method = res
          O$intersect.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
        }
      }
    }  
  } 

  ## ---------------------------
  ## Linear method: looking at the intersection of three lines (up, bot and down)
 
  O$linear.method = c(NA, NA)
  O$linear.method.indices = NA
  res = NULL
  res = try( bottom.contact.linear( sm=sm0[, c("depth.residual", "timestamp", "ts" )], O=O) , silent=TRUE )
  if ( ! "try-error" %in% class( res) ) {
    if ( all(is.finite( res ) ) ) {
      duration =  as.numeric( difftime( res[1], res[2], units="mins" ) )
      if ( duration > tdif.min & duration < tdif.max ) {
        O$linear.method = res
        O$linear.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
      }
    }
  }  
  
   
  ## ---------------------------
  
  O$manual.method = c(NA , NA)
  if ( user.interaction  ) { 
    print( "Click with mouse on start and stop locations now.")          
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = c( quantile( x$depth, c(0.05, 0.975), na.rm=TRUE) , median( x$depth, na.rm=TRUE ) * 1.05 )
    plot(depth~ts, x, ylim=c(drange[2],drange[1]), xlim=c(trange[1],trange[2]), pch=20, cex=0.1, col="gray" )
    points( depth~ts, x[O$good,], pch=20, col=mcol, cex=0.2)

    useridentified = locator( n=2, type="o", col="cyan")
    u.ts0 = which.min( abs( x$ts-useridentified$x[1] ))
    u.ts1 = which.min( abs( x$ts-useridentified$x[2] ))
    O$manual.method = c( x$timestamp[u.ts0], x$timestamp[ u.ts1 ]  )
    O$manual.method.indices = which( x$timestamp >= O$manual.method[1] &  x$timestamp <= O$manual.method[2] ) 
  }
 
  O$means = c(NA, NA )
  methods = c("manual.method", "variance.method", "smooth.method", "incremental.method", "modal.method", "linear.method", "means" )
  standard =  which( methods=="manual.method")
  direct = which( methods %in%  c("smooth.method", "modal.method", "incremental.method", "linear.method" ) )
  
  tzone = tz( x$timestamp[1] )

  tmp = as.data.frame(O[methods], stringsAsFactors=FALSE )
  tmp = as.data.frame( t(tmp), stringsAsFactors=FALSE  )
  colnames(tmp) =c("start", "end" )
  tmp[,"start"]  = as.POSIXct( tmp[,"start"], origin= "1970-01-01", tz=tzone )
  tmp[,"end"]    = as.POSIXct( tmp[,"end"], origin= "1970-01-01" , tz=tzone  )
  tmp["means", "start"] = as.POSIXct( mean( tmp[ direct, "start" ], na.rm=TRUE ) , origin= "1970-01-01", tz=tzone  )
  tmp["means", "end"] = as.POSIXct( mean( tmp[ direct, "end" ], na.rm=TRUE ) , origin= "1970-01-01", tz=tzone  )
 
  O$bottom0 = NA
  O$bottom1 = NA
  O$bottom0.sd = NA
  O$bottom1.sd = NA
  O$bottom0.n = NA
  O$bottom1.n = NA
  O$bottom.diff = NA

  if ( any (is.na( tmp[ standard, c("start", "end")] ) ) ) {
    # no manual standard .. use mean as the standard
    O$bottom0 = tmp["means", "start"]
    O$bottom1 = tmp["means", "end" ]
    O$bottom0.sd = sd(  ( tmp[ direct, "start" ]), na.rm=TRUE ) # in secconds
    O$bottom1.sd = sd(  ( tmp[ direct, "end" ]), na.rm=TRUE )
    O$bottom0.n = length( which( is.finite( tmp[ direct, "start" ] )) )
    O$bottom1.n = length( which( is.finite( tmp[ direct, "end" ] )) )
    O$bottom.diff =  difftime( O$bottom1, O$bottom0, units="secs" )
  } else {
    # over-ride all data and use the manually determined results
    O$bottom0 = tmp[ standard, "start" ]
    O$bottom1 = tmp[ standard, "end" ]
    O$bottom0.sd = NA
    O$bottom1.sd = NA
    O$bottom0.n = length( which( is.finite( tmp[ direct, "start" ] )) )
    O$bottom1.n = length( which( is.finite( tmp[ direct, "end" ] )) )
    O$bottom.diff = difftime( O$bottom1, O$bottom0, units="secs" )
  }

  tmp$diff = difftime( tmp$end, tmp$start, units="secs" )
  tmp$start.bias =  difftime( tmp$start,  O$bottom0, units="secs" )
  tmp$end.bias   = difftime( tmp$end,  O$bottom1, units="secs" )

  O$summary = tmp

  # finalised data which have been filtered 
  fin.all = which( x$timestamp > O$bottom0 & x$timestamp < O$bottom1 )  
  if (length( fin.all ) == 0 ) fin.all = min( which( O$good)) : max( which( O$good) )
  fin.good = intersect( fin.all, which( O$good)  )
  fin0 = min( fin.all, na.rm=TRUE)
  fin1 = max( fin.all, na.rm=TRUE)

  O$depth.mean = mean( x$depth[ fin.good ], na.rm=TRUE )
  O$depth.sd = sd( x$depth[ fin.good ], na.rm=TRUE)
  O$depth.n = length( fin.good )
  O$depth.n.bad = length( fin.all) - O$depth.n
  O$depth.smoothed.mean =  mean( x$depth.smoothed[ fin0:fin1 ], na.rm=TRUE )
  O$depth.smoothed.sd = sd( x$depth.smoothed[ fin0:fin1 ], na.rm=TRUE )
  O$depth.goodvalues = O$good
  O$depth.filtered = fin.good
  O$depth.smoothed = x$depth.smoothed
  O$ts = x$ts
  O$timestamp = x$timestamp
  O$signal2noise = O$depth.n / length( fin.all )  # not really signal to noise but rather  % informations 

  O$bottom.contact = rep( FALSE, nrow(x) )
  O$bottom.contact[ fin.all ] = TRUE
  
  # for minilog and seabird data .. we have temperature estimates to make ..
  tmean= NA
  tmeansd = NA
  if (exists( "temperature", x )) {  
    tmean = mean( x$temperature[fin.all], na.rm=TRUE )
    tmeansd = sd( x$temperature[fin.all], na.rm=TRUE )
  }
#if(is.na(O$bottom0) ) browser()
  O$res = data.frame( cbind(z=O$depth.mean, t=tmean, zsd=O$depth.sd, tsd=tmeansd, 
                            n=O$depth.n, t0=O$bottom0, t1=O$bottom1, dt=O$bottom.diff ) ) 

  print( O$summary)
  O$good = NULL

  return( O )

}





