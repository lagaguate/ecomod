
bottom.contact = function( x, bcp ) {
  
  #require(lubridate) 
  #require( numDeriv ) 

  debug = FALSE
  if (debug) {
    x = mm
    bcp = bottom.contact.parameters( data.source="groundfish", YR="default", nr="default" ) 
    debug.plot = TRUE
  }
  
  debug.plot = FALSE
  n.min.required = 30 

  O = list()  # output list
  O$id = bcp$id
  O$error.flag = NA
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
  
  if( !any(x$depth > bcp$depth.min)) return( NULL ) 

  # simple time-based gating ..
  if (!is.na(bcp$time.gate)) {
    bcp$trange.max = (bcp$tdif.max+5)
    O$good = bottom.contact.gating.time ( Zt=x$timestamp, good=O$good, bcp=bcp )
    x$depth[ !O$good ] = NA
  }
 
  # simple depth-based gating
  O$good = bottom.contact.gating.depth ( Z=x$depth, good=O$good, bcp=bcp) 
  x$depth[ !O$good ] = NA
  
  # time and depth-based gating
  x.timerange = range( x$timestamp, na.rm=TRUE )
  x.dt = difftime( x.timerange[2], x.timerange[1], units="mins" )

  if ( x.dt < (bcp$tdif.min ) ) { 
      O$error.flag = "Time track is too short?"   
      return(O)
  }

  if ( x.dt > (bcp$tdif.max+10) ) {   # +10 = 5 min on each side
    # data vector is too long ... truncate 
    mm = modes( x$depth  )
    mm.i = which( x$depth > mm["mean", "lb"] & x$depth < mm["mean", "ub"] )
    mm.r = range( mm.i )
    mm.dt = difftime( x$timestamp[mm.r[2]], x$timestamp[mm.r[1]], units="mins" )
    O$good[ setdiff(1:nrow(x), mm.i)] = FALSE
    O$good[mm.i] = TRUE

    if ( mm.dt > (bcp$tdif.max+10) ) {

      O$error.flag = "Time track is too long?"   
      if (is.null(bcp$setdepth)) {
        zx = which.max( x$depth )
        shallow = x$depth[zx] / 4 
      } else {
        zx = median( which( x$depth < (bcp$setdepth + 5 ) & x$depth > ( bcp$setdepth - 5 ) ) )  # median location bounded byu +/- 5 m
        shallow = bcp$setdepth / 4 
      }
      ss = which( x$depth < shallow ) 
      if (length( ss) > 30 ) {
        x$depth[ss] = NA
        x$timestamp[ss] = NA
      }
      timerange = new_interval(  (x$timestamp[zx] -dminutes( bcp$tdif.max+5) ),  ( x$timestamp[zx]+ dminutes( bcp$tdif.max+5) ) )
      oo = which( ! x$timestamp %within% timerange)
      O$good[oo] = FALSE
      x$depth[ !O$good ] = NA
      return(O)
    }
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
  res = try( bottom.contact.filter.noise ( x, O$good, bcp ), silent =TRUE )

  if ( ! "try-error" %in% class( res) )  {
    x$depth.smoothed = res$depth.smoothed
    O$good = res$good
    O$variance.method = res$variance.method
    O$variance.method.indices  = res$variance.method.indices
    x$depth[ !O$good ] = NA
  }

  if(debug.plot) {
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = c( quantile( x$depth, c(0.05, 0.975), na.rm=TRUE) , median( x$depth, na.rm=TRUE ) * 1.05 )
    plot(depth~ts, x, ylim=c(drange[2],drange[1]), xlim=c(trange[1],trange[2]), pch=20, cex=0.1, col="gray" )
    mcol = "gray"
    points( depth~ts, x[ O$variance.method.indices, ], pch=20, col=mcol, cex=0.2)
  }


  ## NOTE::: From this point on, O$good is now complete 
  ## -- it contains indices of time and depth-based gating as well as the variance based gating

 
  # finalize selection of area of interest (based upon gating, above)
  aoi.range = range( which( O$good )  )
  aoi.mid = trunc( mean( aoi.range ) ) # approximate midpoint
  aoi.min = aoi.range[1]
  aoi.max = aoi.range[2]
  O$aoi = aoi.min:aoi.max  # stored for use in the linear method where we need to recover the left-most index 
 
  ##--------------------------------
  # Modal method: gating by looking for modal distribution and estimating sd of the modal group in the data 
  # first by removing small densities ( 1/(length(i)/nb)  ) and by varying the number of breaks in the histogram
  # until a target number of breaks, nbins with valid data are found
  O$modal.method = c(NA, NA)
  O$modal.method.indices = NA
  sm0 = x[ O$aoi, c("depth.smoothed", "timestamp", "ts" ) ]  # send filtered data ... continuity not important .. order is important
  res = NULL
  res = try( bottom.contact.modal( sm=sm0, bcp ), silent=TRUE )
    if ( ! "try-error" %in% class( res) ) {
      if ( all(is.finite(res) ) ) {
        duration =  abs( as.numeric( difftime( res[1], res[2], units="mins" ) ) )
        if (is.finite(duration) &&  duration > bcp$tdif.min & duration < bcp$tdif.max ) {
          O$modal.method = res
          O$modal.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
        }
      }
    }  
     
  if(debug.plot) {
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = c( quantile( x$depth, c(0.05, 0.975), na.rm=TRUE) , median( x$depth, na.rm=TRUE ) * 1.05 )
    plot(depth~ts, x, ylim=c(drange[2],drange[1]), xlim=c(trange[1],trange[2]), pch=20, cex=0.1, col="gray" )
    mcol = "green"
    points( depth~ts, x[ O$modal.method.indices, ], pch=20, col=mcol, cex=0.2)
  }

  

  ## ---------------------------- 
  ## Smooth method: using smoothed data (slopes are too unstable with raw data), 
  ## compute first derivatives to determine when the slopes inflect 

  O$smooth.method = c(NA, NA)
  O$smooth.method.indices = NA
  sm0 = x[ O$aoi, c("depth.smoothed", "timestamp", "ts")]  # Send all data within the aoi --- check this .. order is important
  res = NULL
  res = try( 
    bottom.contact.smooth( sm=sm0, bcp=bcp ) , silent =TRUE)
    if ( ! "try-error" %in% class( res) ) {
      if ( all(is.finite(res) ) ) {
        duration =  abs( as.numeric( difftime( res[1], res[2], units="mins" ) ) )
        if (is.finite(duration) &&  duration > bcp$tdif.min & duration < bcp$tdif.max ) {
          O$smooth.method = res
          O$smooth.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
        }
      }
    }  
      
  if(debug.plot) {
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = c( quantile( x$depth, c(0.05, 0.975), na.rm=TRUE) , median( x$depth, na.rm=TRUE ) * 1.05 )
    plot(depth~ts, x, ylim=c(drange[2],drange[1]), xlim=c(trange[1],trange[2]), pch=20, cex=0.1, col="gray" )
    mcol = "blue"
    points( depth~ts, x[ O$smooth.method.indices, ], pch=20, col=mcol, cex=0.2)
  }

  

  ## ---------------------------- 
  ## maxdepth method: looking for the max depth near the areas of interest (left, right) 
  O$maxdepth.method = c(NA, NA) 
  O$maxdepth.method.indices = NA
  sm0 = x[, c("depth.smoothed", "timestamp", "ts")]  # Send all data within the aoi --- check this .. order is important
  sm0$depth[ !O$good ] = NA
  sm0 = sm0[ O$aoi, ]

  bcmethods=c( "smooth.method", "modal.method" )
  res = NULL
  res = try( bottom.contact.maxdepth( sm=sm0, O=O, bcmethods=bcmethods, bcp=bcp ) , silent=TRUE ) 
  
  if ( ! "try-error" %in% class( res) ) {
    if ( all(is.finite( res ) ) ) {
      duration = abs( as.numeric( difftime( res[1], res[2], units="mins" ) ) )
      if ( is.finite(duration) && duration > bcp$tdif.min & duration < bcp$tdif.max ) {
        O$maxdepth.method = res
        O$maxdepth.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
      }
    }
  }  
 
  if(debug.plot) {
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = c( quantile( x$depth, c(0.05, 0.975), na.rm=TRUE) , median( x$depth, na.rm=TRUE ) * 1.05 )
    plot(depth~ts, x, ylim=c(drange[2],drange[1]), xlim=c(trange[1],trange[2]), pch=20, cex=0.1, col="gray" )
    mcol = "yellow"
    points( depth~ts, x[ O$maxdepth.method.indices, ], pch=20, col=mcol, cex=0.2)
  }



  ## ---------------------------
  ## Linear method: looking at the intersection of three lines (up, bot and down)
 
  O$linear.method = c(NA, NA)
  O$linear.method.indices = NA
  sm0 = x[, c("depth.smoothed", "timestamp", "ts")]  # Send all data within the aoi --- check this .. order is important
  sm0$depth[ !O$good ] = NA
  sm0 = sm0[ O$aoi, ]
 
  res = NULL
  res = try( bottom.contact.linear( sm=sm0, O=O, bcp=bcp ) , silent=TRUE )

  if ( ! "try-error" %in% class( res) ) {
    if ( all(is.finite( res ) ) ) {
      duration =  abs( as.numeric( difftime( res[1], res[2], units="mins" ) ) )
      if ( is.finite(duration) && duration > bcp$tdif.min & duration < bcp$tdif.max ) {
        O$linear.method = res
        O$linear.method.indices = which( x$timestamp >= res[1] &  x$timestamp <= res[2] ) # x correct
      }
    }
  }  
  
 
  if(debug.plot) {
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = c( quantile( x$depth, c(0.05, 0.975), na.rm=TRUE) , median( x$depth, na.rm=TRUE ) * 1.05 )
    plot(depth~ts, x, ylim=c(drange[2],drange[1]), xlim=c(trange[1],trange[2]), pch=20, cex=0.1, col="gray" )
    mcol = "red"
    points( depth~ts, x[ O$linear.method.indices, ], pch=20, col=mcol, cex=0.2)
  }


  ## ---------------------------
  
  O$manual.method = c(NA , NA)
  if ( bcp$user.interaction  ) { 
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
  methods = c("manual.method", "variance.method", "smooth.method", "modal.method", "maxdepth.method", "linear.method", "means" )
  standard =  which( methods=="manual.method")
  direct = which( methods %in%  c("smooth.method", "modal.method", "linear.method", "maxdepth.method" ) )
  
  tzone = tz( x$timestamp[1] )

  tmp = as.data.frame(O[methods], stringsAsFactors=FALSE )
  tmp = as.data.frame( t(tmp), stringsAsFactors=FALSE  )
  colnames(tmp) =c("start", "end" )
  tmp[,"start"]  = as.POSIXct( tmp[,"start"], origin= "1970-01-01", tz=tzone )
  tmp[,"end"]    = as.POSIXct( tmp[,"end"], origin= "1970-01-01" , tz=tzone  )
  tmp["means", "start"] = as.POSIXct( mean( tmp[ direct, "start" ], trim=0.1, na.rm=TRUE ) , origin= "1970-01-01", tz=tzone  )
  tmp["means", "end"] = as.POSIXct( mean( tmp[ direct, "end" ], trim=0.1, na.rm=TRUE ) , origin= "1970-01-01", tz=tzone  )
 
  O$bottom0 = NA
  O$bottom1 = NA
  O$bottom0.sd = NA
  O$bottom1.sd = NA
  O$bottom0.n = NA
  O$bottom1.n = NA
  O$bottom.diff = NA
  O$bottom.diff.sd = NA

  if ( any (is.na( tmp[ standard, c("start", "end")] ) ) ) {
    # no manual standard .. use mean as the standard
    O$bottom0 = tmp["means", "start"]
    O$bottom1 = tmp["means", "end" ]
    O$bottom0.sd = sd(  ( tmp[ direct, "start" ]), na.rm=TRUE ) # in secconds
    O$bottom1.sd = sd(  ( tmp[ direct, "end" ]), na.rm=TRUE )
    O$bottom0.n = length( which( is.finite( tmp[ direct, "start" ] )) )
    O$bottom1.n = length( which( is.finite( tmp[ direct, "end" ] )) )
    O$bottom.diff =  difftime( O$bottom1, O$bottom0, units="secs" )
    O$bottom.diff.sd = sqrt(O$bottom0.sd ^2 + O$bottom0.sd^2) # sec 
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
  O$good = O$good
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
  O$res = data.frame( cbind(z=O$depth.mean, t=tmean, zsd=O$depth.sd, tsd=tmeansd, 
                            n=O$depth.n, t0=O$bottom0, t1=O$bottom1, dt=O$bottom.diff ) ) 
 
  if(debug.plot) {
    trange = range( x$ts[O$good], na.rm=TRUE )
    drange = c( quantile( x$depth, c(0.05, 0.975), na.rm=TRUE) , median( x$depth, na.rm=TRUE ) * 1.05 )
    plot(depth~ts, x, ylim=c(drange[2],drange[1]), xlim=c(trange[1],trange[2]), pch=20, cex=0.1, col="gray" )
    mcol = "yellow"
    points( depth~ts, x[ O$maxdepth.method.indices, ], pch=20, col=mcol, cex=0.2)
  }


  print( O$summary)

  return( O )

}





