  bottom.contact = function(x, settimestamp=NULL, setdepth=NULL ) {
        
    require(mgcv)
    n.req = 30 
    
    x$chron = string2chron( x$chron )
    x$tinc = c(1:nrow(x))
    
    O = data.frame( cbind(z=NA, t=NA, zsd=NA, tsd=NA, n=NA, t0=NA, t1=NA, dt=NA ) ) 
    
    if (length( which(is.finite(x$depth) ) )< n.req) return(O)  # no data 
    
    if (!is.null( settimestamp) ) {
      O$t0=settimestamp
      settimestamp = string2chron( settimestamp )  
    }
   
    if (!is.null( setdepth) )  O$z=setdepth

    problem =F
    if (nrow(x) < n.req) problem=T
    
    # select based upon time stamps
    if ( !is.null( settimestamp ) ) {
      jj = which( x$chron > (settimestamp-2/24/60 ) & x$chron < (settimestamp+10/24/60 )  ) # 2min prior to settimestamp AND 8 MINUTES AFTER settimestamp (some are really this long)
      if ( length(jj) > n.req ) {
        r = range( jj ) 
        x = x[ r[1] : r[2], ]
      }
    }

    # depths examined to identifythe deepest point. 
    # As there are occasionally strong extremes from erroneous pings, 
    # a modelled (smoothed prediction is used to determine deepest point.
      
      gdepth= try( gam( depth~s(tinc, k=6, bs="ts"), data=x ), silent=T )
      if ( "try-error" %in% class( gdepth) ) {
       # remove the very largest in case of noise/errors
        qz = which( x$depth < quantile( x$depth, 0.95, na.rm=T ) )
        maxz = which.max( x$depth[qz] )
        if (length(maxz)>n.req) bottom = qz[maxz]  
      } else {
        x$depthsmoothed = predict( gdepth, newdata=x )
        bottom = which.max(x$depthsmoothed)
      }
      
      O$z = x$depth[bottom]
      x = x[ which( x$depth > ( O$z - 25 ) ) ,]  # drop anything greater than 10 m from deepest point 

      # drop data too far from the deepest point 
      Cx = x$chron[bottom]
      dropmin = 10  # do not touch  .. some are really quite long
      jj = which( abs( as.numeric(x$chron - Cx)*24*60 ) < dropmin )
      if ( length(jj) > n.req ) {
        r = range( jj ) 
        x = x[ r[1] : r[2], ]
      }
       
      nx = nrow(x)
      if ( nx < n.req ) {
        O$dt = as.character( O$dt )
        O$t0 = as.character( O$t0 )
        O$n = nx
        return( O)
      }
      
      # detrend any linear trends to allow better resolution of start/stop
      # define reference area 
      i0 = floor(nx*1/5)
      i1 = floor(nx*2.5/5)
      if ( !is.null( settimestamp ) ) {
        threshold= 15/60/60/24 # within 15 seconds of "touchdown"
        i0ref = which( as.numeric( x$chron - settimestamp) <= threshold ) 
        if (length(i0ref) > 0) {
          i0 = max(i0ref) # the time in the middle ("closest") to set "touchdown"
          i1ref = which( as.numeric(x$chron - (x$chron[i0] + 4.5/60/24)) <= threshold ) # offset 4.5 minutes
          if (length(i1ref) > 0) {
            i1 = max(i1ref) # the time closest to ref
          } else {
            i1 = i0 + 90 # 3sec intervals , 4.5 minutes to center
      } } }

      zlm = try( lm( depth ~ tinc, x[ i0:i1, ] ) )
      if ("try-error" %in% class( zlm)) {
        x$zresid = x$depth
        target.sd0 = 0.1
      } else {
        target.sd0 = 0.01
        x$zresid = x$depth - predict( zlm, newdata=x ) 
      } 
      
      target.sd = max( target.sd0, sd (x$zresid[i0:i1], na.rm=T ), na.rm=T ) 

      # select based on depths: remove large residuals iteratively
      # 5 m is resolution of depth; range is 1 to 2 m; ~ 10 m range / 80 to 100 data points 
      # gives and expected SD~1  .. prevent from going to zero
      for (cnt in 1:12 ) {
        qt = quantile( x$zresid, c(0.1, 0.95) , na.rm=T) # asymetrical as most data extemes are in the lower tail
        iq = which( x$zresid >= qt[1] & x$zresid <= qt[2]  ) # trim 10% of the shallower depths and recompute SD
        if (length(iq) < n.req ) break() 
        Zsd = max( target.sd0, sd( x$zresid[iq] ) )  # force sd > 1/10 of the approx SD of usual depth variability as a hard lower bound
        k = range( iq, na.rm=T ) 
        dk = k[2] - k[1]
        if (dk < n.req ) break()
        x = x[ k[1] : k[2], ]
        O$z = mean( x$depth, na.rm=T ) # update these in case there is no more data
        O$zsd = sd( x$depth, na.rm=T )  
        O$n = nrow(x)
        if ( Zsd <= target.sd | problem ) break()
      }
  
    if ( !is.null( setdepth ) & ! is.finite(O$zsd) & !is.finite(O$z) ) {
      if ( ((O$z+2*O$zsd) < setdepth | (O$z-2*O$zsd) > setdepth ) ) O$z = setdepth  # in case there are problems with depth calibration 
    }

    # final updates
    # x$chron[1] is current best guess of t0, if it is > x minutes from the 
    # set derived start time (settimestamp) then assume the above was ineffective
    O$n = nrow(x)
    # final updates
    # x$chron[1] is current best guess of t0, if it is > x minutes from the 
    # set derived start time (settimestamp) then assume the above was ineffective
    O$n = nrow(x)
    O$t0 = x$chron[1]

    #if (!is.null( settimestamp) ) {
    #  if ( abs( as.numeric( settimestamp - O$t0 )) > 2/60/24) { # two minutes away from start time is likely an error
    #    O$t0 = settimestamp
    #  } 
    #}

    O$t1 = x$chron[O$n]
    O$dt = O$t1 - O$t0

    O$dt = as.character( O$dt )
    O$t0 = as.character( O$t0 )
    O$t1 = as.character( O$t1 )
    
    O$z = mean( x$depth, na.rm=T )
    O$zsd = sd( x$depth, na.rm=T )

    if (exists( "temperature", x )) {
      O$t = mean( x$temperature, na.rm=T )
      O$tsd = sd( x$temperature, na.rm=T )
    }

    O$t0 = x$chron[1]

    #if (!is.null( settimestamp) ) {
    #  if ( abs( as.numeric( settimestamp - O$t0 )) > 2/60/24) { # two minutes away from start time is likely an error
    #    O$t0 = settimestamp
    #  } 
    #}

    O$t1 = x$chron[O$n]
    O$dt = O$t1 - O$t0

    O$dt = as.character( O$dt )
    O$t0 = as.character( O$t0 )
    O$t1 = as.character( O$t1 )
    
    O$z = mean( x$depth, na.rm=T )
    O$zsd = sd( x$depth, na.rm=T )
  
    if (exists( "temperature", x )) {
      O$t = mean( x$temperature, na.rm=T )
      O$tsd = sd( x$temperature, na.rm=T )
    }

    return( O )
  }


