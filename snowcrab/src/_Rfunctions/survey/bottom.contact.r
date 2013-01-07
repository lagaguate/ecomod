  bottom.contact = function(x, settimestamp=NULL, setdepth=NULL ) {
        
    require(mgcv)
    n.req = 30 
    
    x$chron = string2chron( x$chron )
    x$tinc = c(1:nrow(x))
    
    O = data.frame( cbind(z=NA, t=NA, zsd=NA, tsd=NA, n=NA, t0=NA, t1=NA, dt=NA ) ) 
    
    if (length( which(is.finite(x$depth) ) )< n.req) return(O)  # no data 
    
    if (length(setdepth)==0) setdepth = NULL
    if (length(settimestamp)==0) settimestamp = NULL

    if (!is.null( settimestamp) ) {
      O$t0=settimestamp
      settimestamp = string2chron( settimestamp )  
    }
   
    if (!is.null( setdepth) )  O$z=setdepth

    problem =F
    if (nrow(x) < n.req) problem=T

    # select based upon time stamps
    if ( !is.null( settimestamp ) ) {
      # 5 min prior to settimestamp some data seem to have desynchronized times (drift?)
      # AND 9 MINUTES AFTER settimestamp (some are really this long)
      timelimits = settimestamp + c(-5, 9) / 24 / 60  # to ensure tails are well defined
      jj = which( x$chron > timelimits[1] & x$chron < timelimits[2] ) 
      if ( length(jj) > n.req ) {
        r = range( jj ) 
        x = x[ r[1] : r[2], ]
      }
    }

    # make depth profiles symmetrical (removing points where depths are too shallow on the right tail
                                     #     oo = c(1:trunc(nrow(x)/2))
                                     #     zminleft = min( x$depth[oo], na.rm=T)
                                     #     jj =  which( x$depth > zminleft)
                                     #     if ( length(jj) > n.req ) {
                                     #       r = range( jj ) 
                                     #       x = x[ r[1] : r[2], ]
                                     #     }
                                     #      
                                     #   do same for right side
                                     #     oo = c(trunc(nrow(x)/2):nrow(x))
                                     #     zminright = min( x$depth[oo], na.rm=T)
                                     #     jj =  which( x$depth > zminright)
                                     #     if ( length(jj) > n.req ) {
                                     #       r = range( jj ) 
                                     #       x = x[ r[1] : r[2], ]
                                     #     }
   
    # depths examined to identify the deepest point. 
    # As there are occasionally strong extremes from erroneous pings, 
    # a modelled (smoothed prediction is used to determine deepest point.

    gdepth= try( gam( depth~s(tinc, k=4, bs="ts"), data=x ), silent=T )
    if ( "try-error" %in% class( gdepth) ) {
     # remove the very largest in case of noise/errors
      qz = which( x$depth < quantile( x$depth, 0.975, na.rm=T ) )
      maxz = which.max( x$depth[qz] )
      if (length(maxz)>n.req) bottom = qz[maxz]  
    } else {
      x$depthsmoothed = predict( gdepth, newdata=x )
      bottom = which.max(x$depthsmoothed)
    }
    
    O$z = x$depth[bottom]

    # drop data too far from the deepest point 
    Cx = x$chron[bottom]
    dropmin = 10  # do not touch  .. some are really quite long as they can be asymmetrical profiles
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
    
    # use deviations from a linear trend in the central portion to define contact
    if (is.null(settimestamp)) {
      i0 = trunc( 1/3 * nrow(x) ) 
      i1 = trunc( 2/3 * nrow(x) )
    } else {
      i0 = which.min( abs(x$chron - (settimestamp + 0.5/24/60))) # 1/2 minute after expected start time
      i1 = which.min( abs(x$chron - (settimestamp + 4.5/24/60))) # 1/2 minute before expected end time 
    }

    zlm = try( lm( depth ~ tinc, x[ i0:i1, ] ) ) 
    if ("try-error" %in% class( zlm)) {
      x$zresid = x$depth
    } else {
      x$zresid = x$depth - predict( zlm, newdata=x ) 
    } 
    
    target.sd = max( 0.1, sd (x$zresid[i0:i1], na.rm=T ), na.rm=T ) 
    
    
    # final filter .. if magnitude of tails is large remove
    # if the increment from the linear trend is > 10*sd thn remove
    jj = which( abs(x$zresid) < 10*target.sd) 
    if ( length(jj) > n.req ) {
      r = range( jj ) 
      x = x[ r[1] : r[2], ]
    }
    
    # start from left and contuniue until sd of residuals begins to deviate sustantially
    nx = nrow(x)
    nx2 = trunc( nx / 2 )
    # start time .. being from centre to left 
    for ( j0 in nx2:1 ) if ( sd(( x$zresid[j0:(nx2+5)]), na.rm=T) > 2*target.sd )  break()
    
    # end time .. begin from centre to right 
    for ( j1 in nx2:nx ) if ( sd(( x$zresid[(nx2-5):j1]), na.rm=T) > 2*target.sd ) break()
    
    if ( (j1-j0) > n.req)  x = x[ j0:j1, ]


    if ( !is.null( setdepth ) & ! is.finite(O$zsd) & !is.finite(O$z) ) {
      if ( ((O$z+2*O$zsd) < setdepth | (O$z-2*O$zsd) > setdepth ) ) O$z = setdepth  
      # in case there are problems with depth calibration 
    }

    # final updates
    O$n = nrow(x)
    O$n = nrow(x)
    O$t0 = x$chron[1]

    O$t1 = x$chron[O$n]
    O$dt = O$t1 - O$t0

 #   O$dt = as.character( O$dt )
 #   O$t0 = as.character( O$t0 )
 #   O$t1 = as.character( O$t1 )
    
    O$z = mean( x$depth, na.rm=T )
    O$zsd = sd( x$depth, na.rm=T )

    if (exists( "temperature", x )) {
      O$t = mean( x$temperature, na.rm=T )
      O$tsd = sd( x$temperature, na.rm=T )
    }

    return( O )
  }


