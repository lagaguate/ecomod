
  net.configuration = function( N, t0=NULL, t1=NULL, tchron=NULL ) {

    require(mgcv)

    n.req = 30

    N$tinc = 1:nrow(N)

    # create default output should the following fail
    out = data.frame(slon=0)
    out$slon=NA
    out$slat=NA
    out$distance=NA
    out$spread=NA
    out$spread_sd=NA
    out$surfacearea=NA
    out$vel=NA
    out$vel_sd=NA
    out$netmind_n=NA
    out$t0=NA
    out$t1=NA
    out$dt=NA
    out$yr=NA

    if ( length( which( is.finite( N$depth))) < n.req ) return(out)
    
    problem = F

    # time checks
    if ( is.null(t0) & !is.null(tchron) ) {
      t0 = tchron
      tchron =NULL
    }

    if (!is.null(t0) & !is.null(tchron) ) {
      t0 = c( tchron, t0 )
      tchron =NULL
    }

    if ( any( is.null( c(t0, t1 ) ) ) ) {
      # try to determine from minilog methods
      m = N[, c("chron", "depth") ]
      b = bottom.contact( x=m, settimestamp=tchron )

      if (is.null(t0) & !is.null(b$t0) ) t0 = string2chron(b$t0)
      if (is.null(t1) & !is.null(b$t1) ) t1 = string2chron(b$t1)

      if (is.na(t0)) t0=NULL
      if (is.na(t1)) t1=NULL
    }

    t0_multiple = NULL
    if ( length(t0)==0 | is.null( t0) ) {
      # try to estimate directly
      gdepth= try( gam( depth~s(tinc, k=5, bs="ts"), data=N ), silent=T )
      if ( "try-error" %in% class( gdepth) ) {
        # remove the very largest in case of noise/errors
        qz = which( N$depth < quantile( N$depth, 0.95, na.rm=T ) )
        maxz = which.max( N$depth[qz] )
        bottom = qz[maxz]
      } else {
        N$depthsmoothed = predict( gdepth, newdata=N )
        bottom = which.max(N$depthsmoothed)
      }
      N = N[ which( N$depth > ( N$depth[bottom] - 15 ) ) ,]  # drop anything greater than 15 m from deepest point
    } else if (length(t0)==1) {
      it0 = which( (N$chron >= t0) )
      if (length(it0)>0) N = N[it0,]
    } else {
      # two to choose from: take the minimum for now
      t0_multiple = t0
      itmin = which( N$chron >=  min( t0, na.rm=T) )
      if (length (itmin)>0) N = N[ itmin,]
    }

    t0 = N$chron[1]  # reset for now

    if ( is.null(t1) ) {
      t1_tmp = t0 + 5 /50/24
    } else {
      t1_tmp = t1
    }

    it1 = which( N$chron <= t1_tmp )
    if (length(it1)>0) N = N[it1,]

    if (length(it1) < n.req)  {
      out$t0 = t0
      # only return t0 estimate to force a more global solution in cleaning step
      out$t1 = NA
      out$dt = NA
      return( out )
    }


    # if we are here, it is because a reasonable amount of data is present ..
    # do some more checks and get a first estimate of some parameters in case other errors/limits are found
    out$slon=N$lon[1]
    out$slat=N$lat[1]
    out$spread=mean( N$doorspread, na.rm=T ) / 1000
    out$spread_sd=sd( N$doorspread, na.rm=T ) /1000


    # first set the impossible door spreads to NA and then interpolate based upon a 5 and 95 % quantiles
    # Netmind has very noisy data
    hl = c( 3, 16 ) # m
    ihl = which( N$doorspread < hl[1] | N$doorspread > hl[2] )
    if (length (ihl)>0 ) N$doorspread[ihl] = NA
    quantiles.to.trim = c(0.05, 0.95)
    qnt =  quantile( N$doorspread, quantiles.to.trim, na.rm=T)
    iqnt = which( N$doorspread < qnt[1] | N$doorspread > qnt[2] )
    N$doorspread[ iqnt ] = NA
    gooddoor =  which(   is.finite(N$doorspread) )
    baddoor =   which( ! is.finite(N$doorspread) )

    if (length(baddoor)>0) N$doorspread[ baddoor ] = NA
    if ( length( gooddoor) < n.req ) {
      problem =T
      if ( length( gooddoor) >0 ) {
        out$spread = mean( N$doorspread[ gooddoor ], na.rm=T ) / 1000
        out$spread_sd = sd( N$doorspread[ gooddoor ], na.rm=T ) / 1000
      }
    }

    if (!is.null( t0_multiple ) ) { # two estimates of t0
      timediff = abs( as.numeric( t0_multiple[1] - t0_multiple[2] ))
      if (timediff > 20/60/60/24 ) { # more than XX seconds
        # check bounds
        if (!is.null( t1) ) {
          dts =  abs( as.numeric( t1 - t0_multiple ))
          igood = which( dts > 5/60/24 &  dts < 7/60/24 )
          if (length(igood)==0 ) t0=mean(t0_multiple) # both not good, take mean
          if (length(igood)==1 ) t0=t0_multiple[igood]
          if (length(igood)==2 ) t0=min( t0_multiple )  # both good, no info which is best .. take the longer tow to be conservative
        }
      } else {
        t0 = min( t0_multiple ) # this is to be more conservative in SA estimates ... better to be wrong by estimating too large a SA
      }
    }

    out$t0 = t0
    out$t1 = t1

    if ( is.null( t1) ) {
      out$dt = 5/60/24
      out$t1 = t1 = out$t0 + out$dt
    }

    out$dt = t1 - t0
    out$yr = as.numeric( as.character( years( out$t0) ))

    itime =  which( N$chron >= t0  &  N$chron <= t1 )
    if ( length( itime) < n.req ) problem = T

    if (problem) {
      out$t0 = t0
      out$t1 = NA
      out$dt = NA
      return(out)
    }

    # end ERROR checks


      N = N[ itime, ]

      # t1 gives the approximate time of net lift-off from bottom
      # this is not good enough as there is a potential backdrift period before net lift off
      # this means distance trawled calculations must use the geo-positioning of the boat
      # at maximum tension and not the position at net movemnent up
      # (otherwise, this would introduce a net bias towards smaller swept areas).
      pos = c( "lon", "lat" )

      distance.from.start = geodist(point=N[1, pos], locations=N[, pos], method="great.circle") #  in km^2
      end = which.max( distance.from.start)
      if( !is.finite(end) ) end=nrow(N)
      n = N[ 1:end , ]

      # integrate area:: piece-wise integration is used as there is curvature of the fishing track (just in case)
      delta.distance = NULL
      for( ii in 1:(end-1) ) {
        gd = geodist( point=n[ ii, pos], locations=n[ ii+1, pos], method="vincenty" ) # km
        delta.distance = c( delta.distance, gd  )
      }
      n$distances = c( 0, cumsum( delta.distance  ) ) # cumsum used to do piecewise integration of distance

      # model/smooth/interpolate the spreads
      n$doorspread.predicted = NA
      ii = which( is.finite( n$doorspread ) )
      if ( length(ii) > 30 ) {
        gam.model = try( gam( doorspread ~ s(distances, k=5, bs="ts"), data=n[ii,], optimizer=c("outer", "nlm")), silent = T )
        if ( ! "try-error" %in% class( gam.model )) {
          n$doorspread.predicted = predict( gam.model, newdata=n, newdata.guaranteed=T )
        } else {
          n$doorspread.predicted = approx( x=n$distances, y=n$doorspread, xout=n$distances, method="linear", rule=2 )$y
        }
      }
      if ( length( which( is.finite( n$doorspread.predicted ) ) ) < 10 ) {
        n$doorspread.predicted = mean( n$doorspread , na.rm=T, trim=0.1 )
      }

      mean.doorspreads = ( n$doorspread.predicted[1:(end-1)] + n$doorspread.predicted[2:(end)] ) / 2 / 1000  # mean between two ends
      partial.area =  delta.distance * mean.doorspreads
      out$surfacearea = sum( partial.area )  # km^2
      out$surfacearea = abs(  out$surfacearea )

      out$spread = mean(n$doorspread.predicted, na.rm=T, trim=0.1)/1000  # in km
      out$spread_sd = sd(n$doorspread.predicted, na.rm=T )/1000
      out$vel = mean(n$speed, na.rm=T, trim=0.1)
      out$vel_sd = sd(n$speed, na.rm=T)

      out$slon=n$lon[1]
      out$slat=n$lat[1]
      out$distance=n$distances[end]
      out$netmind_n=end


    return (out)
  }


