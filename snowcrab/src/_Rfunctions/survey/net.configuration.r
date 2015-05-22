
  net.configuration = function( N, t0=NULL, t1=NULL, tchron=NULL ) {
    
    tzone = "America/Halifax"  ## need to verify if this is correct

    # N is netmind data 
    # t0 is current best estimate of start and end time 
    # tchron is .. ?
    if(length(t0)>1) {t0 = NULL}
    # create default output should the following fail
    out = data.frame( slon=NA, slat=NA, distance=NA, spread=NA, spread_sd=NA, 
      surfacearea=NA, vel=NA, vel_sd=NA, netmind_n=NA, t0=NA, t1=NA, dt=NA, yr=NA )
    
    n.req = 30
    t0_multiple = NULL
    #changed this switch from depgth filed to lat as if there is not depth info can still run script
	  if ( length( which( is.finite( N$lat))) < n.req ) print(N[1,])
    
    if(is.na(t0)) t0 = NULL
    if(is.na(t1)) t1 = NULL
          
    problem = F

    # time checks
    if ( is.null(t0) & !is.null(tchron) ) {
      t0 = tchron
      tchron =NULL
    }

    if (!is.null(t0) & !is.null(tchron)  ) {
     t0_multiple = t0 = c( as.POSIXct(tchron, tz=tzone), t0 )
      tchron =NULL
    }
  
    if(N$netmind_uid[1] =='netmind.S26092014.9.541.17.48.304') return(out)
 
    if ( any( is.null( t1 ) || is.null(t0) ) )   {
      # try to determine from netmind data if minilog/seadbird data methods have failed. .. not effective due to noise/and small data stream 
     
      N$timestamp =  as.POSIXct( N$chron , tz=tzone )
      M = N[, c("timestamp", "depth") ]
      
      if(!is.null(tchron)) settimestamp= as.POSIXct( tchron , tz=tzone )
      if(is.null(tchron)) settimestamp= as.POSIXct( t0 , tz=tzone )
      time.gate =  list( t0=settimestamp - dminutes(5), t1=settimestamp + dminutes(9) )
      
      bcp = list( 
        id=N$netmind_uid[1], datasource="snowcrab", nr=nrow(M), YR=yr,
        tdif.min=3, tdif.max=9, time.gate=time.gate,
        setdepth=rid$setZx[i], depth.min=20, depth.range=c(-20,30), depthproportion=0.6 
      )
      
      bcp = bottom.contact.parameters( bcp ) # add other default parameters
   

      bc = NULL
      bc = bottom.contact( x=M, bcp=bcp )
        
      if (FALSE) {
        # to visualize/debug
        bottom.contact.plot( bc) 
      }
      
      if (is.null(t0) & !is.null(bc$bottom0) ) t0 = bc$bottom0
      if (is.null(t1) & !is.null(bc$bottom1) ) t1 = bc$bottom1
      N = N[ bc$bottom.contact , ] 
    }

    if (all(is.na(t0))) t0=NULL
    if (all(is.na(t1))) t1=NULL

    if ( is.null(t1) ) {
      t1_tmp = t0 + 5 /50/24
    } else {
      t1_tmp = t1
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
      #problem =T turned this off December 23, 2013 as if we have decent gps data now we can calc distance
      if ( length( gooddoor) >0 ) {
      	out$netmind_n <- length(gooddoor)
        out$spread = mean( N$doorspread[ gooddoor ], na.rm=T ) / 1000
        out$spread_sd = sd( N$doorspread[ gooddoor ], na.rm=T ) / 1000
      }
    }

    if (!is.null( t1) )    t1 = as.POSIXct(t1,origin='1970-01-01', tz=tzone)
    
    if (!is.null( t0_multiple )) {
        if( !any(is.na(t0_multiple) )) { # two estimates of t0
      timediff = abs( as.numeric( t0_multiple[1] - t0_multiple[2] ))
      
      if (timediff > 20 ) { # more than XX seconds
        # check bounds
        if (!is.null( t1) ) {
   
          dts =  abs( as.numeric( t1 - t0_multiple ))
          if(all(is.chron(dts))) igood = which( dts > 5/60/24 &  dts < 7/60/24 )
          if(any(!is.chron(dts))) igood = which( dts > 300 &  dts < 420 )
          if(any(dts<12)) igood = which( dts > 5 &  dts < 7 )
          
          if (length(igood)==0 ) t0=mean(t0_multiple) # both not good, take mean
          if (length(igood)==1 ) t0=t0_multiple[igood]
          if (length(igood)==2 ) t0=min( t0_multiple )  # both good, no info which is best .. take the longer tow to be conservative
        }
      } else {
        t0 = min( t0_multiple ) # this is to be more conservative in SA estimates ... better to be wrong by estimating too large a SA
      }
    }
  }
if(length(t0)>1) t0 = NA
    out$t0 = t0
    out$t1 = t1

    if ( is.null( t1) ) {
      out$dt = 5/60/24
      out$t1 = t1 = out$t0 + out$dt
    }

    out$dt = t1 - t0
if(!is.na(out$t0))    out$yr = as.numeric( as.character( years( out$t0) ))
if(is.na(out$t0))    out$yr = as.numeric( as.character( years(N$chron[1]) ))

    if(is.chron(t0)) itime =  which( N$chron >= t0  &  N$chron <= t1 )
    if(is.POSIXct(t0)) itime =  which( as.POSIXct(N$chron, tz=tzone) >= t0  &  as.POSIXct(N$chron, tz=tzone) <= t1 )
    
    if ( length( itime) < n.req ) problem = T

    if (problem) {
      out$t0 = t0
      out$t1 = NA
      out$dt = NA
      return(out) 
    }

    # eOR checks

      N = N[ itime, ]
if(nrow(N)>1) {
  
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
      
	      out$vel = mean(n$speed, na.rm=T, trim=0.1)
	      out$vel_sd = sd(n$speed, na.rm=T)
	      out$slon=n$lon[1]
	      out$slat=n$lat[1]
	      out$netmind_n=end

      delta.distance = NULL
	   n$distance = NA
     if(nrow(n)>10) {	
      # integrate area:: piece-wise integration is used as there is curvature of the fishing track (just in case)
       
      delta.distance = geosphere::distMeeus ( nms[ 1:(end-1), pos ], nms[ 2:end, pos ] ) / 1000 ## in meters convert to km 
      n$distances = c( 0, cumsum( delta.distance  ) ) # cumsum used to do piecewise integration of distance
	
      # model/smooth/interpolate the spreads
      n$doorspread.predicted = NA
      ii = which( is.finite( n$doorspread ) )
      if ( length(ii) > n.req ) {
           
         n$doorspread.predicted = approx( x=n$distances, y=n$doorspread, xout=n$distances, method="linear", rule=2 )$y
         
         
       #turned off gam model in December 20, 2013 giving unrealistic values for spread as the new esnoar files have 0 and NA whereas older netmind are filled with previous value 
			      	#gam.model = try( gam( doorspread ~ s(distances, k=5, bs="ts"), data=n[ii,], optimizer=c("outer", "nlm")), silent = T )
			        #if ( ! "try-error" %in% class( gam.model )) {
			        #  n$doorspread.predicted = predict( gam.model, newdata=n, newdata.guaranteed=T )
			        #} else {
			        #  n$doorspread.predicted = approx( x=n$distances, y=n$doorspread, xout=n$distances, method="linear", rule=2 )$y
			        #}
      }
      if ( length( which( is.finite( n$doorspread.predicted ) ) ) < 10 ) {
        n$doorspread.predicted = mean( n$doorspread , na.rm=T, trim=0.1 )
      }

      mean.doorspreads = ( n$doorspread.predicted[1:(end-1)] + n$doorspread.predicted[2:(end)] ) / 2 / 1000  # mean between two ends
      partial.area =  delta.distance * mean.doorspreads
      out$surfacearea = sum( partial.area )  # km^2
      out$surfacearea = abs(  out$surfacearea )

      out$spread = mean(n$doorspread.predicted, na.rm=T, trim=0.1)/1000  # in km
     spread_sd = sd(n$doorspread.predicted, na.rm=T )/1000
     if(!is.na(spread_sd) & spread_sd!=0) out$spread_sd = spread_sd #if just using the mean from above do not over write spread_sd
     out$distance=n$distances[end]
   }
	}
    return (out)
  }


