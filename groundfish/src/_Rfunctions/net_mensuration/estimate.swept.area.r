
estimate.swept.area = function( gsi=NULL, x=NULL, getnames=FALSE, threshold.cv=10  ){

  if (getnames) return( c("sweptarea.mean", "depth.mean", "depth.sd", "wingspread.mean", "wingspread.sd" ) )
  
  gsi$sweptarea.mean = NA
  gsi$depth.mean = NA
  gsi$depth.sd = NA
  gsi$wingspread.mean = NA
  gsi$wingspread.sd = NA

  # debug
  if (FALSE){
    gsi = gs[gii,]
    x= nm[ii,]
  }
  
  x = x[order( x$timestamp ) ,]
  bc = which( x$timestamp >=gsi$bc0.datetime & x$timestamp <= gsi$bc1.datetime ) 
  x = x[bc,]
  
  ##--------------------------------
  # timestamps have frequencies higher than 1 sec .. duplciates are created and this can pose a problem
  x$ts = difftime( x$timestamp, min(x$timestamp), units="secs" )
  x$lon.sm = NA  # interpolated locations
  x$lat.sm = NA  # interpolated locations
  x$time.increment = NA
  ndat = nrow(x)
  
  if ( FALSE ) {
    plot (latitude~longitude, data=x, pch=20, cex=.1)
    plot (depth~timestamp, data=x, pch=20, cex=.1)
    plot (depth~ts, data=x, pch=20, cex=.1)
    plot (wingspread~ts, data=x, pch=20, cex=.1)
    plot (doorspread~ts, data=x, pch=20, cex=.1)
    plot (doorspread~ts, data=x[x$door.and.wing.reliable,], pch=20, cex=.2, col="green") 
  }

  mean.velocity.m.per.sec = gsi$speed * 1.852  * 1000 / 3600
  x$distance = as.numeric( x$ts * mean.velocity.m.per.sec )
  
  npos = sqrt( length( unique( x$longitude)) ^2  + length(unique(x$latitude))^2)
 
  # ------------
  # clean up distance /track

  x$distance.sm = NA
  if (npos > 30) { 
    # interpolated.using.velocity" .. for older data with poor GPS resolution
    # use ship velocity and distance of tow estimated on board to compute incremental distance, assuming a straight line tow
    nn = abs( diff( x$ts ) )
    dd = median( nn[nn>0], na.rm=TRUE )
    x$t = jitter( x$t, amount=dd / 20) # add noise as inla seems unhappy with duplicates in x?
    uu = smooth.spline( x=x$ts, y=x$longitude, keep.data=FALSE) 
    x$lon.sm = uu$y
    vv = smooth.spline( x=x$ts, y=x$latitude, keep.data=FALSE) 
    x$lat.sm = vv$y
    pos = c("lon.sm", "lat.sm")
    dh =  rep(0, ndat-1)
    for( j in 1:(ndat-1) ) dh[j] = geodist( point=x[j,pos], locations=x[j+1,pos], method="vincenty" ) * 1000 # m .. slower but high res
    # dh = zapsmall( dh, 1e-9 )
    x$distance.sm = c( 0, cumsum( dh ) )
  } 

  
  # ------------
  # clean up doorspread 
  
  doorspread.median = median(x$doorspread, na.rm=T)
  doorspread.sd = sd(x$doorspread, na.rm=T)
  
  threshold.cv = 0.5 ## ?? good value ??

  x$tsv = as.numeric( x$ts )
  ii = interpolate.xy.robust( xy=x[,c("tsv", "doorspread" )], method="loess"  ) 
  x$doorspread.sm = 
  

  if ( doorspread.sd / doorspread.median > threshold.cv ) {
      SA.door = doorspread.median * max( x$distance.sm, na.rm=TRUE )
  } else {
    # piece-wise integration here.
    len.dist = diff ( x$distance.sm ) 
    
    
    len.door = 
    artial.area =  delta.distance * mean.doorspreads
    #out$surfacearea = sum( partial.area )  # km^2
    #out$surfacearea = abs(  out$surfacearea )
    
  }
  
  
  # ------------
  # wingspread .. repeat as above
  
  wingspread.median = median(x$wingspread, na.rm=T)
  wingspread.sd = sd(x$wingspread, na.rm=T)
  
  if ( wingspread.sd / wingspread.median > threshold.cv ) {
    if (all( !is.finite(x$distance.sm)) ) {
      SA.door = wingspread.median * max( x$distance.sm, na.rm=TRUE )
    } else {
      SA.door = wingspread.median * max( x$distance, na.rm=TRUE )
    }
  } else {
    # piece-wise integration here.
    #partial.area =  delta.distance * mean.doorspreads
    #out$surfacearea = sum( partial.area )  # km^2
    #out$surfacearea = abs(  out$surfacearea )
    
  }
  
  
  
  
  return( gsi)

}


