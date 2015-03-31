
bottom.contact.maxdepth = function( sm, O, bcmethods, bcp ) {
  # using previously computed estimates of touchdown (bc0) and lift-off (bc1) and associated SD's 
  # find maximum depths in the neighbourhoods
  
  res = c(NA, NA)
  names( sm) = c("Z", "timestamp", "ts" ) # Z is used to abstract the variable name such that any variable can be passed
  N = nrow(sm)
 
  tzone = lubridate::tz( sm$timestamp[1] )
  tmp = as.data.frame(O[bcmethods], stringsAsFactors=FALSE )
  tmp = as.data.frame( t(tmp), stringsAsFactors=FALSE  )
  colnames(tmp) =c("start", "end" )
  tmp[,"start"]  = as.POSIXct( tmp[,"start"], origin= "1970-01-01", tz=tzone )
  tmp[,"end"]    = as.POSIXct( tmp[,"end"], origin= "1970-01-01" , tz=tzone  )
  
  bottom0.mean = as.POSIXct( mean( tmp[ bcmethods, "start" ], na.rm=TRUE ) , origin= "1970-01-01", tz=tzone  )
  bottom1.mean = as.POSIXct( mean( tmp[ bcmethods, "end" ], na.rm=TRUE ) , origin= "1970-01-01", tz=tzone  )
  
  bottom0.sd = max( 10, sd(  ( tmp[ bcmethods, "start" ]), na.rm=TRUE ) ) # in seconds
  bottom1.sd = max( 10, sd(  ( tmp[ bcmethods, "end" ]), na.rm=TRUE ) )
  
  bc0.range = bottom0.mean + bcp$maxdepth.sd.multiplier * bottom0.sd * c(-1, 1)   
  bc1.range = bottom1.mean + bcp$maxdepth.sd.multiplier * bottom1.sd * c(-1, 1)   

  bc0 = which( sm$timestamp > bc0.range[1] & sm$timestamp < bc0.range[2] )
  bc1 = which( sm$timestamp > bc1.range[1] & sm$timestamp < bc1.range[2] )
 #browser()
  zx0 = abs( sm$Z[bc0] - max( sm$Z[bc0] , na.rm=TRUE ) )
  zx1 = abs( sm$Z[bc1] - max( sm$Z[bc1] , na.rm=TRUE ) )

  zx0i = which( zx0 < bcp$maxdepth.eps.depth )
  zx1i = which( zx1 < bcp$maxdepth.eps.depth )

  if (length( zx0i) > 0 ) {
    z0 = min ( zx0i )
  } else {
    z0 = which.max( sm$Z[bc0] )
  }
 
  if (length( zx1i) > 0 ) {
    z1 = max ( zx1i )
  } else {
    z1 = which.max( sm$Z[bc1] )
  }

  bc0i = bc0[ z0 ] # choose left-most (earliest)
  bc1i = bc1[ z1 ] # choose right most (latest)
  
  res =  c( sm$timestamp[bc0i], sm$timestamp[bc1i] )

  return(res)
}


