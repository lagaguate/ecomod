
substrate.lookup = function( x, p, discretization.scale=0, dist.scale=5 ) {

  require(fields)
  
  nx = nrow(x)
	x$hid = 1:nx
  x0 = data.frame( hid = x$hid )

  if ( !any(grepl( "plon", names(x) ) )) {
    x = lonlat2planar (x, proj.type=p$internal.projection ) 
  }
  if ( p$spatial.domain == "snowcrab" ) p$spatial.domain = "SSE"
	x$plon = round( x$plon, discretization.scale )
	x$plat = round( x$plat, discretization.scale )
  x = x[ which(is.finite( x$plon+x$plat ) ) ,]  # don't worry these will be merged back into "x0" (above)
 
  S =  substrate.db ( p=p, DS="planar")
  S$substrate.mean = log(S$grainsize)
  S$grainsize = NULL

  S$plat = round( S$plat, discretization.scale )
  S$plon = round( S$plon, discretization.scale )

  x = merge( x, S, by=c("plon", "plat" ), all.x=T, all.y=F, sort=FALSE )

  miss = which( !is.finite( x$substrate.mean ) )
  if( length(miss) >0 ) {
    distances =  rdist( S[,c("plon", "plat")], x[miss, c("plon", "plat")] )
    for( jj in 1:length(miss) ) {
      dd = which.min( distances[,jj] )
      if (distances[ dd, jj ] < dist.scale  ) {
        x$substrate.mean[ miss[jj] ] = S$substrate.mean[dd]
      }
    }
  }
	
  x = merge( x0, x[,c("hid","substrate.mean")], by="hid", all.x=T, all.y=F, sort=T )
  x = x[ order(x$hid) , ]

  if ( nrow( x ) != nx ) {
    print( "Merge error -- duplicated coords" )
    stop()
  }
  return( x$substrate.mean )

}



