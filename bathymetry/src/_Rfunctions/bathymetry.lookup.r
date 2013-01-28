bathymetry.lookup = function( x, p, discretization.scale=0, dist.scale=5 ) {
	
  require(fields)  # rdist
 
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
    
  O = bathymetry.db( p=p, DS="baseline" )
  O$plon = round( O$plon, discretization.scale )
  O$plat = round( O$plat, discretization.scale )

  x = merge( x, O, by=c("plon", "plat" ), all.x=T, all.y=F, suffixes=c("",".bathymetry") )

  miss = which( !is.finite( x$z ) )
  if( length(miss) >0 ) {
    if ( exists( "z.bathymetry", x ) ) {
      x$z[miss] = x$z.bathymetry[miss]
      x$z.bathymetry = NULL
    }
    miss = which( !is.finite( x$z ) )
    if ( length(miss) >0 ) {
      distances =  rdist( O[,c("plon", "plat")], x[miss, c("plon", "plat")] )
      for( jj in 1:length(miss) ) {
        dd = which.min( distances[,jj] )
        if (distances[ dd, jj ] < dist.scale  ) {
          x$z[ miss[jj] ] = O$z[dd]
        }
      }
    }
  }
	
  x = merge( x0, x[, c("hid", "z")], by="hid", all.x=T, all.y=F, sort=T )
  x = x[ order(x$hid) , ]

  if ( nrow( x ) != nx ) {
    print( "Merge error -- duplicated coords" )
    stop()
  }

  return ( x$z )

}

