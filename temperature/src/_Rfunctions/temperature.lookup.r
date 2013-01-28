
temperature.lookup = function( x, p,  discretization.scale = 0, dist.scale=5 ) {

  require(fields)
  # x must contain plon, plat, and chron
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
  
  if (exists( "yr",x ) )  yrs = sort( unique( x$yr ))
  if (any( grepl( "chron", names(x) ) )) {
    require(chron)
    x$yr = as.numeric( as.character( years( x$chron )))
    yrs = sort( unique( x$yr ))
    if (is.null ( yrs) ) stop()
  }  

  if (! exists("weekno", x) ) {
    x$dayno = convert.datecodes(x$chron, "julian")  # from  source ("/home/jae/ecomod/common/functions.date.r")
    x$weekno = ceiling (x$dayno / 365 * 52 )
  }
  weeknos = sort( unique( x$weekno ) )

  O = bathymetry.db( p=p, DS="baseline" )
  O$plon = round( O$plon, discretization.scale )
  O$plat = round( O$plat, discretization.scale )
  O$z = NULL
  O$row = 1:nrow(O)
 
  out = NULL

  for (Y in yrs) {
    print( Y )
  
    PS = temperature.interpolations( p=p, DS="spatial.interpolation", yr=Y  )
    if (is.null(PS)) next ()
    
    PS[ PS < -2 ] = -2
    PS[ PS > 30 ] = 30 
  
    ii = which( x$yr == Y )
    if ( length(ii) == 0 ) next()

    H = x[ii,]  

    tmp = NULL
    tmp = merge( H, O, by=c("plon", "plat" ), all.x=T, all.y=F, sort=F )
    
    hij = matrix( NA, ncol=2, nrow=length(ii) )	
    hij[,1] = tmp$row
    hij[,2] = tmp$weekno

    tmp$t = as.numeric( PS[ hij ] )

    miss = which( !is.finite( tmp$t ) )
    if( length(miss) >0 ) {
      distances =  rdist( O[,c("plon", "plat")], H[miss, c("plon", "plat")] )
      for( jj in 1:length(miss) ) {
        dd = which.min( distances[,jj] )
        if (distances[ dd, jj ] < dist.scale ) {
          tmp$t[ miss[jj] ] = PS[dd, tmp$weekno[miss[jj]] ]
        }
      }
    }
    out = rbind( out, tmp )
  } # end for years

  if ( is.null( out) )  return( x$t )  # nothing to do, return itself
  rm (x)

  res = merge( x0, out[ , c("hid", "t" )], by="hid", all.x=T, all.y=F, sort=T )
  res = res[ order(res$hid) , ]
  res$hid = NULL

  if ( nrow(res) != nx ) {
    print( "Merge error -- duplicated coords" )
    stop()
  }
  return( res$t )
}

