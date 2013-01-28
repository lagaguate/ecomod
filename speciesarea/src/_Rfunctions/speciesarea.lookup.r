speciesarea.lookup = function( x, p, discretization.scale=0, dist.scale=5 ) {

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
   
  out = NULL
  for (Y in yrs) { 
    print( Y )
    ii = which( x$yr == Y )
    H = x[ii,]  

    SAG = sar.interpolate( p=p, yr=y, modtype=p$speciesarea.modeltype )
    if (is.null(SAG)) next ()
    
    SAG$yr = NULL
    SAG$plon = round( SAG$plon, discretization.scale )
    SAG$plat = round( SAG$plat, discretization.scale )
    
    tmp = NULL
    tmp = merge( H, SAG, by=c("plon", "plat" ), all.x=T, all.y=F, suffixes=c("", ".redundant") )
    
    if (length( grep("redundant", names( tmp ) ) > 0 )) {
      print( "Error in merge" )
      stop()
    }

    miss = which( !is.finite( tmp$z ) )
    if( length(miss) >0 ) {
      hvars = setdiff( names( tmp) , c("plon", "plat", "yr", "chron", "hid" )  )
      distances =  rdist( PS[,c("plon", "plat")], H[miss, c("plon", "plat")] )
      for( jj in 1:length(miss) ) {
        dd = which.min( distances[,jj] )
        if (distances[ dd, jj ] < dist.scale ) {
          tmp[ miss[jj], hvars ] = SAG[ dd, hvars ]
        }
      }
    }
    out = rbind( out, tmp )
    
  }
  rm (SAG, tmp) 

  out = out[, - which(names(out) %in% c("plon","plat","chron", "yr")) ] 
  x = merge( x0, out, by="hid", all.x=T, all.y=F, sort=T )
  x = x[ order(x$hid) , ]
  x$hid =NULL

  if ( nrow(x) != nx ) {
    print( "Merge error -- duplicated coords" )
    stop()
  }
  return( x )

}

