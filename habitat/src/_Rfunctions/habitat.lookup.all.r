habitat.lookup.all = function(x, p, discretization.scale, dist.scale) {

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
  }  

  if (is.null ( yrs) ) stop()
  
  if (! exists("weekno", x) ) {
    x$dayno = convert.datecodes(x$chron, "julian")  # from  source ("/home/jae/ecomod/common/functions.date.r")
    x$weekno = ceiling (x$dayno / 365 * 52 )
  }
  # weeknos = sort( unique( x$weekno ) )

  out = NULL

  for (Y in yrs) { 
    print( Y )
    ii = which( x$yr == Y )
    if (length( ii) == 0) next()  
    H = x[ii,]  

    PS = habitat.db( DS="complete", p=p, year=Y )
    if (is.null(PS)) next ()
    PS$yr = NULL
    PS$plon = round( PS$plon, discretization.scale )
    PS$plat = round( PS$plat, discretization.scale )
    
    tmp = NULL
    tmp = merge( H, PS, by=c("plon", "plat" ), all.x=T, all.y=F, suffixes=c("", ".redundant") )
    
    oo =  grep("redundant", names( tmp)) 
    if (length(oo) > 0 ) {
      tmp = tmp[, -oo ] 
    }

    miss = which( !is.finite( tmp$z ) )
    if( length(miss) >0 ) {
      hvars = setdiff( names( tmp) , names(H) )
      distances =  rdist( PS[,c("plon", "plat")], H[miss, c("plon", "plat")] )
      for( jj in 1:length(miss) ) {
        dd = which.min( distances[,jj] )
        if (distances[ dd, jj ] < dist.scale ) {
          tmp[ miss[jj], hvars ] = PS[dd, hvars ]
        }
      }
    }	
    out = rbind( out, tmp )
    
  }
  out = merge( x0, out, by="hid", all.x=T, all.y=F, sort=T ) 
  out = out[ order(out$hid) , ]
  out$hid = NULL

  if ( nrow(out) != nx ) {
    print( "Merge error -- duplicated coords" )
    stop()
  }
  return( out )

}
