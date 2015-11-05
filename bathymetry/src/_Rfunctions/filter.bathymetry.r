
filter.bathymetry = function( DS, Z ) {
  #\\ filter locations based upon depths and location of spatial.domain
  
  if ( DS == "canada.east.highres" ) {
    Z = Z[ which( Z$z < 5000 & Z$z > 0) , ] 
  }
 
  if ( DS == "canada.east" ) {
    Z = Z[ which(Z$z < 1000 & Z$z > 0 ) ,] 
  }
  
  if ( DS == "SSE.mpa" ) {
    Z = Z[ which(Z$z < 2000 & Z$z > 0 ) ,] 
  }
          
  if ( DS =="SSE" ) {
    Z = Z[ which(Z$z < 800 & Z$z > 0 ) ,] 
  }
  
  if ( DS == "snowcrab" ) {
    #\\ NOTE::: snowcrab baseline == SSE baseline, except it is a subset 

    kk = which( Z$z < 350 & Z$z > 10  )
    if (length( kk) > 0) Z = Z[ kk, ]
    
    ps = spatial.parameters( type="snowcrab" )
    jj = filter.region.polygon( Z[,c(1:2)], region="cfaall", planar=T,  proj.type=ps$internal.projection ) 
    if (length( jj) > 0) Z = Z[ jj, ]
    
    # filter out area 4X   
    corners = data.frame( cbind( 
      lon = c(-63, -65.5, -56.8, -66.3 ),  
      lat = c( 44.75, 43.8, 47.5, 42.8 )  
    ) )
    corners = lonlat2planar( corners, proj.type=ps$internal.projection )
    dd1 = which( Z$plon < corners$plon[1] & Z$plat > corners$plat[1]  ) 
    if (length( dd1) > 0) Z = Z[- dd1, ]
    dd2 = which( Z$plon < corners$plon[2] & Z$plat > corners$plat[2]  ) 
    if (length( dd2) > 0) Z = Z[- dd2, ]
    dd3 = which( Z$plon > corners$plon[3] ) # east lim
    if (length( dd3) > 0) Z = Z[- dd3, ]
    dd4 = which( Z$plon < corners$plon[4] )  #west lim
    if (length( dd4) > 0) Z = Z[- dd4, ]
    dd5 = which( Z$plat > corners$plat[3]  ) # north lim
    if (length( dd5) > 0) Z = Z[- dd5, ]
    dd6 = which( Z$plat < corners$plat[4]  )  #south lim 
    if (length( dd6) > 0) Z = Z[- dd6, ]
  }
  return(Z)
}

