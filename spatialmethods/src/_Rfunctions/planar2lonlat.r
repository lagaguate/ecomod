
  # inverse projection: planar to lon/lat using proj
  planar2lonlat = function (x, proj.type, planar.coord.scale=1000, newnames = c("lon", "lat")  ) {
    # convert planar coord systems which need to be expressed in meters to lon-lat
    # planar.coord.scale is the multiplier applied upon the planar coords
    # when planar.coord.scale=1000 .. it is converting incoming data (km) to m 
    # when planar.coord.scale=1 .. it means no conversion as it is already in m
    x$plon = x$plon * planar.coord.scale
    x$plat = x$plat * planar.coord.scale  
   

  # first try an internal conversion /lookup for CRS  
    proj4.params = try( sp::CRS( lookup.projection.params(proj.type) ), silent=TRUE )
    
    # if internal lookup does not work then try to directly pass to CRS   
    if ( "try-error" %in% class( proj4.params) ) proj4.params = try( sp::CRS( proj.type ), silent=TRUE )
    if ( "try-error" %in% class( proj4.params) ) {
      print( proj.type )
      warning( "Projection not recognised") 
    }

    y = rgdal::project( cbind(x$plon, x$plat), proj4.params@projargs, inv=T ) 
    
    colnames(y) = newnames  
    for (i in 1:length( newnames)) {
      if ( newnames[i] %in% colnames(x) ) x[, newnames[i]] = NULL   
    }
    x = cbind(x,y)
    return (x)
  }


