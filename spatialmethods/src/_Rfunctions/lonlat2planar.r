
  lonlat2planar = function ( x, proj.type, ndigits=2, newnames = c("plon", "plat") ) {
    # convert lon/lat to a projected surface using proj
    # proj.type can be an internal code such as "utm20" or a proj4 argument
    # output is in km

    m2km = 1/1000
    
    # first try an internal conversion /lookup for CRS  
    proj4.params = try( sp::CRS( lookup.projection.params(proj.type) ), silent=TRUE )
    
    # if internal lookup does not work then try to directly pass to CRS   
    if ( "try-error" %in% class( proj4.params) ) proj4.params = try( sp::CRS( proj.type ), silent=TRUE )
    if ( "try-error" %in% class( proj4.params) ) {
      print( proj.type )
      warning( "Projection not recognised") 
    }

    y = rgdal::project( cbind(x$lon, x$lat), proj4.params@projargs, inv=F ) * m2km
    
    y = round(y, ndigits )
    colnames(y) = newnames 
    for (i in 1:length( newnames)) {
      if ( newnames[i] %in% colnames(x) ) x[, newnames[i]] = NULL   
    }
    x = cbind(x,y)
    return (x)
  }


