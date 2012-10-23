
  # inverse projection: planar to lon/lat using proj
  planar2lonlat = function (x, proj.type, pass.direct=F) {

    km2m = 1000
    
    if (pass.direct) {
      proj4.params = proj.type
    } else {
      proj4.params = lookup.projection.params(proj.type)
    }
    y = rgdal::project( cbind(x$plon, x$plat)* km2m, proj4.params, inv=T ) 
    colnames(y) = newnames  = c("lon", "lat")
    for (i in 1:length( newnames)) {
      if ( newnames[i] %in% colnames(x) ) x[, newnames[i]] = NULL   
    }
    x = cbind(x,y)
    return (x)
  }


