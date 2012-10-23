
  # convert lon/lat to a projected surface using proj
  lonlat2planar = function (x, proj.type, pass.direct=F, ndigits=2) {
   
    m2km = 1/1000
    if (pass.direct) {
      proj4.params = proj.type
    } else {
      proj4.params = lookup.projection.params(proj.type)
    }
    y = rgdal::project( cbind(x$lon, x$lat), proj4.params, inv=F ) * m2km
    y = round(y, ndigits )
    colnames(y) = newnames = c("plon", "plat")
    for (i in 1:length( newnames)) {
      if ( newnames[i] %in% colnames(x) ) x[, newnames[i]] = NULL   
    }
    x = cbind(x,y)
    return (x)
  }


