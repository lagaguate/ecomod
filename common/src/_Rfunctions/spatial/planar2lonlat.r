
  # inverse projection: planar to lon/lat using proj
  planar2lonlat = function (x, proj.type, pass.direct=F, planar.coord.scale="km" ) {

    if (planar.coord.scale == "km") {
      # coord systems work with meters by default
      x$plon = x$plon * 1000
      x$plat = x$plat * 1000
    }
   
    if (planar.coord.scale == "m") {
      # nothing to do .. here just as a reminder
      # coord systems work with meters by default
    }

    if (pass.direct) {
      proj4.params = proj.type
    } else {
      proj4.params = lookup.projection.params(proj.type)
    }
    y = rgdal::project( cbind(x$plon, x$plat), proj4.params, inv=T ) 
    colnames(y) = newnames  = c("lon", "lat")
    for (i in 1:length( newnames)) {
      if ( newnames[i] %in% colnames(x) ) x[, newnames[i]] = NULL   
    }
    x = cbind(x,y)
    return (x)
  }


