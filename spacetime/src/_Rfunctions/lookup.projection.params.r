  lookup.projection.params = function(x) {

    pproj = cbind ("tmercator", "+proj=tmerc +ellps=WGS84 +x_0=500000 +y=-400000 +lon_0=90w " )
    pproj = rbind( pproj, cbind ("lambert.conic", "+proj=lcc +ellps=WGS84 +lon_0=63W +lat_0=45N +lat_1=43N +lat_2=47N " ))
    pproj = rbind( pproj, cbind ("lambert.conic.4t", "+proj=lcc +ellps=WGS84 +lon_0=63W +lat_0=47N +lat_1=45N +lat_2=49N " ))
    pproj = rbind( pproj, cbind ("lambert.conic.canada.east", "+proj=lcc +ellps=WGS84 +lon_0=62W +lat_0=45N " ))
    pproj = rbind( pproj, cbind ("utm20", "+proj=utm +ellps=WGS84 +zone=20 " ))
    pproj = rbind( pproj, cbind ("utm20.substrate", "+proj=utm +datum=NAD83 +zone=20 " ))
#    pproj = rbind( pproj, cbind ("", "" ))

    i = which(pproj[,1]==x)
    return (pproj[i,2])

  }


