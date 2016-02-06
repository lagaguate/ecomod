
spacetime.regrid = function( Z0, p0, p1, method="fast" ) {
  #\\ regrid/reproject from p0 to p1 
  #\\ rgdal calls this "warping"
  
  if (method=="raster") {
    # here Z0 has to be rasterizable or a list of rasters or a brick
    require (raster)
    for (vn in names(Z0)) {
       Z[[vn]] = projectRaster( 
          from = rasterize( Z0, spatial.parameters.to.raster(p0), field=vn, fun=mean), 
          to   = spatial.parameters.to.raster( p1) )
    }
    return (Z)
  }

  if (method=="fast") {
      # extract coords, convert to new coords, interpolate where required an
      L0 = planar2lonlat( L0, proj.type=p0$internal.projection )  # convert to lon lat
      # L0$plon0 = L0$plon
      # L0$plat0 = L0$plat
      L0 = lonlat2planar( L0, proj.type=p1$internal.projection )  # convert lon, lat to new projection     
      L0$plon = grid.internal( L0$plon, p1$plons ) # ensure correct resolution
      L0$plat = grid.internal( L0$plat, p1$plats )
      L2M = cbind( ( L0$plon-p1$plons[1])/p1$pres + 1, (L0$plat-p0$plats[1])/p0$pres + 1) # row, col indices in matrix form of the new coordinate system
      M = matrix( NA, nrow=p0$nplons, ncol=p0$nplats) # matrix respresentation of the data in new coord system
      M[L2M] = Z0
      # L1 = target locations in new coordinate system
      Z1 = fields::interp.surface( list( x=p1$plons, y=p1$plats, z=M ), loc=L1 )
      ii = which( is.na( Z1 ) )
      if ( length( ii) > 0 ) {
        theta = 7.5 #km
        nsd = 6 # no SD's in buffer
        wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres, 
                theta=theta, xwidth=nsd*theta, ywidth=nsd*theta )
        Z1ii =  fields::image.smooth( M, dx=p1$pres, dy=p1$pres, wght=wght )$z  
        Z1[ii] =  fields::interp.surface( list( x=p1$plons, y=p1$plats, z=Z1ii), loc=L1[ii,] )
      }
      
    return( Z1)
  }

 
}


