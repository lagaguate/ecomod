spatial.parameters.to.raster = function( params ) {
  #\\ Take a spatial parameter list wirh corners and resolution and CRS
  #\\ and convert to a raster template
  require( raster) 

  ras = raster( 
    ncols=params$nplons, 
    nrows=params$nplats, 
    ext=extent ( rbind( params$corners$plon, params$corners$plat ) ), 
    crs=params$internal.crs )
 
  return(ras)
}
