
spatial.parameters.to.raster = function( params, edge.reference=TRUE ) {
  #\\ Take a spatial parameter list wirh corners and resolution and CRS
  #\\ and convert to a raster template
  #\\ ecomod uses left edge as coordinates, raster uses center
  require( raster) 
  if (edge.reference) {
    params$corners$plon = params$corners$plon + c(-0.5, 0)*params$pres 
    params$corners$plat = params$corners$plat + c(-0.5, 0)*params$pres 
  }

  ras = raster( 
    ncols=params$nplons, 
    nrows=params$nplats, 
    res=params$pres ,
    xmn= params$corners$plon[1], # rasters are center referenced
    xmx= params$corners$plon[2],
    ymn= params$corners$plat[1],
    ymx= params$corners$plat[2],
    # ext=extent ( rbind( params$corners$plon, params$corners$plat ) ), 
    crs=params$internal.crs )
    
    dd = dim(ras)
    if( dd[1] != params$nplats) stop( "Dim of plats in error")
    if( dd[2] != params$nplons) stop( "Dim of plons in error")
    
    ras_coord = coordinates(ras)
    uu = sort(unique(ras_coord[,1]))
    vv = sort(unique(ras_coord[,2]))
    
    u = unique( params$plons-uu )
    v = unique( params$plats-vv )
  
    up=FALSE
    if(length(u)==0) if ( u!=0) {
      up=TRUE
      ras_coord[,1] = ras_coord[,1] + u
    }

    if(length(v)==0) if ( v!=0) {
      up=TRUE
      ras_coord[,1] = ras_coord[,1] + v
    }

    if(up) {
      print( "Raster coords offset issue ... trying a simple fix")
      uu = sort(unique(ras_coord[,1]))
      vv = sort(unique(ras_coord[,2]))
      u = unique( params$plons-uu )
      v = unique( params$plats-vv )
   
      if( u!=0) {
        print(" This has been tested with known projections in ecomod, this is a new one? and raster library is not happy? " )
        print(u)
        stop(" plons offsets are not unique/correct")
      }

      if( v!=0) {
        print(" This has been tested with known projectionsin ecomod, this is a new one? and raster library is not happy ..." )
        print(v)
        stop(" plats offsets are not unique/correct? ")
      }
      coordinates(ras) = ras_coord
    }

  return(ras)

}


if (0) {
  u = spatial.parameters.to.raster(p)
}


