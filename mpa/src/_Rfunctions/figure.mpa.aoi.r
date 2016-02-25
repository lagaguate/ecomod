
figure.mpa.aoi = function(p ) {
  polys = mpa.db( p=p, DS="polygons" )
  polys$map.contours = spTransform(polys$map.contours, CRS(p$internal.crs))  
  polys$map.coastline = spTransform(polys$map.coastline, CRS(p$internal.crs))  
  polys$sab.polygons = spTransform(polys$sab.polygons, CRS(p$internal.crs))  
  plot( extent(polys$map.contours), type="n", xaxs="i", yaxs="i", axes=TRUE )
  for (i in 1: length(polys$map.contours) ) lines( polys$map.contours[i], col=p$map.depthcontours.colours[i] )
  lines( polys$sab.polygons["StAnnsBank_AOI"], col="slateblue4", lwd=2 ) 
  lines( polys$map.coastline, col="steelblue"  )
  sab.loc = rowMeans( bbox( polys$sab.polygons["StAnnsBank_AOI"] ) )
  text( sab.loc[1], sab.loc[2], " St Anns Bank \n MPA ", pos=3, col="slateblue4", cex=1.1 )
  sp::compassRose( p$corners$plon[2]-50 , p$corners$plat[1]+50, cex= 0.75 )
  box()
}


