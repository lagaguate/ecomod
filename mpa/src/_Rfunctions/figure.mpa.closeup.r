figure.mpa.closeup = function( p ) {
  aoi = extent(p$sab.polygons )
  aoi@xmin = aoi@xmin -55
  aoi@xmax = aoi@xmax +40
  aoi@ymin = aoi@ymin -50
  aoi@ymax = aoi@ymax +50
  plot( aoi, type="n", axes=FALSE, xlab="", ylab="" )
  for (i in 1: length(p$map.contours) )  lines( p$map.contours[i], col=p$map.depthcontours.colours[i] )
  lines( p$sab.polygons, col="gray70", lwd=1 ) 
  lines( p$sab.polygons["StAnnsBank_AOI"], col="slateblue4", lwd=2 ) 
  sp::compassRose( aoi@xmin + 15, aoi@ymax-15, cex= 0.75 )
  box()
}

