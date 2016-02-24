figure.mpa.aoi = function(p) {
  plot( extent(p$map.contours), type="n", axes=FALSE, xlab="", ylab="" )
  for (i in 1: length(p$map.contours) )  lines( p$map.contours[i], col=p$map.depthcontours.colours[i] )
  lines( p$sab.polygons["StAnnsBank_AOI"], col="slateblue4", lwd=2 ) 
  sab.loc = rowMeans( bbox( p$sab.polygons["StAnnsBank_AOI"] ) )
  text( sab.loc[1], sab.loc[2]+40, " St Anns Bank \n MPA ", pos=3, col="slateblue4", cex=1.1 )
  sp::compassRose( p$corners$plon[1] + 60, p$corners$plat[2]-70, cex= 0.75 )
  box()
}


