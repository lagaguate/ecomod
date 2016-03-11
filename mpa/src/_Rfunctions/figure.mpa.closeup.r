
figure.mpa.closeup = function( p ) {
  
  polys = mpa.db( p=p, DS="polygons" ) # load saved version made in ecomod/mpa/src/mpa.r
  polys$map.contours = spTransform(polys$map.contours, CRS(p$internal.crs))  
  polys$map.coastline = spTransform(polys$map.coastline, CRS(p$internal.crs))  
  polys$sab.polygons = spTransform(polys$sab.polygons, CRS(p$internal.crs))  
  aoi = extent(polys$sab.polygons )
  aoi@xmin = aoi@xmin -55
  aoi@xmax = aoi@xmax +40
  aoi@ymin = aoi@ymin -50
  aoi@ymax = aoi@ymax +50
  plot( aoi, type="n",  xaxs="i", yaxs="i", axes=TRUE, xlab="Easting (km)", ylab="Northing (km)" )
  for (i in 1: length(polys$map.contours) )  lines( polys$map.contours[i], col=p$map.depthcontours.colours[i] )
  lines( polys$sab.polygons, col="gray70", lwd=1 ) 
  lines( polys$sab.polygons["StAnnsBank_AOI"], col="slateblue4", lwd=2.5 ) 
  lines( polys$map.coastline, col="steelblue"  )
  sp::compassRose( aoi@xmin + 18, aoi@ymax-18, cex= 0.75 )
  sab.loc = rowMeans( bbox( polys$sab.polygons["StAnnsBank_AOI"] ) )
  text( sab.loc[1], sab.loc[2]+8, " St Anns Bank \n MPA ", pos=3, col="slateblue4", cex=0.8 )
  text( sab.loc[1]-89, sab.loc[2]-17, " Cape Breton, \n Nova Scotia ", pos=3, col="steelblue", cex=0.8 )
  text( sab.loc[1]+50, sab.loc[2]+65, " Laurentian \n Channel ", pos=3, col="steelblue", cex=0.8 )
  
  box()

}

