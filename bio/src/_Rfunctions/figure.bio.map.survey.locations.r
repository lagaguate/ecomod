figure.bio.map.survey.locations = function( p ) {
  set = bio.db( DS="set", p=p )
  loadfunctions( "coastline")
  # redo coast to get unclipped version
  coast = coastline.db( DS=" gshhg coastline full redo ", 
        xlim=p$corners$lon, ylim=p$corners$lat, no.clip=TRUE, level=1 )
  plot( coast, col="transparent", border="steelblue2" , 
     xlim=c(-68,-55), ylim=c(41,48),
     xaxs="i", yaxs="i", axes=TRUE )  # ie. coastline
  for (i in 1: length(polys$map.contours) ) lines( polys$map.contours[i], col=p$map.depthcontours.colours[i] )
  lines( polys$sab.polygons["StAnnsBank_AOI"], col="slateblue4", lwd=2 ) 
  sab.loc = rowMeans( bbox( polys$sab.polygons["StAnnsBank_AOI"] ) )
   
  points( set$lon, set$lat, pch=20, cex=0.5, col=dscols[ss$data.source]  )
 
  sp::compassRose( sab.loc[1]+2.3 , sab.loc[2]-4.3, cex= 0.7 )
  maps::map.scale( sab.loc[1]+1 , sab.loc[2]-5.1, ratio=FALSE, cex=0.8 )
  text( sab.loc[1], sab.loc[2]+0.2, " St Anns Bank \n MPA ", pos=3, col="slateblue4", cex=0.8 )
  box()

}


