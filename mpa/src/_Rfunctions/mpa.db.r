
  mpa.db = function( p=NULL, DS="polygons" ) {

    if (DS == "polygons" ) {
      bbox = p$corners[ , c("lon", "lat")]
      colnames( bbox) = c("lon","lat")
      coordinates( bbox ) = c("lon", "lat")
      proj4string( bbox ) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      bbox = spTransform( bbox, CRS(p$internal.crs ) )

      aoi = polygon.db( id="StAnnsBank_AOI", returnvalue="sp.polygon", crs=p$internal.crs )  
      z1 = polygon.db( id="StAnnsBank_Zone1", returnvalue="sp.polygon", crs=p$internal.crs  )
      z2 = polygon.db( id="StAnnsBank_Zone2", returnvalue="sp.polygon" , crs=p$internal.crs )
      z3 = polygon.db( id="StAnnsBank_Zone3", returnvalue="sp.polygon", crs=p$internal.crs  )
      z4 = polygon.db( id="StAnnsBank_Zone4", returnvalue="sp.polygon", crs=p$internal.crs  )
      p$sab.polygons = bind( aoi, z1, z2, z3, z4, keepnames=TRUE ) 
      
      mc = spTransform( isobath.db( p=p, DS="isobath", depths=p$map.depthcontours, return.lonlat=TRUE ), CRS(p$internal.crs ) )
      mcnames = names( mc) 
      mcout = raster::crop( mc[1], bbox ) 
      for (i in 2:length(mc) ) {
        mcout = bind( mcout, raster::crop( mc[i], bbox ), keepnames=FALSE )
      }
      p$map.contours = mcout
      return (p)
    }
 
  }


