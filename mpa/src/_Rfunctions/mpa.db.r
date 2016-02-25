
  mpa.db = function( p=NULL, DS="polygons" ) {

    mpadir = project.datadirectory( "mpa", "data")
    
    if (DS %in% c( "polygons.redo", "polygons" ) ) {
      fn = file.path( mpadir, "sab.polygons.rdata" )
      if (DS == "polygons" ) {
        out = NULL
        if  (file.exists( fn)) load(fn)
        return (out)
      }

      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84"
      out = list()
      bbox = p$corners[ , c("lon", "lat")]
      colnames( bbox) = c("lon","lat")
      coordinates( bbox ) = c("lon", "lat")
      proj4string( bbox ) = crs
      bbox = spTransform( bbox, CRS(crs) )

      aoi = polygon.db( id="StAnnsBank_AOI", returnvalue="sp.polygon", crs=crs )  
      z1 = polygon.db( id="StAnnsBank_Zone1", returnvalue="sp.polygon", crs=crs  )
      z2 = polygon.db( id="StAnnsBank_Zone2", returnvalue="sp.polygon", crs=crs  )
      z3 = polygon.db( id="StAnnsBank_Zone3", returnvalue="sp.polygon", crs=crs  )
      z4 = polygon.db( id="StAnnsBank_Zone4", returnvalue="sp.polygon", crs=crs   )
      
      out$sab.polygons = bind( aoi, z1, z2, z3, z4, keepnames=TRUE ) 
      
      mc = isobath.db( p=p, DS="isobath", depths=p$map.depthcontours, crs=crs  )
      mcnames = names( mc)
      # must crop each one separately
      mcout = raster::crop( mc[1], bbox ) 
      for (i in 2:length(mc) ) {
        mcout = bind( mcout, raster::crop( mc[i], bbox ), keepnames=FALSE )
      }
      out$map.contours = mcout
      out$map.coastline = coastline.db( p=p, crs=crs )
      save( out, file=fn, compress=TRUE )
      return (out)
    }

  }


