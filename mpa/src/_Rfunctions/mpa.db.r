
  mpa.db = function( p=NULL, DS="polygons", crs=NULL ) {

    mpadir = project.datadirectory( "mpa", "data")
    
    if (DS %in% c( "polygons.redo", "polygons" ) ) {
      fn = file.path( mpadir, "sab.polygons.rdata" )
      if (DS == "polygons" ) {
        out = NULL
        if  (file.exists( fn)) load(fn)
          if ( !is.null(crs)) {
            out$map.contours = spTransform(out$map.contours, CRS(p$internal.crs))  
            out$map.coastline = spTransform(out$map.coastline, CRS(p$internal.crs))  
            out$map.coastline.unclipped = spTransform(out$map.coastline.unclipped, CRS(p$internal.crs))  
            out$sab.polygons = spTransform(out$sab.polygons, CRS(p$internal.crs))  
          }
        return (out)
      }

      # storage/internal format is lon/lat:
      crs = "+init=epsg:4326"
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
      out$map.contours = isobath.db( p=p, DS="isobath", depths=p$map.depthcontours, crs=crs  )
      
      out$map.coastline = coastline.db( DS=" gshhg coastline highres redo ", 
        xlim=p$corners$lon+c(-1,1), ylim=p$corners$lat+c(-1,1), no.clip=FALSE, level=1 )  # add a small buffer abound data

      out$map.coastline.unclipped = coastline.db( DS=" gshhg coastline full redo ", 
        xlim=p$corners$lon, ylim=p$corners$lat, no.clip=TRUE, level=1 )
      
      save( out, file=fn, compress=TRUE )
      if ( !is.null(crs)) {
        out$map.contours = spTransform(out$map.contours, CRS(p$internal.crs))  
        out$map.coastline = spTransform(out$map.coastline, CRS(p$internal.crs))  
        out$map.coastline.unclipped = spTransform(out$map.coastline.unclipped, CRS(p$internal.crs))  
        out$sab.polygons = spTransform(out$sab.polygons, CRS(p$internal.crs))  
      }
      return (out)
    }

  }


