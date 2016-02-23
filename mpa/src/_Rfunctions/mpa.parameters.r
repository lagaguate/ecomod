
  mpa.parameters = function( p=NULL, DS="default" ) {

    if (DS == "default" ) {
 
      p$default.spatial.domain = "canada.east"  # for temperature lookups
      p$taxa =  "maxresolved"
      p$seasons = "allseasons"
      p$data.sources = c("groundfish", "snowcrab")  # for bio.db
      p$nw = 10 # number of intervals in time within a year in the temperature interpolations ( must match temperature.r 's value )
    

      return (p)
    }
 

    if ( DS=="mapping" ) {
      p$mapping$regions = c("Canada", "USA") # library "map" polygon designations
      p$mapping$output.directory = file.path( p$project.outdir.root, "maps")
      p$mapping$palette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
      p$mapping$contours = isobath.db( p=p, DS="isobath", depths=p$mapping$depthcontours, return.lonlat=FALSE  )
      
        aoi = polygon.db( id="StAnnsBank_AOI", returnvalue="sp.polygon", crs=p$internal.crs ),  
        z1 = polygon.db( id="StAnnsBank_Zone1", returnvalue="sp.polygon", crs=p$internal.crs  ),
        z2 = polygon.db( id="StAnnsBank_Zone2", returnvalue="sp.polygon" , crs=p$internal.crs ),
        z3 = polygon.db( id="StAnnsBank_Zone3", returnvalue="sp.polygon", crs=p$internal.crs  ),
        z4 = polygon.db( id="StAnnsBank_Zone4", returnvalue="sp.polygon", crs=p$internal.crs  )
     
      p$polygons = bind( aoi, z1, z2, z3, z4 ) 

      return (p)
    }
  
  }


