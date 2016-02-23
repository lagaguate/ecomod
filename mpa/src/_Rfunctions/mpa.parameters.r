
  mpa.parameters = function( p=NULL, DS="default" ) {

    if (DS == "default" ) {
 
      p$default.spatial.domain = "canada.east"  # for temperature lookups
      p$taxa =  "maxresolved"
      p$seasons = "allseasons"
      p$data.sources = c("groundfish", "snowcrab")  # for bio.db
      p$nw = 10 # number of intervals in time within a year in the temperature interpolations ( must match temperature.r 's value )
    
      p$polygons$aoi = polygon.db( id="StAnnsBank_AOI" )
      p$polygons$z1 = polygon.db( id="StAnnsBank_Zone1" )
      p$polygons$z2 = polygon.db( id="StAnnsBank_Zone2" )
      p$polygons$z3 = polygon.db( id="StAnnsBank_Zone3" )
      p$polygons$z4 = polygon.db( id="StAnnsBank_Zone4" )

      return (p)
    }
 

    if ( DS=="mapping" ) {
      mapping = list() 
      mapping$xlim = c(-72, -56 ) # longitudes
      mapping$ylim = c(42, 49) # latitudes
      mapping$regions = c("Canada", "USA") # library "map" polygon designations
      mapping$output.directory = file.path( p$project.outdir.root, "maps")
      mapping$palette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
      mapping$depthcontours = c(0, 10, 100, 200, 300, 400, 500 )
      mapping$plygn = isobath.db( p=p, DS="isobath", depths=mapping$depthcontours, return.lonlat=TRUE  )
      return (mapping)
    }
  
  }


