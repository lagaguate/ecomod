
polygon.db = function( DS="load", p, id=NULL, crs=NULL ) {
  #\\ create/extract polygons and/or return on a map
  #\\ if crs is passed, default storage/load CRS is assumed lonlat
  require( rgdal)
  require( sp)

  if (DS=="map.background") {
    # load libraries to quickly map coastlines
    library(maps) 
    library(mapdata)  # high resolution world coastlines/polygons  
    map( regions=p$regions, xlim=p$xlim, ylim=p$ylim, fill=FALSE, plot=TRUE )
    map.scale()
    box()
  }

  if (DS=="load") {
    fn = NULL
    fn = try( find.ecomod.gis( id ) )
    if ( class(fn) %in% "try-error") {
      print( "Something wnet wrong. See error message below:" )
      print( fn)
    }
    X = read.table (fn)
    names( X) = c("lon", "lat" )
    if (!is.null( crs )) X = rgdal::project( as.matrix(X), proj=as.character(crs) ) 
    return(X)
  }

  if (DS=="create") {
    polygon.db ( DS="map.background", p=p ) 
    X = locator(type="o" )
    X = as.data.frame( X)
    names(X) = c("lon", "lat")
    X = rbind(X, X[1,]) 
    lines (X)
    u = readline("If it looks good, type 'good' and [enter], otherwise interrrupt and start over.. " )
    save.filename = file.path( p$output.directory, p$output.filename )
    print( save.filename )
    if (file.exists(save.filename) ) {
      u = readline( "Filename (above) exists. Interrupt now otherwise it will be overwritten" )
    }
    write.table( X, file=save.filename )
    return ("save completed")  
  }

}



