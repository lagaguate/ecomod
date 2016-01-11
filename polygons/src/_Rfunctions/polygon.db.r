
polygon.db = function( DS="load", p=NULL, id=NULL, crs="", plotmap=FALSE ) {
  #\\ create/extract polygons and/or return on a map
  #\\ if crs is passed, default storage/load CRS is assumed lonlat
  require( rgdal)
  require( sp)
  library(maps) 
  library(mapdata)  # high resolution world coastlines/polygons  

  if (DS=="map.background") {
    # load libraries to quickly map coastlines
    map( database="worldHires", regions=p$regions, xlim=p$xlim, ylim=p$ylim, fill=FALSE, plot=TRUE )
    map.scale()
    box()
  }

  if (DS=="load") {
    fn = NULL
    fn = try( find.ecomod.gis( id ) )
    if ( class(fn) %in% "try-error") {
      print( "Something went wrong. See error message below:" )
      print( fn)
    }
    X = read.table (fn)
    names( X ) = c("lon", "lat" )
    if ( crs!="") {
      X = rgdal::project( as.matrix(X), proj=as.character(crs) ) 
      names(X) = c("plon", "plat")
    } 
    if (plotmap) {
      if ( crs=="" ) {
        polygon.db( DS="map.background", p=p) 
      } else {
        u = map( database="worldHires", regions=p$regions, xlim=p$xlim, ylim=p$ylim, fill=FALSE, plot=FALSE )  
        v = data.frame( cbind( u$x, u$y) )
        w = rgdal::project( as.matrix(v), proj=as.character(crs) )
        plot (w, pch=".", col="gray", xlab="Easting", ylab="Northing")
      }
      lines(X, col="green")
    }
    return(X)
  }

  if (DS=="create") {
    print( "Left mouse click and then to end right-mouse click (or [Esc] in Windows ) " )
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



