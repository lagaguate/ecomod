
polygon.db = function( DS="load", p=NULL, id=NULL, crs="+init=epsg:4326", plotmap=FALSE, returnvalue="default" ) {
  #\\ create/extract polygons and/or return on a map
  #\\ if crs is passed, default storage/load CRS is assumed lonlat
  #\\ default return value is lon/lat in data frame, also possible to return as a polygon
  require( rgdal)
  require( sp)
  require(raster)
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
    colnames( X ) = c("lon", "lat" )
    if ( as.character(crs) != "+init=epsg:4326" ) {
      YY = X 
      coordinates(YY) = ~lon+lat
      proj4string( YY) =  "+init=epsg:4326" 
      Z = spTransform( YY, CRS(crs) ) 
      X = coordinates(Z)
    } 
    if (plotmap) {
      if ( as.character(crs) != "+init=epsg:4326"  ) {
        polygon.db( DS="map.background", p=p) 
      } else {
        u = map( database="worldHires", regions=p$regions, xlim=p$xlim, ylim=p$ylim, fill=FALSE, plot=FALSE )  
        v = data.frame( cbind( u$x, u$y) )
        w = rgdal::project( as.matrix(v), proj=as.character(crs) )
        plot (w, pch=".", col="gray", xlab="Easting", ylab="Northing")
      }
      lines(X, col="green")
    }
    if (returnvalue=="default") return( X )
    if (returnvalue=="sp.polygon") return( xy.to.SpatialPolygon(X, id=id, crs=as.character(crs) ) )
  }

  if (DS=="create") {
    print( "Left mouse click and then to end right-mouse click (or [Esc] in Windows ) " )
    polygon.db ( DS="map.background", p=p ) 
    X = locator(type="o" )
    X = as.data.frame( X)
    colnames(X) = c("lon", "lat")
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



