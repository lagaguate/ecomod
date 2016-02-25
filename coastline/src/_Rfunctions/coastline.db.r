
coastline.db = function( DS="gshhg coastline highres", crs="+init=epsg:4326", p=NULL, level=4, xlim=NULL, ylim=NULL, ... ) {

  #\\various methods to obtain coastline data
  RLibrary( "maps", "mapdata", "maptools", "rgdal", "rgeos" )
  datadir = project.datadirectory( "coastline" )
  if (! file.exists( datadir) ) dir.create(datadir, showWarnings=FALSE , recursive=TRUE ) 
  
  if (grepl( "gshhg", DS, ignore.case=TRUE ) ) {
    #\\ simple wrapper to read in the requested data
    fn.gshhs = "http://www.soest.hawaii.edu/pwessel/gshhg/gshhg-bin-2.3.4.zip"  
    fn.local = file.path( datadir, basename( fn.gshhs ) )

    if (DS=="gshhg.download") {
      #\\ GSHHG is probably the best world-wide standard for coastline, etc
      #\\ 'A Global Self-consistent, Hierarchical, High-resolution Geography' Database
      #\\ http://www.soest.hawaii.edu/pwessel/gshhg/
      print( "If this fails, you might need to update filename manually in coastline.db( DS='download' )" )
      download.file( url=fn.gshhs, destfile=fn.local )
      unzip( fn.local, exdir=file.path( datadir, "gshhg" )  )
    }

    if (grepl( "coastline", DS, ignore.case=TRUE )) fn.root = "gshhs"
    if (grepl( "rivers", DS, ignore.case=TRUE  )) fn.root = "wdb_rivers"
    if (grepl( "borders", DS, ignore.case=TRUE  )) fn.root = "wdb_borders"

    if (grepl("full", DS, ignore.case=TRUE ) ) fn.suffix = "f.b"
    if (grepl( "highres", DS, ignore.case=TRUE )) fn.suffix = "h.b"
    if (grepl( "intermediate", DS, ignore.case=TRUE  )) fn.suffix = "i.b"
    if (grepl( "low", DS, ignore.case=TRUE  )) fn.suffix = "l.b"
    if (grepl( "crude", DS, ignore.case=TRUE ) ) fn.suffix = "c.b"
  
    # construct the gshhg data filename appropriate to above choices:
    fn = file.path( datadir, "gshhg", paste( fn.root, fn.suffix, sep="_" ) )
    # local saves to speed things up a little 
    domain = "notspecified"
    if (!is.null(p)) {
      if ( exists("spatial.domain", p ) ) {
        domain = p$spatial.domain 
    }}
    fn.loc = paste( fn, domain, "rdata", sep="." )
    out = NULL
    if ( !grepl("redo", DS) ){
      if ( file.exists( fn.loc) ) {
        load( fn.loc )
        return (out)
    }}
    # if here then none found .. create a new one
    if ( !is.null(p)) {
      if (exists( "corners", p) ) {
        ylim = range(p$corners$lat)
        xlim = range(p$corners$lon)
      }
    }
    if (is.null(ylim)) stop( "Need lon/lat bounds in p$corners or xlim/ylim" )
    if (!file.exists(fn)) {
      print( "Global Self-consistent, Hierarchical, High-resolution Geography' Database")
      print( "not found ... Downloading to ecomod_data/coastline/ ..." )
      coastline.db( DS="gshhg.download")
    }
    print ("Don't panic about  the following .. Rgshhs is just being fussy:")
    out = maptools::getRgshhsMap( fn, xlim=xlim, ylim=ylim, level=level, verbose=FALSE, ... )
    print ("")
    print ("")
    print( "The above is not a fatal error .. check your data: " )
    print (out)
    if ( length(out) > 0 ) save (out, file=fn.loc, compress=TRUE )
    return(out)
  }

  # -----------------------------

  if (DS %in% c( "mapdata.coastLine", "mapdata.coastLine.redo")) {
    
    fn.coastline = file.path( datadir, "mapdata.coastline.rdata" )
    if ( file.exists( fn.coastline)) {
      load( fn.coastline) 
      if ( ! proj4string( coastSp ) ==  as.character(crs) ) coastSp = spTransform( coastSp, CRS(crs) )
      if (DS=="mapdata.coastLine") return( coastSp )
    }  
      coast = maps::map( database="worldHires", regions=c("Canada", "US"), fill=TRUE,
                  ylim=c(37,50), xlim=c(-72,-48), resolution=0, plot=FALSE)
      coastSp = map2SpatialLines( coast, IDs=sapply(coast$names, function(x) "0"),  # force all to be "0" elevation
                  proj4string= crs("+init=epsg:4326"))
      save( coastSp, file=fn.coastline ) ## save spherical
      if ( ! proj4string( coastSp ) == as.character(crs) ) coastSp = spTransform( coastSp, CRS(crs) )
      return( coastSp )
  }
  
  # ------------------------

  if (DS %in% c("mapdata.coastPolygon", "mapdata.coastPolygon.redo") ) {
    fn.coastpolygon = file.path( datadir, "mapdata.coastpolygon.rdata" )
    if ( file.exists( fn.coastpolygon)) {
      load( fn.coastpolygon) 
      if ( ! proj4string( coastSp ) == as.character(crs) ) coastSp = spTransform( coastSp, CRS(crs) )
      if (DS=="mapdata.coastPolygon") return( coastSp )
    } 
      RLibrary( "maps", "mapdata", "maptools", "rgdal" )
      coast = maps::map( database="worldHires", regions=c("Canada", "US"), fill=TRUE,
                  ylim=c(37,50), xlim=c(-72,-48), resolution=0, plot=FALSE)
      coastSp = map2SpatialPolygons( coast, IDs=sapply(coast$names, function(x) x[1]),
                  proj4string= crs("+init=epsg:4326"))
      save( coastSp, file=fn.coastpolygon )
      if ( ! proj4string( coastSp) == as.character( crs) ) coastSp = spTransform( coastSp, CRS(crs) )
      return( coastSp )
  }



  # -----------------------------
  
  if ( DS %in% c("gmt.coastline", "gmt.coastline.redo")) {
    # gmt methods are deprecated .. for posterity 
    fn = file.path( datadir, paste( "coastline", p$spatial.domain, "rdata", sep=".") )
    p = gmt.parameters( p) 
    if ( DS == "gmt.coastline" ) {
      out = NULL
      if (file.exists(fn) ) {
        load( fn)
        return (isobath)    
      }
    } 
    tmpdir =  tempdir()
    ib = NULL
    isobaths = file.path(tmpdir, make.random.string(".tmp.isobaths"))
    tmp.iso = file.path(tmpdir, make.random.string(".tmp.iso"))
    cmd( "pscoast", p$region, p$gmtproj, " -Df -M -W  >", isobaths )   # shorelines
    cmd( "pscoast", p$region, p$gmtproj, " -Df -M -N1 -N3  >>", isobaths ) # political boundaries
    cmd( "pscoast", p$region, p$gmtproj, " -Df -M -Ir >>", isobaths ) # rivers and lakes
    if ( file.exists( isobaths ) ) {
      cmd( "gawk '!/>/' ", isobaths, ">", tmp.iso )
      isobath = read.table(tmp.iso)
      isobath = isobath[, c(1,2)]
      names( isobath ) = c( "lon", "lat" )
      check = min( isobath$lon) 
      while ( check < -360 ) {
        isobath$lon = isobath$lon + 360 
        check = min( isobath$lon)
      }
      check = max( isobath$lon) 
      while ( check > 360 ) {
        isobath$lon = isobath$lon - 360 
        check = max( isobath$lon)
      }
      save( isobath, file=fn, compress=T)
      remove.files ( c(isobaths, tmp.iso) ) 
    }
    return(fn)
  }

}


