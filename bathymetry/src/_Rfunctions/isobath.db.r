
isobath.db = function( ip=NULL, p=NULL, depths=c(100, 200), DS="isobath", return.lonlat=FALSE ) {
  #\\ create or return isobaths and coastlines/coast polygons
  #\\ return.lonlat=TRUE forces spherical coords otherwise planar coords as defined in p
  #browser()
  if (DS %in% c( "isobath", "isobath.redo" )) {
    fn.iso = file.path( project.datadirectory("bathymetry", "isobaths" ), "isobaths.rdata" ) 
    isobaths = NULL
    notfound = NULL
      
    if ( DS != "isobath.redo" & file.exists(fn.iso) ) {
      load(fn.iso)
      notfound = setdiff( as.character(depths), names(isobaths) )
      if (length( notfound)==0) {
        if (!return.lonlat) isobaths = spTransform( isobaths, CRS(p$internal.crs))
        if (DS=="isobath") return( isobaths[ as.character(depths) ] )
      }
    }
    
    p1 = spatial.parameters( type="canada.east.highres" )
    depths = sort( unique(c(depths, notfound) ))
    addcoast = FALSE
    if ( 0 %in% depths ) {
      addcoast = TRUE
      depths = setdiff(depths, 0)
    }
    Z = bathymetry.db( p=p1, DS="spde_complete", return.format="list" )$z  #planar coords

    cl = contourLines( x=p$plons, y=p$plats, t(as.matrix( flip( Z, direction="y") )), levels=depths )
    isobaths = maptools::ContourLines2SLDF(cl, proj4string=CRS( p1$internal.crs ) )
    row.names(slot(isobaths, "data")) = as.character(depths)
    for (i in 1:length(depths)) slot( slot(isobaths, "lines")[[i]], "ID") = as.character(depths[i])
    isobaths = as.SpatialLines.SLDF( isobaths )
    crs( isobaths ) =  crs ( p1$internal.crs )  # crs gets reset .. not sure why
    isobaths = spTransform( isobaths, CRS("+init=epsg:4326") )  ## longlat  as storage format
    if (addcoast) {
      # add coastline .. contour is too jagged.. used mapdata coastline 
      coast = isobath.db( p=p1, DS="coastLine", return.lonlat=TRUE ) 
      isobaths = rbind( coast, isobaths )
    }
    save( isobaths, file=fn.iso, compress=TRUE) # save spherical
    if (!return.lonlat) isobaths = spTransform( isobaths, CRS(p$internal.crs))
    return( isobaths )
  }

  # ------------------------
  
  if (DS %in% c( "coastLine", "coastLine.redo")) {
    fn.coastline = file.path( project.datadirectory("bathymetry", "isobaths" ), "coastline.rdata" )
    if ( file.exists( fn.coastline)) {
      load( fn.coastline) 
      if (!return.lonlat) coastSp = spTransform( coastSp, crs(p$internal.crs) )
      if (DS=="coastLine") return( coastSp )
    }  
      RLibrary( "maps", "mapdata", "maptools", "rgdal" )
      coast = maps::map( database="worldHires", regions=c("Canada", "US"), fill=TRUE,
                  ylim=c(37,50), xlim=c(-72,-48), resolution=0, plot=FALSE)
      coastSp = map2SpatialLines( coast, IDs=sapply(coast$names, function(x) "0"),  # force all to be "0" elevation
                  proj4string= crs("+init=epsg:4326"))
      save( coastSp, file=fn.coastline ) ## save spherical
      if (!return.lonlat) coastSp = spTransform( coastSp, crs(p$internal.crs) )
      return( coastSp )
  }
  
  # ------------------------

  if (DS %in% c("coastPolygon", "coastPolygon.redo") ) {
    fn.coastpolygon = file.path( project.datadirectory("bathymetry", "isobaths" ), "coastpolygon.rdata" )
    if ( file.exists( fn.coastpolygon)) {
      load( fn.coastpolygon) 
      if (!return.lonlat) coastSp = spTransform( coastSp, crs(p$internal.crs) )
      if (DS=="coastPolygon") return( coastSp )
    } 
      RLibrary( "maps", "mapdata", "maptools", "rgdal" )
      coast = maps::map( database="worldHires", regions=c("Canada", "US"), fill=TRUE,
                  ylim=c(37,50), xlim=c(-72,-48), resolution=0, plot=FALSE)
      coastSp = map2SpatialPolygons( coast, IDs=sapply(coast$names, function(x) x[1]),
                  proj4string= crs("+init=epsg:4326"))
      save( coastSp, file=fn.coastpolygon )
      if (!return.lonlat) coastSp = spTransform( coastSp, crs(p$internal.crs) )
      return( coastSp )
  }


  # -----------------------------
  
  if ( DS %in% c("gmt", "gmt.redo")) {
    # gmt methods are deprecated
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    datadir = project.datadirectory("bathymetry", "isobaths", p$spatial.domain ) 
    if (! file.exists( datadir) ) dir.create(datadir, showWarnings=FALSE , recursive=TRUE ) 

    if ( DS == "gmt" ) {
      out = NULL
      for (d in depths) {
        fn = file.path( datadir, paste( "isobath", d, "rdata", sep=".") )
        if (file.exists(fn) ) load( fn)
        out = rbind( out, isobath )
      }
      return (out)    
    } 
    tmpdir =  tempdir()
    # ip is the first parameter passed in the parallel mode
    if (is.null(ip)) ip = 1:p$nruns
    for (id in ip ) {
      d = p$runs[ id, "depths" ]
      fn = file.path( datadir,  paste( "isobath", d, "rdata", sep=".") )
      ib = NULL
      print (fn)
      isobaths = file.path(tmpdir, make.random.string(".tmp.isobaths"))
      tmp.iso = file.path(tmpdir, make.random.string(".tmp.iso"))

      if (d==0) { # ie. coastline 
        cmd( "pscoast", p$region, p$gmtproj, " -Df -M -W  >", isobaths )   # shorelines
        cmd( "pscoast", p$region, p$gmtproj, " -Df -M -N1 -N3  >>", isobaths ) # political boundaries
        cmd( "pscoast", p$region, p$gmtproj, " -Df -M -Ir >>", isobaths ) # rivers and lakes
      } else {
        gmt.bin = p$bathymetry.bin 
        gmt.clip = file.path(tmpdir, make.random.string(".gmt.clip"))
        gmt.depths = file.path(tmpdir, make.random.string(".gmt.depths"))
        gmt.depth.mask = file.path(tmpdir, make.random.string(".gmt.depth.mask"))
        gmt.data.landmask = file.path(tmpdir, make.random.string(".gmt.data.landmask"))
        basemap = file.path(tmpdir, make.random.string(".gmt.basemap.ps"))
        bathy.masked = file.path(tmpdir, make.random.string(".gmt.bathy.masked"))
        bathy.contour= paste( "-S10", paste("-D", isobaths, sep=""), paste("-C", abs(d), sep=""), paste("-L",d-1,"/",d+1,sep="") ) # override default
      # cmd( "gmtconvert -bo", inp, ">", gmt.bin )
        cmd( "blockmedian -bi3 -bo", p$bathymetry.bin, p$region, p$res.isobaths , ">", gmt.clip )
        cmd( "surface -bi3", gmt.clip, p$region, p$res.isobaths , p$bathy.tension, paste("-G", gmt.depths, sep="" ))
        cmd( "grdclip", gmt.depths, p$bathy.zrange, paste("-G", gmt.depth.mask, sep="") )
        cmd( "grdlandmask", p$region, p$res.isobaths , "-N1/NaN/NaN/NaN/NaN -Dif", paste("-G", gmt.data.landmask, sep=""))
        cmd( "grdmath", gmt.data.landmask, gmt.depth.mask, "MUL =", bathy.masked)
        cmd( "grdcontour", bathy.masked, p$gmtproj, bathy.contour, ">", basemap )
        remove.files ( c( basemap, gmt.clip, gmt.depths, gmt.data.landmask, gmt.depth.mask, bathy.masked ) )
      }
      
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
    }
    return(fn)
  }

}


