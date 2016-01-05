
  
  substrate.db = function( p=NULL, DS=NULL, grids.new=NULL ) {
 
    if ( DS %in% c("substrate.initial", "substrate.initial.redo") ) {
      # Read in the ArcInfo ascii grid file using library maptools and output a SpatialGridDataFrame
      # data provided by Kostelev: 
      # Kostylev, V.E., and Hannah, C.G., 2007, Process-driven characterization and mapping of seabed habitats,
      # in Todd, B.J.,and Greene, H.G., eds., Mapping the Seafloor for Habitat Characterization: 
      # Geological Association of Canada, Special Paper 47, p. 171-184.
      # Scotian shelf gridded grain size (mm).
      # NAD83 UTM zone 20 (I think)
    
      rawdata.file = file.path( project.datadirectory("substrate"), "data", "grainsize.txt" )
      filename = file.path( project.datadirectory("substrate"), "data", "substrate.asciigrid.rdata" )
      
      if (DS =="substrate.initial" ) {
        load( filename )   
        return ( substrate )
      }
      proj4.params = "+proj=utm +zone=20 +datum=NAD83 +units=m" #resolution is 500m X 500m
      substrate = sp::read.asciigrid( rawdata.file, proj4string=CRS( proj4.params ), colname="grainsize" )  ## mm
      save( substrate, file=filename, compress=T )
      return(filename)
    }

    # lon - lat converted
    if (  DS %in% c("lonlat.highres", "lonlat.highres.redo") ) {
      filename = file.path( project.datadirectory("substrate"), "data", "substrate.lonlat.highres.rdata" )
      if (DS =="lonlat.highres" ) {
        load( filename)
        return( substrate)
      }
      # initial data stored in planar coords ... convert to lon/lats
      substrate = substrate.db( DS="substrate.initial" )
      substrate = as.data.frame( substrate )
      names(substrate) = c("grainsize", "plon", "plat" )
      substrate = substrate[,c("plon", "plat", "grainsize")]  
      proj4.params = "+proj=utm +zone=20 +datum=NAD83 +units=m"  # original/raw data still in NAD83 geoid
      substrate= planar2lonlat ( substrate, proj4.params ) 
      substrate= substrate[ ,c("lon", "lat", "grainsize")]
      save( substrate, file=filename, compress=T   )
      return ( filename )
    }

    if ( DS %in% c("lonlat.interpolated", "lonlat.interpolated.redo") ) { 
      # interpolation to internal grid
      # locally (internally) force the highest possible resolution to not lose data and extrapolate safely
      filename.lonlat.interp = file.path( project.datadirectory("substrate"), "data", 
					paste( p$spatial.domain, "substrate.lonlat.interpolated.rdata", sep=".")  ) 
      if (DS =="lonlat.interpolated" ) {
        load (filename.lonlat.interp )
        return( substrate)
      }
      substrate = substrate.db( p, DS="lonlat.highres" ) 
    
      p$res = "-I10s"  # 10 arc sec -- ie. all data
      p$tension = "-T1" # interpolated but minimally smoothed solutions
      rlons = range(p$lons)
      rlats = range(p$lats)
      p$region = paste("-R", rlons[1], "/", rlons[2], "/", rlats[1], "/", rlats[2], sep="")

			names.sub = colnames( substrate )
      grainsize.range = range( substrate$grainsize, na.rm=T )
      substrate = interpol.grid(xyz=substrate, params=p, getdata=T, method="tps" )
      colnames( substrate ) = names.sub  # the above function renames the 3rd var to z   
      # interpolation can bring in data larger or smaller than realistic
      substrate$grainsize[ which( ( substrate$grainsize < grainsize.range[1] )) ] = grainsize.range[1] 
      substrate$grainsize[ which( ( substrate$grainsize > grainsize.range[2] )) ] = grainsize.range[2] 
      save( substrate, file=filename.lonlat.interp, compress=T ) 
      return( filename.lonlat.interp )
    }

    # ------------

    if ( DS %in% c("lonlat", "lonlat.redo", "lonlat.grid") ) { 
      # interpolation to internal grid
      # locally (internally) force the highest possible resolution to not lose data
      filename.lonlat = file.path( project.datadirectory("substrate"), "data", 
					paste( p$spatial.domain, "substrate.lonlat.rdata", sep=".") ) 
      filename.lonlat.grid = file.path( project.datadirectory("substrate"), "data", 
					paste( p$spatial.domain, "substrate.lonlat.grid.rdata", sep=".") ) 
      
      if (DS =="lonlat.grid" ) {
        load( filename.lonlat.grid )
        return( substrate )
      }
      if (DS =="lonlat" ) {
        load( filename.lonlat )
        return( substrate )
      }
      
			ilons = c( p$lons, p$lons[length(p$lons)]+(p$lons[2]-p$lons[1]) )
      ilats = c( p$lats, p$lats[length(p$lats)]+(p$lats[2]-p$lats[1]) )

      substrate = substrate.db( p, DS="lonlat.interpolated" ) 
      substrate$lon = as.numeric(as.character(cut(substrate$lon, ilons, include.lowest=T, right=F, labels=p$lons)))
      substrate$lat = as.numeric(as.character(cut(substrate$lat, ilats, include.lowest=T, right=F, labels=p$lats)))
      
      gc()
      substrate = block.spatial ( xyz=substrate, function.block=block.mean ) 
      save( substrate, file=filename.lonlat, compress=T ) 

      gc()
      substrate = xyz2grid( substrate, p$lons, p$lats)
      save( substrate, file=filename.lonlat.grid, compress=T ) 
      return ( paste( filename.lonlat.grid, filename.lonlat, sep="\n") )
    }
  

    if ( DS %in% c("planar", "planar.redo", "planar.grid") ) { 
      # Re-grid data to be internally consistent with the snowcrab coordinate system
      # WGS84 ellipsoid and not NAD83 ... 
      filename.planar = file.path( project.datadirectory("substrate"), "data", paste( p$spatial.domain, "substrate.planar.rdata", sep=".") ) 
      filename.planar.grid = file.path( project.datadirectory("substrate"), "data", paste( p$spatial.domain, "substrate.planar.grid.rdata", sep=".") ) 
  
      if (DS =="planar.grid" ) {
        load( filename.planar.grid )
        return( substrate )
      }
      if (DS =="planar" ) {
        load( filename.planar)
        return( substrate )
      }
    
      substrate = substrate.db( p, DS="lonlat.interpolated" ) 
      substrate = lonlat2planar( substrate,  proj.type=p$internal.projection )  # utm20, WGS84 (snowcrab geoid) 
      substrate = substrate[ ,c("plon", "plat", "grainsize" )]
        
			substrate$plon = grid.internal( substrate$plon, p$plons )
      substrate$plat = grid.internal( substrate$plat, p$plats )
      
      gc()
      substrate = block.spatial ( xyz=substrate, function.block=block.mean) 
      save( substrate, file=filename.planar, compress=T ) 

      gc()
      substrate = xyz2grid(substrate, p$plons, p$plats)
      save( substrate, file=filename.planar.grid, compress=T ) 
      return ( paste( filename.planar.grid, filename.planar, sep="\n" ) )
    } 
  
    # ---------------
    
    if (DS %in% c("substrate.spacetime.inputs.data.redo", "substrate.spacetime.inputs.data") ) { 
   
      datadir = project.datadirectory("substrate", "data" )
			dir.create( datadir, showWarnings=F, recursive=T )
      fn = file.path( datadir, paste( "substrate", "spacetime", p$spatial.domain, "rdata", sep=".") )
      
      if (DS =="substrate.spacetime.inputs.data" ) {
        load( fn)
        return( substrate )
      }

      substrate = bathymetry.db( p, DS="spde_complete", return.format = "list" ) 
      substrate$substrate = projectRaster( 
          from=raster( substrate.db( p=p, DS="substrate.initial" ) ), 
          to=spatial.parameters.to.raster( p) )
      substrate = as( brick(substrate), "SpatialGridDataFrame" )
 
      save (substrate, file=fn, compress=TRUE)
      return(fn)
    }


    ### ------
    

    if (DS %in% c("substrate.spacetime.inputs.prediction.redo", "substrate.spacetime.inputs.prediction") ) { 
   
      datadir = project.datadirectory("substrate", "data" )
			dir.create( datadir, showWarnings=F, recursive=T )
      fn = file.path( datadir, paste( "substrate", "spacetime", p$spatial.domain, "rdata", sep=".") )
      
      if (DS =="substrate.spacetime.inputs.prediction" ) {
        #load( fn)
        substrate = substrate.db( p, DS="substrate.spacetime.inputs.data" ) 
        return( substrate )
      }
      
      ### prediction grids are the same as the input grid .. do nothing for now
      ### but kept separate from "*...inputs" in case thy diverge in future
      print( "This is just a placeholder ..  grids are the same as inputs")
      # substrate = substrate.db( p, DS="substrate.spacetime.inputs.data" ) 
      # save (substrate, file=fn, compress=TRUE)
      return(fn)
    }

    #-------------------------

    if ( DS %in% c("substrate.spacetime.finalize.redo", "substrate.spacetime.finalize" )) {
      #// substrate( p, DS="substrate.spacetime.finalize(.redo)" return/create the 
      #//   spacetime interpolated method formatted and finalised for production use with predictions and statistics
      fn = file.path(  project.datadirectory("substrate"), "interpolated", 
        paste( "substrate", "spacetime", "finalized", p$spatial.domain, "rdata", sep=".") )
      if (DS =="substrate.spacetime.finalize" ) {
        B = NULL
        if ( file.exists ( fn) ) load( fn)
        return( B )
      }
 
      preds = spacetime.db( p=p, DS="predictions" )  
      nr = p$nplons
      nc = p$nplats
 
      BP = expand.grid( plon=p$plons, plat=p$plats ) # coords of full prediction area
      attr( BP, "out.attrs") = NULL
      BP$substrate.predictionMean = preds[,2] 
      BP$substrate.predictionSD   = preds[,3]
      
      # remove land
      oc = landmask( db="worldHires", regions=c("Canada", "US"), return.value="land", tag="predictions" )
      BP$substrate.predictionMean[oc] = NA
      BP$substrate.predictionSD[oc]   = NA

      rm(preds); gc()
     
      # merge into statistics
      BS = spacetime.db( p=p, DS="statistics" )
      B = cbind( BP, BS ) 
      names(B) = c( names(BP), "substrate.rangeMode", "substrate.rangeSD", "substrate.spatialSD", "substrate.observationSD" )

      save( B, file=fn, compress=TRUE)
      return(fn)

      if (0) {
        levelplot( log(substrate.predictionMean) ~ plon + plat, B, aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) ) 
        levelplot( log(substrate.rangeMode) ~ plon + plat, B, aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) ) 
        levelplot( substrate.rangeSD ~ plon + plat, B, aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) ) 
      }
    }

    # -------------
    
    if ( DS %in% c( "spde_complete", "spde_complete.redo" ) ) {
     #// substrate.db( DS="spde_complete" .. ) returns the final form of the substrate data after
     #// regridding and selection to area of interest as specificied by girds.new=c("SSE", etc)
      Z = NULL
      
      if ( DS %in% c("spde_complete", "complete") ) {
        
        if  (DS %in% c("complete")) {
          # for backwards compatibility
          Z = substrate.db( p=p, DS="spde_complete", return.format == "dataframe" )
          return (Z)
        }

        # DS="spde_complete"
        domain = NULL
        if ( is.null(domain)) {
          if ( exists("spatial.domain", p)) {
            domain = p$spatial.domain 
          } else if ( !is.null(grids.new)) { # over-rides p$spatial domain
            if( length( grids.new )== 1 ) {
              domain = grids.new
        } } }
        fn = file.path( project.datadirectory("substrate", "interpolated"), 
          paste( "substrate", "spde_complete", domain, "rdata", sep=".") )
        if ( file.exists ( fn) ) load( fn)
        if ( return.format == "dataframe" ) { ## default
          Z = as( brick(Z), "SpatialPointsDataFrame" ) 
          return( Z )
        } 
        if ( return.format == "dataframe.filtered" ) {
          Z = as( brick(Z), "SpatialPointsDataFrame" ) 
          Z = filter.bathymetry( DS=p$spatial.domain, Z=Z ) 
          return( Z )
        } 
        if ( return.format %in% c("list") ) return( Z  )
      }

      p0 = p  # the originating parameters
      Z0 = substrate.db( p=p0, DS="substrate.spacetime.finalize" )
      coordinates( Z0 ) = ~ plon + plat 
      crs(Z0) = crs( p0$interal.crs )
      above.sealevel = which( Z0$z < 0 ) # depth values < 0 are above  
      if (length(above.sealevel)>0) Z0[ above.sealevel ] = NA
 
      Z = list()
      
      grids = unique( c( p$spatial.domain, grids.new, "canada.east.highres.lonlat" ))

      for (gr in grids ) {
        p1 = spatial.parameters( type=gr )
        for (vn in names(Z0)) {
          Z[[vn]] = projectRaster( 
            from =rasterize( Z0, spatial.parameters.to.raster(p0), field=vn, fun=mean), 
            to   =spatial.parameters.to.raster( p1) )
        } 
        fn = file.path( project.datadirectory("substrate", "interpolated"), 
          paste( "substrate", "spde_complete", p1$spatial.domain, "rdata", sep=".") )
        save (Z, file=fn, compress=TRUE)
        print(fn)
      }
      return ( "Completed subsets" )
    }

  }


