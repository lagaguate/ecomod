
  
  substrate.db = function( p=NULL, DS=NULL ) {
 
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
      proj4.params = "+proj=utm +zone=20 +datum=NAD83 "
      substrate = readAsciiGrid( rawdata.file, proj4string=CRS( proj4.params ) )
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
      substrate$plon = substrate$plon / 1000  # convert to km
      substrate$plat = substrate$plat / 1000  # convert to km
      proj4.params = "+proj=utm +zone=20 +datum=NAD83"  # original/raw data still in NAD83 geoid
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
    
      jlons = c( p$plons, p$plons[length(p$plons)]+(p$plons[2]-p$plons[1]) )
      jlats = c( p$plats, p$plats[length(p$plats)]+(p$plats[2]-p$plats[1]) )
    
      substrate = substrate.db( p, DS="lonlat.interpolated" ) 
      substrate = lonlat2planar( substrate,  proj.type=p$internal.projection )  # utm20, WGS84 (snowcrab geoid) 
      substrate = substrate[ ,c("plon", "plat", "grainsize" )]
      
      substrate$plon = as.numeric(as.character(cut(substrate$plon, jlons, include.lowest=T, right=F, labels=p$plons)))
      substrate$plat = as.numeric(as.character(cut(substrate$plat, jlats, include.lowest=T, right=F, labels=p$plats)))
      
      gc()
      substrate = block.spatial ( xyz=substrate, function.block=block.mean) 
      save( substrate, file=filename.planar, compress=T ) 

      gc()
      substrate = xyz2grid(substrate, p$plons, p$plats)
      save( substrate, file=filename.planar.grid, compress=T ) 
      return ( paste( filename.planar.grid, filename.planar, sep="\n" ) )
    } 
  
    
    
    if (DS %in% "bigmemory.inla" ) { 

      # create file backed bigmemory objects

      p$tmp.datadir = file.path( p$project.root, "tmp" )
      if( !file.exists(p$tmp.datadir)) dir.create( p$tmp.datadir, recursive=TRUE, showWarnings=FALSE )

      # input data stored as a bigmatrix to permit operations with min memory usage
      p$backingfile.W = "input.bigmatrix.tmp"
      p$descriptorfile.W = "input.bigmatrix.desc"

      p$backingfile.P = "predictions.bigmatrix.tmp"
      p$descriptorfile.P = "predictions.bigmatrix.desc"

      p$backingfile.S = "statistics.bigmatrix.tmp"
      p$descriptorfile.S = "statstics.bigmatrix.desc"
     
#      p$backingfile.Pmat = "predictions_mat.bigmatrix.tmp"
#      p$descriptorfile.Pmat = "predictions_mat.bigmatrix.desc"

      p$outfilename.P =  file.path( p$project.root, "data", "predictions.rdata" ) 
      p$outfilename.S =  file.path( p$project.root, "data", "statistics.rdata" ) 

     
      # ------------------------------
      # load raw data .. slow so only if needed
      if (p$reload.rawdata) {
        SS = substrate.db ( p, DS="lonlat.highres" )  # signif=7 for lat, lon  
 
        # add depth and related stats
        #ZZ = bathymetry.db( p, DS="inla" )
        ZZ = bathymetry.db( p, DS="inla" )
       
        SS = lonlat2planar( SS, proj.type=p$internal.projection, ndigits=7 )
        # levelplot( z~plon+plat, SS, xlab='', ylab='', col.regions=colorRampPalette(c( "white", "darkblue", "black"), space = "Lab")(n), scale=list(draw=FALSE), aspect="iso", cuts=n )
        
        
        W = filebacked.big.matrix( nrow=nrow(SS), ncol=3, type="double", dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.W, descriptorfile=p$descriptorfile.W ) 
        W[] = as.matrix( SS[,c("plon", "plat", "grainsize")] )
      }

      if (p$reset.outputfiles ) {
        # ------------------------------
        # prediction indices in matrix structure 
        #  Pmat = filebacked.big.matrix( ncol=p$nplats, nrow=p$nplons, type="integer", dimnames=NULL, separated=FALSE, 
        #   backingpath=p$tmp.datadir, backingfile=p$backingfile.Pmat, descriptorfile=p$descriptorfile.Pmat ) 
        # Pmat[] = c(1:(p$nplons*p$nplats))
          # col=lat=ydir, row=lon=xdir is format of matrix image, etc
          # Pmat = matrix( 1:(p$nplons*p$nplats), ncol=p$nplats, nrow=p$nplons ) 
          # P = as.vector(Pmat)
          # Pmat[ cbind( round(( P$plon - p$plons[1]) / p$pres ) + 1, round(( P$plat - p$plats[1] ) / p$pres ) + 1 ) ] = P$var


        # ------------------------------
        # predictions storage matrix (discretized) 
        P = filebacked.big.matrix( nrow=p$nplon * p$nplat, ncol=3, type="double", init=0, dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.P, descriptorfile=p$descriptorfile.P ) 

        # ------------------------------
        # statistics storage matrix ( aggregation window, AW )
        sbbox = list( plats = seq( p$corners$plat[1], p$corners$plat[2], by=p$dist.mwin ), 
                      plons = seq( p$corners$plon[1], p$corners$plon[2], by=p$dist.mwin )
        )
        AW = expand.grid( sbbox$plons, sbbox$plats )
        attr( AW , "out.attrs") = NULL
        names( AW ) = c("plon", "plat")
        statsvars = c("range", "range.sd", "spatial.error", "observation.error") 
        nstats = length( statsvars ) 
        S = filebacked.big.matrix( nrow=nrow(AW), ncol=nstats+2, type="double", init=0, dimnames=NULL, separated=FALSE, 
          backingpath=p$tmp.datadir, backingfile=p$backingfile.S, descriptorfile=p$descriptorfile.S ) 
        S[,1] = AW[,1]
        S[,2] = AW[,2]
      
      }

      S = attach.big.matrix(p$descriptorfile.S , path=p$tmp.datadir ) 
      p$nS = nrow(S) 
      
      return(p)
    }
    
  }


