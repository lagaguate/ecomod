
  bathymetry.db = function( p=NULL, DS=NULL, additional.data=c("snowcrab", "groundfish") ) {
     
    if ( DS =="Greenlaw_DEM") {
      # DEM created 2014
      # GCS_WGS_1984, UTM_Zone_20N; spheroid:: 6378137.0, 298.257223563
      # 322624071 "grid points
      # 50 m  horizontal resolution
      # depth range: -5053.6 to 71.48 m 
      fn = project.directory( "bathymetry", "data", "bathymetry.greenlaw.rdata" )
      if (file.exists (fn) ) {
        load(fn)
        return(gdem)
      }

      require(rgdal)
      demfile.adf = project.directory( "bathymetry", "data", "greenlaw_DEM", "mdem_50", "w001001.adf" )  # in ArcInfo adf format
      dem = new( "GDALReadOnlyDataset", demfile.adf )
      # gdem = asSGDF_GROD( dem, output.dim=dim(dem) ) # regrid to another dim
      # gdem = getRasterData(dem) # in matrix format
      gdem = getRasterTable(dem) # as a data frame
      names(gdem) = c("plon", "plat", "z")
      gdem = gdem[ is.finite( gdem$z ) , ]
      gdem = gdem[ which( gdem$z < p$depthrange[2] ) , ] # limit to 3000m depths due to file size
      gdem = gdem[ which( gdem$z > p$depthrange[1] ) , ] # limit to 3000m depths due to file size
      gdem = planar2lonlat( gdem, "utm20", planar.coord.scale=1 )  # plon,plat already in meters
      gdem = gdem[, c("lon", "lat", "z") ]
      save( gdem, file=project.directory( "bathymetry", "data", "bathymetry.greenlaw.rdata"), compress=TRUE )
    }


    if (  DS %in% c("z.lonlat.rawdata.redo", "z.lonlat.rawdata") ) {
			# raw data minimally modified all concatenated
      
      datadir = project.directory("bathymetry", "data" )
			dir.create( datadir, showWarnings=F, recursive=T )
      
      fn = file.path( datadir, "bathymetry.canada.east.lonlat.rawdata.rdata" )
      
      if (DS =="z.lonlat.rawdata" ) {
        load( fn)
        return( bathy )
      }
 
			# this data was obtained from CHS via Jerry Black in 13 April 2009 n=9,965,979 -- 
      fn  = file.path( datadir, "jerry.black.xyz.xz")  # xz compressed file
      chs.jerry.black = read.table( xzfile(fn), header=T, sep="," )
			names( chs.jerry.black ) = c("lon", "lat", "z")
      chs.jerry.black = chs.jerry.black[ which( chs.jerry.black$z < 1000 ), ] # remove some large values that are likely "missing values" 
      chs.jerry.black$z = - chs.jerry.black$z  

			# this data was obtained from CHS via David Greenberg in 2004; range = -5467.020, 383.153; n=28,142,338
      fn = file.path( datadir, "nwa.chs15sec.xyz.xz") # xz compressed file
      chs15 = read.table( xzfile( con ) ) 
      names(chs15) = c("lon", "lat", "z")
      chs15 = chs15[ which( chs15$z < 1000 ) , ] 
      chs15$z = - chs15$z  
 
      bathy = rbind( chs15, chs.jerry.black )
      rm( chs15, chs.jerry.black ); gc()

      # Michelle Greenlaw's DEM from 2014
      # range -3000 to 71.5 m; n=155,241,029 .. but mostly interpolated 
      gdem = bathymetry.db( DS="Greenlaw_DEM" )
      gdem$z = - gdem$z

      bathy = rbind( bathy, gdem )
      rm(gdem) ; gc()

	 		# chs and others above use chs depth convention: "-" is below sea level,
			# in snowcrab and groundfish convention "-" is above sea level
			# retain postive values at this stage to help contouring near coastlines

			if ( "snowcrab" %in% additional.data ) {
        # range from 23.8 to 408 m below sea level ... these have dropped the "-" for below sea level; n=5925 (in 2014)
			  p0 = p  # loadfunctions "snowcrab" will overwrite p .. store a copy and return it to original state below	
        loadfunctions( "snowcrab")
        sc = snowcrab.db("set.clean")[,c("lon", "lat", "z") ]
				sc = sc [ which (is.finite( rowSums( sc ) ) ) ,]
				j = which(duplicated(sc))
        if (length (j) > 0 ) sc = sc[-j,]
        bathy = rbind( bathy, sc )
			  p = p0
        rm (sc); gc()
      }

			if ( "groundfish" %in% additional.data ) {
        # n=13031; range = 0 to 1054
				loadfunctions("groundfish")
				gf = groundfish.db( "set.base" )[, c("lon","lat", "sdepth") ]
				gf = gf[ which( is.finite(rowSums(gf) ) ) ,]
        names(gf) = c("lon", "lat", "z")
				j = which(duplicated(gf))
        if (length (j) > 0 ) gf = gf[-j,]
 				bathy = rbind( bathy, gf )
        rm (gf); gc()
			}
 
      write.table( bathy, file=p$bathymetry.xyz, col.names=F, quote=F, row.names=F)
      
			cmd( "gmtconvert -bo", p$bathymetry.xyz, ">", p$bathymetry.bin )
      save( bathy, file=fn, compress=T )
      return ( fn )
    }


    if ( DS %in% c("prepare.intermediate.files.for.dZ.ddZ", "Z.gridded", "dZ.gridded", "ddZ.gridded" ) ) {
			
			tmpdir  = tempdir()
			outdir = project.directory("bathymetry", "interpolated" )
			dir.create( outdir, showWarnings=F, recursive=T )
 
			fn.interp.z = file.path( outdir,  paste(  p$spatial.domain, "Z.interpolated.xyz", sep=".") )
			fn.interp.dz = file.path( outdir,  paste( p$spatial.domain, "dZ.interpolated.xyz", sep=".") )
			fn.interp.ddz = file.path( outdir,  paste( p$spatial.domain, "ddZ.interpolated.xyz", sep=".") )
			
			if (DS=="Z.gridded") { 
				Z = read.table( fn.interp.z )
				names( Z ) = c("lon", "lat", "z")
				return( Z )
			} 
			if (DS=="dZ.gridded") { 
				dZ = read.table( fn.interp.dz )
				names( dZ ) = c("lon", "lat", "dZ")
				return( dZ )
			} 
			if (DS=="ddZ.gridded") { 
				ddZ = read.table( fn.interp.ddz )
				names( ddZ ) = c("lon", "lat", "ddZ")
				return( ddZ )
			} 

			append = "-O -K"
			b.res = "-I10s"  # use full resolution of bathymetry data
			bathy.tension = "-T0.75"  # large steepness :: 0.35+ for steep; 0.25 for smooth
			blocked = file.path(tmpdir, make.random.string(".gmt.blocked"))
			grids  = file.path(tmpdir, make.random.string( ".gmt.depths"))
			z.dds = file.path(tmpdir, make.random.string( ".gmt.z.dds"))
			z.d2ds2 = file.path(tmpdir, make.random.string( ".gmt.z.d2ds2"))

			cmd( "blockmean", p$bathymetry.bin, "-bi3 -bo", p$region, b.res, ">", blocked )  
			cmd( "surface", blocked, "-bi3", p$region, b.res, bathy.tension, paste("-G", grids, sep="") )
			cmd( "grdmath -M", grids, "DDX ABS", grids, "DDY ABS ADD 0.5 MUL =", z.dds )
			cmd( "grdmath -M -N", grids, "CURV =", z.d2ds2 )
			cmd( "grd2xyz", grids, ">", fn.interp.z )  # the scalar in meter 
			cmd( "grd2xyz", z.dds, ">", fn.interp.dz )  # the scalar in meter / meter
			cmd( "grd2xyz", z.d2ds2, ">", fn.interp.ddz )  # the scalar m / m^2

			remove.files( c(blocked, grids, z.dds, z.d2ds2  ) )
			return ("intermediate files completed")
		}
   
		if ( DS %in% c( "Z.redo", "Z.lonlat", "Z.lonlat.grid", "Z.planar", "Z.planar.grid" ) ) { 

			outdir = project.directory("bathymetry", "interpolated" )
			dir.create( outdir, showWarnings=F, recursive=T )
		
			fn.lonlat = file.path( outdir, paste( p$spatial.domain, "Z.interpolated.lonlat.rdata", sep=".") )
      fn.lonlat.grid = file.path( outdir, paste( p$spatial.domain, "Z.interpolated.lonlat.grid.rdata", sep=".") )
      fn.planar = file.path( outdir, paste(p$spatial.domain, "Z.interpolated.planar.rdata", sep=".") )
      fn.planar.grid = file.path( outdir, paste(p$spatial.domain, "Z.interpolated.planar.grid.rdata", sep=".") )

      if ( DS == "Z.lonlat" ) {
        load( fn.lonlat )
        return( Z )
      }
      if ( DS == "Z.lonlat.grid" ) {   # not used ... drop?
        load( fn.lonlat.grid )
        return( Z )
      }
      if ( DS == "Z.planar" ) {
        load( fn.planar )
        return( Z )
      }
      if ( DS == "Z.planar.grid" ) {    # used by map.substrate
        load( fn.planar.grid )
        return( Z )
      }
  
      Z0 = Z = bathymetry.db( p, DS="Z.gridded" )
      Z$lon = grid.internal( Z$lon, p$lons )
      Z$lat = grid.internal( Z$lat, p$lats )
      Z = block.spatial ( xyz=Z, function.block=block.mean ) 
     
      # create Z in lonlat xyz dataframe format
      save(Z, file=fn.lonlat, compress=T)  

      # create Z in lonlat grid/matrix format
      Z = xyz2grid(Z, p$lons, p$lats)  # using R
      save(Z, file=fn.lonlat.grid, compress=T) # matrix/grid format
      
      # ---- convert to planar coords ...
      # must use the interpolated grid to get even higher resolution "data" with extrapolation on edges 
      Z = lonlat2planar( Z0, proj.type= p$internal.projection )   
      Z = Z[, c("plon", "plat", "z")]
      Z$plon = grid.internal( Z$plon, p$plons )
      Z$plat = grid.internal( Z$plat, p$plats )
  
      gc()
      Z = block.spatial ( xyz=Z, function.block=block.mean ) 
      
      # create Z in planar xyz format 
      save( Z, file=fn.planar, compress=T ) 

      # create Z in matrix/grid format 
      gc()
      Z = xyz2grid( Z, p$plons, p$plats)
      save( Z, file=fn.planar.grid, compress=T ) 
    
      return ("interpolated depths completed")

		} 
    
		if ( DS %in% c( "dZ.redo", "dZ.lonlat", "dZ.lonlat.grid", "dZ.planar", "dZ.planar.grid" ) ) { 
  
			outdir = file.path( project.directory("bathymetry"), "interpolated" )
			dir.create( outdir, showWarnings=F, recursive=T )
	    
      fn.lonlat = file.path( outdir, paste( p$spatial.domain, "dZ.interpolated.lonlat.rdata", sep=".")  )
      fn.lonlat.grid = file.path( outdir, paste( p$spatial.domain, "dZ.interpolated.lonlat.grid.rdata", sep=".")  )
      fn.planar = file.path( outdir, paste( p$spatial.domain, "dZ.interpolated.planar.rdata", sep=".")  )
      fn.planar.grid = file.path( outdir, paste( p$spatial.domain, "dZ.interpolated.planar.grid.rdata", sep=".")  )

      if ( DS == "dZ.lonlat" ) {
        load( fn.lonlat )
        return( dZ )
      }
      if ( DS == "dZ.lonlat.grid" ) {
        load( fn.lonlat.grid )
        return( dZ )
      }
      if ( DS == "dZ.planar" ) {
        load( fn.planar )
        return( dZ )
      }
      if ( DS == "dZ.planar.grid" ) {
        load( fn.planar.grid )
        return( dZ )
      }
  
      dZ0 = bathymetry.db( p, DS="dZ.gridded" )
      dZ0$dZ = log( abs( dZ0$dZ ) )

      dZ = dZ0
      dZ$lon = grid.internal( dZ$lon, p$lons )
      dZ$lat = grid.internal( dZ$lat, p$lats )
      dZ = block.spatial ( xyz=dZ, function.block=block.mean ) 
     
      # create dZ in lonlat xyz dataframe format
      save(dZ, file=fn.lonlat, compress=T)  

      # create dZ in lonlat grid/matrix format
      dZ = xyz2grid(dZ, p$lons, p$lats)
      save(dZ, file=fn.lonlat.grid, compress=T) # matrix/grid format
      
      
      # ---- convert to planar coords ...
      # must use the interpolated grid to get even higher resolution "data" with extrpolation on edges 
      dZ = lonlat2planar( dZ0, proj.type= p$internal.projection )  # utm20, WGS84 (snowcrab geoid) 
      dZ = dZ[, c("plon", "plat", "dZ")]
      dZ$plon = grid.internal( dZ$plon, p$plons )
      dZ$plat = grid.internal( dZ$plat, p$plats )
  
      gc()
      dZ = block.spatial ( xyz=dZ, function.block=block.mean ) 
      
      # create dZ in planar xyz format 
      save( dZ, file=fn.planar, compress=T ) 

      # create dZ in matrix/grid format 
      gc()
      dZ = xyz2grid( dZ, p$plons, p$plats)
      save( dZ, file=fn.planar.grid, compress=T ) 
      
      return ("interpolated files complete, load via another call for the saved files")
    }
    
    if ( DS %in% c( "ddZ.redo", "ddZ.lonlat", "ddZ.planar", "ddZ.lonlat.grid", "ddZ.planar.grid"  ) ) { 
     	outdir = file.path( project.directory("bathymetry"), "interpolated" )
			dir.create( outdir, showWarnings=F, recursive=T )
	  
      fn.lonlat = file.path( outdir,  paste( p$spatial.domain, "ddZ.interpolated.lonlat.rdata", sep=".")  )
      fn.lonlat.grid = file.path( outdir,  paste( p$spatial.domain, "ddZ.interpolated.lonlat.grid.rdata", sep=".")  )
      fn.planar = file.path( outdir,  paste( p$spatial.domain, "ddZ.interpolated.planar.rdata", sep=".")  )
      fn.planar.grid = file.path( outdir,  paste( p$spatial.domain, "ddZ.interpolated.planar.grid.rdata", sep=".") )

      if ( DS == "ddZ.lonlat" ) {
        load( fn.lonlat )
        return( ddZ )
      }
      if ( DS == "ddZ.lonlat.grid" ) {
        load( fn.lonlat.grid )
        return( ddZ )
      }
      if ( DS == "ddZ.planar" ) {
        load( fn.planar )
        return( ddZ )
      }
      if ( DS == "ddZ.planar.grid" ) {
        load( fn.planar.grid )
        return( ddZ )
      }

      ddZ0 = bathymetry.db( p, DS="ddZ.gridded" )
      ddZ0$ddZ = log( abs( ddZ0$ddZ ) )

      # ----- convert to lonlats blocked
      ddZ = ddZ0
      ddZ$lon = grid.internal( ddZ$lon, p$lons )
      ddZ$lat = grid.internal( ddZ$lat, p$lats )
      ddZ = block.spatial ( xyz=ddZ, function.block=block.mean ) 
      
      # lonlat xyz dataframe format
      save(ddZ, file=fn.lonlat, compress=T)  

      ddZ = xyz2grid(ddZ, p$lons, p$lats)
      save(ddZ, file=fn.lonlat.grid, compress=T) # matrix/grid format
          
      # ---- convert to planar coords ...
      # must use the interpolated grid to get even higher resolution "data" with extrpolation on edges 
      ddZ = ddZ0
      ddZ = lonlat2planar( ddZ0, proj.type= p$internal.projection )  # utm20, WGS84 (snowcrab geoid) 
      ddZ = ddZ[, c("plon", "plat", "ddZ")]
      gc()
      ddZ$plon = grid.internal( ddZ$plon, p$plons )
      ddZ$plat = grid.internal( ddZ$plat, p$plats )
   
      ddZ = block.spatial ( xyz=ddZ, function.block=block.mean ) 
      
      # create ddZ in planar xyz format 
      save( ddZ, file=fn.planar, compress=T ) 

      gc()
      ddZ = xyz2grid(ddZ, p$plons, p$plats)
      save( ddZ, file=fn.planar.grid, compress=T ) 
      
      return ("interpolated files complete, load via another call for the saved files")
    }


    if (DS %in% c("baseline", "baseline.redo") ) {
      # form prediction surface in planar coords for SS snowcrab area
      outfile =  file.path( project.directory("bathymetry"), "interpolated", paste( p$spatial.domain, "baseline.interpolated.rdata" , sep=".") )

      if ( DS=="baseline" ) {
        load( outfile )
        return (Z)
      }

	
      # ---------
			
      if ( p$spatial.domain == "canada.east" ) {
     		p = spatial.parameters( type=p$spatial.domain, p=p )
        Z = bathymetry.db( p, DS="Z.planar" )
				Z = Z[ which(Z$z < 1000 & Z$z > 0 ) ,] 
			}

      # ---------
		
			if ( p$spatial.domain =="SSE" ) {
        Z = bathymetry.db( p, DS="Z.planar" )
  		  Z = Z[ which(Z$z < 800 & Z$z > 0 ) ,] 
		  }

      # ---------

			if ( p$spatial.domain == "snowcrab" ) {
 
        # NOTE::: snowcrab baseline == SSE baseline, except it is a subset 
        # begin with the SSE conditions 
        p0 = p 
        p = spatial.parameters( type="SSE", p=p )
        Z = bathymetry.db( p, DS="baseline" )
        p = p0

        kk = which( Z$z < 350 & Z$z > 10  )
	  	  if (length( kk) > 0) Z = Z[ kk, ]
        jj = filter.region.polygon( Z[,c(1:2)], region="cfaall", planar=T,  proj.type=p$internal.projection ) 
        if (length( jj) > 0) Z = Z[ jj, ]
        # filter out area 4X   
        corners = data.frame( cbind( 
          lon = c(-63, -65.5, -56.8, -66.3 ),  
          lat = c( 44.75, 43.8, 47.5, 42.8 )  
        ) )
        corners = lonlat2planar( corners, proj.type=p$internal.projection )
        dd1 = which( Z$plon < corners$plon[1] & Z$plat > corners$plat[1]  ) 
        if (length( dd1) > 0) Z = Z[- dd1, ]
        dd2 = which( Z$plon < corners$plon[2] & Z$plat > corners$plat[2]  ) 
        if (length( dd2) > 0) Z = Z[- dd2, ]
        dd3 = which( Z$plon > corners$plon[3] ) # east lim
        if (length( dd3) > 0) Z = Z[- dd3, ]
        dd4 = which( Z$plon < corners$plon[4] )  #west lim
        if (length( dd4) > 0) Z = Z[- dd4, ]
        dd5 = which( Z$plat > corners$plat[3]  ) # north lim
        if (length( dd5) > 0) Z = Z[- dd5, ]
        dd6 = which( Z$plat < corners$plat[4]  )  #south lim 
        if (length( dd6) > 0) Z = Z[- dd6, ]
         
      }
			
      # require (lattice); levelplot( z~plon+plat, data=Z, aspect="iso")
			
      save (Z, file=outfile, compress=T )

			return( paste( "Baseline data file completed:", outfile )  )
    }
 



    if (DS %in% c( "complete", "complete.redo") ) {
      # form prediction surface in planar coords for SS snowcrab area
      
      outfile =  file.path( project.directory("bathymetry"), "interpolated", paste( p$spatial.domain, "complete.rdata" , sep=".") )
      if (p$spatial.domain == "snowcrab" ) outfile=gsub( p$spatial.domain, "SSE", outfile )

      if ( DS=="complete" ) {
        if (file.exists( outfile) ) load( outfile )
        if (p$spatial.domain == "snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          Z = Z[id,]
        }
        return (Z )
      }
   
      Z = bathymetry.db( p, DS="Z.planar" )
      dZ = bathymetry.db( p, DS="dZ.planar" )
      ddZ = bathymetry.db( p, DS="ddZ.planar" )
      Z = merge( Z, dZ, by=c("plon", "plat"), sort=FALSE )
      Z = merge( Z, ddZ, by=c("plon", "plat"), sort=FALSE )
      save (Z, file=outfile, compress=T )

			return( paste( "Completed:", outfile )  )
    }
 
	

    if (DS %in% c("lookuptable.sse.snowcrab.redo", "lookuptable.sse.snowcrab" )) { 
      # create a lookuptable for SSE -> snowcrab domains
      # both share the same initial domains + resolutions
      fn = file.path( project.directory("bathymetry"), "interpolated", "sse.snowcrab.lookup.rdata") 
      if (DS== "lookuptable.sse.snowcrab" ) { 
        if (file.exists(fn)) load(fn)
        return(id)
      }
      zSSE = bathymetry.db ( p=spatial.parameters( type="SSE" ), DS="baseline" )
      zSSE$id.sse = 1:nrow(zSSE)
      
      zsc  = bathymetry.db ( p=spatial.parameters( type="snowcrab" ), DS="baseline" )
      zsc$id.sc = 1:nrow(zsc)

      z = merge( zSSE, zsc, by =c("plon", "plat"), all.x=T, all.y=T, sort=F )
      ii = which(is.finite(z$id.sc ) & is.finite(z$id.sse )  )
      if (length(ii) != nrow(zsc) ) stop("Error in sse-snowcrab lookup table size")
      id = sort( z$id.sse[ ii] )
      # oo= zSSE[id,] 

      save( id, file=fn, compress=T )
      return(fn)
    }     

  }  






