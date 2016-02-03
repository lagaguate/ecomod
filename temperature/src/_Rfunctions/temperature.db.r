  temperature.db = function ( ip=NULL, year=NULL, p, DS, yr=NULL ) {
    
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
  
    # ------

    if (DS %in% "bigmemory.filenames" ) {
      p$tmp.datadir = file.path( p$project.root, "tmp" )
      if( !file.exists(p$tmp.datadir)) dir.create( p$tmp.datadir, recursive=TRUE, showWarnings=FALSE )
      p$backingfile.tbot = paste( "tbot.bigmatrix", p$spatial.domain, "tmp", sep=".")
      p$backingfile.tbotse = paste( "tbotse.bigmatrix", p$spatial.domain, "tmp", sep=".")
      return(p)
    }

    # ------

    if (DS %in% "bigmemory.cleanup" ) { 
      # not used .. here for reference for other projects
      # load bigmemory data objects pointers
      p = temperature.db( p=p, DS="bigmemory.filenames" ) 
      todelete = file.path( p$tmp.datadir,c( p$backingfile.tbot, p$backingfile.tbotse )) 
      for (fn in todelete ) if (file.exists(fn)) file.remove(fn) 
      return( todelete )
    }

    # ------

    if (DS %in% "bigmemory.initiate" ) { 
      p = temperature.db( p=p, DS="bigmemory.filenames" ) 
    # create file backed bigmemory objects
      # fn.tbot = file.path(p$tmp.datadir, p$backingfile.tbot )
      # if ( file.exists( fn.tbot) ) file.remove( fn.tbot) 
      # fn.tbotse = file.path(p$tmp.datadir, p$backingfile.tbotse )
      # if ( file.exists( fn.tbotse) ) file.remove( fn.tbotse ) 
      nr = p$nP
      nc = p$nw*p$ny
      # shared RAM object
      tbot = big.matrix(nrow=nr, ncol=nc, type="double" , shared=TRUE)  
      tbot.se = big.matrix(nrow=nr, ncol=nc, type="double", shared=TRUE)
      p$descriptorfile.tbot = bigmemory::describe( tbot)
      p$descriptorfile.tbotse = bigmemory::describe( tbot.se)
      return( p ) 
    }

    #  -------------

    if ( DS %in% "bigmemory.status" ) { 
        # not used .. here for reference for other projects
        tbot = bigmemory::attach.big.matrix(p$descriptorfile.tbot  )
        # problematic and/or no data (e.g., land, etc.) and skipped
        i = which( is.nan( tbot[, 1] ) )
        # not yet completed
        j = which( is.na( tbot[,1] ) ) 
        # completed 
        k = which( is.finite (tbot[,1])  ) # not yet done
        return( list(problematic=i, incomplete=j, completed=k, n.total=nrow(tbot[]), 
                     n.incomplete=length(j), n.problematic=length(i), n.complete=length(k)) ) 
      }


    # -----------------

    if (DS %in% c("climatology", "climatology.redo") ) {
      
      # form a basic prediction surface in planar coords for SS habitat for 
      # factors that do not "change" rapidly and 
      # e.g. climatological means of temperature characteristics, substrate, bathymetry
    
      outdir = file.path( project.datadirectory("temperature"), "data", "interpolated" )
      dir.create(outdir, recursive=T, showWarnings=F)
      
      outfile =  file.path( outdir, paste("PS.climatology", p$spatial.domain, "rdata", sep="." ) )
      if ( p$spatial.domain =="snowcrab" ) outfile = file.path( outdir, paste("PS.climatology", "ESS", "rdata", sep="." ) )

      if ( DS=="climatology" ) {
        PS = NULL
        if (file.exists(outfile)) load( outfile )
        return (PS)
      }
     
			# depth is the primary constraint 
      Z = bathymetry.db( p=p, DS="baseline" )  # SS to a depth of 500 m  the default used for all planar SS grids
      Z$id = 1:nrow(Z)
      Z$z = NULL

      E.tmean = hydro.modelled.db( p=p, DS="bottom.mean", vname="tmean" ) 
      names(E.tmean) = c("plon", "plat", "tmean")
      PS = merge( Z, E.tmean, by  =c("plon", "plat"), all.x=T, all.y=F, sort=F )
      rm ( Z, E.tmean ) ; gc()

      E.tamp  = hydro.modelled.db( p=p, DS="bottom.mean", vname="tamplitude"  ) 
      names(E.tamp) = c("plon", "plat", "tamp")
      PS = merge( PS, E.tamp, by  =c("plon", "plat"), all.x=T, all.y=F, sort=F )
      rm ( E.tamp ) ; gc()

      E.wmin  = hydro.modelled.db( p=p,  DS="bottom.mean", vname="wmin"  ) 
      names(E.wmin) = c("plon", "plat", "wmin")
      PS = merge( PS, E.wmin, by  =c("plon", "plat"), all.x=T, all.y=F, sort=F )
      rm ( E.wmin ) ; gc()

      E.thp   = hydro.modelled.db( p=p,  DS="bottom.mean", vname="thalfperiod" ) 
      names(E.thp) = c("plon", "plat", "thp")
      PS = merge( PS, E.thp, by  =c("plon", "plat"), all.x=T, all.y=F, sort=F )
      rm ( E.thp ) ; gc()

      E.tsd   = hydro.modelled.db( p=p,  DS="bottom.mean", vname="tsd" ) 
      names(E.tsd) = c("plon", "plat", "tsd")
      PS = merge( PS, E.tsd, by  =c("plon", "plat"), all.x=T, all.y=F, sort=F )
      rm ( E.tsd ) ; gc()

      print( "Interpolating missing data with inverse-distance weighted means" )
        vars = setdiff( names(PS), c("plon", "plat", "id") )
        require (gstat)
        for (v in vars) {
          print(v)
          for (dists in p$interpolation.distances) { 
            ii = which ( !is.finite( PS[, v]) )
            if (length(ii)==0) break()
            print( length(ii) )
            gs = gstat( id=v, formula=PS[-ii,v]~1, locations=~plon+plat, data=PS[-ii,], 
                nmax=p$interpolation.nmax, maxdist=dists, set=list(idp=.5)) 
            PS[ii,v] = predict( object=gs, newdata=PS[ii,] ) [,3]
        }}
     
      PS = PS[ order( PS$id), ]
      PS$id = NULL

      save (PS, file=outfile, compress=T )
      return( outfile )
    }


    # -------------------------------


    if (DS %in% c("complete", "complete.redo" )) {
      ### a conveniance data table to reduce number of merges occuring during modelling steps
      ### annual stats and climatology are merged together  
      ### essentially the base level data set for habitat db but needed at a lower level as it is used for the other indicators
      
      if (DS=="complete") {
        PS = NULL
        outdir =  file.path( project.datadirectory("temperature"), "data", "interpolated", "complete", p$spatial.domain )
        outfile =  file.path( outdir, paste( "PS", year, "rdata", sep= ".") )
        if ( file.exists( outfile ) ) load( outfile )
        return(PS)
      }

      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (is.null(ip)) ip = 1:p$nruns
      
      print ( "Completing and downscaling data where necessary ..." )

      # default domain climatology
      p0 = spatial.parameters( type=p$spatial.domain.default )
      Z0 = matrix( NA, nrow=p0$nplons, ncol=p0$nplats)
      PS0 = bathymetry.db ( p=p0, DS="baseline" )
      PS0$id =1:nrow(PS0)
      CL = temperature.db(p=p0, DS="climatology")
      CL$plon = CL$plat = CL$z = NULL
      names(CL)[ which(names(CL)=="tmean") ] = "tmean.cl"  
      names(CL)[ which(names(CL)=="tamplitude") ] = "tamp.cl"  
      names(CL)[ which(names(CL)=="wmin") ] = "wmin.cl"  
      names(CL)[ which(names(CL)=="thalfperiod") ] = "thp.cl"  
      names(CL)[ which(names(CL)=="tsd") ] = "tsd.cl"  
      PS0 = merge( PS0, CL,  by =c("id"), all.x=T, all.y=F, sort=F ) 
      PS0_m = cbind( (PS0$plon-p0$plons[1])/p0$pres + 1, (PS0$plat-p0$plats[1])/p0$pres + 1) # row, col indices in matrix form
      rm (CL); gc()
      
      for (iy in ip) {
        yr = p$runs[iy, "yrs"]
        print (paste( yr))
	      
        # default domain annual stats
        E = hydro.modelled.db( DS="bottom.statistics.annual", p=p0, yr=yr  ) 
        E$z = NULL
        if (is.null(E)) print( paste( "bottom.statistics.annual not found for:" , yr ) )
        names(E)[ which(names(E)=="tamplitude") ] = "tamp"  # fix this at the level of "bottom statistics"
        names(E)[ which(names(E)=="thalfperiod") ] = "thp"
        PS0 = merge( PS0, E,  by =c("plon", "plat"), all.x=T, all.y=F, sort=F)
        PS0 = PS0[ order( PS0$id), ]
        
        for (gr in  unique( c(p$spatial.domain.default, p$subregions)) ) {
          print (gr)
          if ( gr == p$spatial.domain.default ) {
            PS = PS0
            p1 = p
          } else {
            # ( gr != p$spatial.domain.default ) {
            # down scale data to alternate grids
            p1 = spatial.parameters( type=gr )
            
            # sub-domain grid
            PS = bathymetry.db ( p=p1, DS="baseline" )
            PS = planar2lonlat( PS, proj.type=p1$internal.projection )  # convert new locations to lon lat
            PS$yr = yr
            PS$plon0 = PS$plon
            PS$plat0 = PS$plat
            PS = lonlat2planar( PS, proj.type=p0$internal.projection )  # convert lon lat to coord system of p0
            locsout = PS[, c("plon", "plat")]
            p0$wgts = fields::setup.image.smooth( nrow=p0$nplons, ncol=p0$nplats, dx=p0$pres, dy=p0$pres, 
                  theta=p$theta, xwidth=p$nsd*p$theta, ywidth=p$nsd*p$theta )
            vn = setdiff( names(PS0), c("plon", "plat", "z" , "yr" ) ) 
            for ( ww in vn ) {
              Z = Z0
              Z[PS0_m] = PS0[,ww]
              # simple linear interpolations 
              is = fields::interp.surface( list( x=p0$plons, y=p0$plats, z=Z), loc=locsout )
              ii = which( is.na( is) ) 
              if ( length( ii)> 0 ) {
                # smoothed surface ..but fast!  mostly edges such as coastlines ..
                kd = try( fields::image.smooth( Z, dx=p0$pres, dy=p0$pres, wght=p0$wgts )$z  )
                if ( ! (class(kd) %in% "try-error") ) {
                  is[ii] = fields::interp.surface( list( x=p0$plons, y=p0$plats, z=kd), loc=locsout[ii,] )
                }
              } 
              jj = which( is.na( is) ) 
              if ( length( jj)> 0 ) is[jj] = median( PS0[,ww], na.rm=TRUE )
              PS[,ww] = is 
            }
            
            # return to coordinate system of original projection
            PS$plon = PS$plon0
            PS$plat = PS$plat0
            PS = PS[ , names(PS0) ]
          }
          PS$id = NULL
          outdir =  file.path( project.datadirectory("temperature"), "data", "interpolated", "complete", p1$spatial.domain )
          outfile =  file.path( outdir, paste( "PS", yr, "rdata", sep= ".") )
          dir.create(outdir, recursive=T, showWarnings=F)
          save (PS, file=outfile, compress=T )
        }
      }
      return( outdir )
    }

    # -----------------------
    
    if ( DS %in% c( "temporal.interpolation", "temporal.interpolation.se", "temporal.interpolation.redo" ) ) {
      # interpolations complete ... now write time slices to disk 
      tinterpdir = project.datadirectory("temperature", "data", "interpolated", "temporal", p$spatial.domain  )
      dir.create( tinterpdir, recursive=T, showWarnings=F )
      if (DS %in% c("temporal.interpolation")) {
          fn1 = file.path( tinterpdir, paste( "temporal.interpolation", yr, "rdata", sep=".") )
          if (file.exists( fn1) ) load(fn1)
          return ( tinterp )
      }
			if (DS %in% c("temporal.interpolation.se")) {
          fn1 = file.path( tinterpdir, paste( "temporal.interpolation.se", yr, "rdata", sep=".") )
          if (file.exists( fn1) ) load(fn1)
          return ( tinterp.se )
      }
      tb <- bigmemory::attach.big.matrix( p$descriptorfile.tbot  )
			tb.se <- bigmemory::attach.big.matrix( p$descriptorfile.tbotse  )
	  
      # copy
      tbot = tb[]
      tbot.se = tb.se[]
      # reject unreasonable extremes 
      bad = which( tbot < -3 | tbot > 25  )
      if (length( bad) > 0) {
        tbot[ bad] = NA
        tbot.se[bad] = NA
      }
      
      # global quantile removal: 99.9 % prob
      tq = quantile( tbot, probs=c(0.0005, 0.9995), na.rm=TRUE  )   # in 2015: -2.46276 21.60798
      bad = which( tbot < tq[1] | tbot > tq[2] )
      if (length( bad) > 0) {
        tbot[ bad] = NA
        tbot.se[bad] = NA
      }

      tr = quantile( tbot.se, probs=c(0.0005, 0.9995), na.rm=TRUE  )   # 0.0802125 11.3508923
      bad = which( tbot.se < tr[1] | tbot.se > tr[2] )
      if (length( bad) > 0) {
        tbot[ bad] = NA
        tbot.se[bad] = NA
      }

      for ( r in 1:length(p$tyears) ) {
				yt = p$tyears[r]
				fn1 = file.path( tinterpdir, paste( "temporal.interpolation", yt, "rdata", sep=".") )
				fn2 = file.path( tinterpdir, paste( "temporal.interpolation.se", yt, "rdata", sep=".") )
        print( fn1 )
				cstart = (r-1) * p$nw 
				col.ranges = cstart + (1:p$nw) 
				tinterp = tbot[,col.ranges]
				tinterp.se = tbot.se[,col.ranges]
				save( tinterp, file=fn1, compress=T) 
				save( tinterp.se, file=fn2, compress=T) 
			}
			return ( "complete" )
    }
  
    # -----------------------
    
    if (DS %in% c(  "spatial.interpolation", "spatial.interpolation.se", "spatial.interpolation.redo" )){
			
      starttime = Sys.time()
        
      if ( exists("init.files", p) ) LoadFiles( p$init.files ) 
      if ( exists("libs", p) ) RLibrary( p$libs ) 
     
			# interpolated predictions over only missing data
			spinterpdir =  file.path( project.datadirectory("temperature"), "data", "interpolated", "spatial", p$spatial.domain )
			if (p$spatial.domain=="snowcrab") {
        spinterpdir = file.path( project.datadirectory("temperature"), "data", "interpolated", "spatial", "SSE" )
      }
  
      dir.create( spinterpdir, recursive=T, showWarnings=F )
	 
			if (DS %in% c("spatial.interpolation")) {
        P = NULL
        fn1 = file.path( spinterpdir, paste("spatial.interpolation",  yr, "rdata", sep=".") )
        if (file.exists( fn1) ) load(fn1)
        return ( P )
      }
     	
			if (DS %in% c("spatial.interpolation.se")) {
        V = NULL
				fn2 = file.path( spinterpdir, paste("spatial.interpolation.se",  yr, "rdata", sep=".") )
        V =NULL
        if (file.exists( fn2) ) load(fn2)
        return ( V )
      }

      if ( is.null(ip) ) ip = 1:p$nruns
      O = bathymetry.db( p=p, DS="baseline" )
      O$z = NULL
      if (p$spmethod == "kernel.density" ) {
        # pre-compute a few things rather than doing it for each iteration
        p$wgts = fields::setup.image.smooth(nrow=p$nplons, ncol=p$nplats, dx=p$pres, dy=p$pres, 
          theta=p$theta, xwidth=p$nsd*p$theta, ywidth=p$nsd*p$theta )
        p$O2M = cbind( (O$plon-p$plons[1])/p$pres + 1, (O$plat-p$plats[1])/p$pres + 1) # row, col indices in matrix form
      }
     
      if (p$spmethod == "gam" ) {
        p$O = O
      }
      
      rm(O); gc()

      for ( r in ip ) { 
        y = p$runs[r, "yrs"]
        P = temperature.db( p=p, DS="temporal.interpolation", yr=y  )
        V = temperature.db( p=p, DS="temporal.interpolation.se", yr=y  )
				
        print ( paste("Year:", y)  )
        for ( ww in 1:52 ) {
          print ( paste( "Week:", ww) )
          # these are simple interpolations 
          P[,ww] = temperature.spatial.interpolate( method=p$spmethod, p=p, z=P[,ww] )
          V[,ww] = temperature.spatial.interpolate( method=p$spmethod, p=p, z=V[,ww] )
        }
			
        # reject unreasonable extremes 
        bad = which( P < -3 | P > 25  )
        if (length( bad) > 0) {
          P[ bad] = NA
          V[bad] = NA
        }
        
        # annual quantile removal: 99% prob
        tq = quantile( P, probs=c(0.0005, 0.9995), na.rm=TRUE  )   
        bad = which( P < tq[1] | P > tq[2] )
        if (length( bad) > 0) {
          P[bad] = NA
          V[bad] = NA
        }

        tr = quantile( V, probs=c(0.0005, 0.9995), na.rm=TRUE  )   
        bad = which( V < tr[1] | V > tr[2] )
        if (length( bad) > 0) {
          P[bad] = NA
          V[bad] = NA
        }
		
        fn1 = file.path( spinterpdir,paste("spatial.interpolation",  y, "rdata", sep=".") )
				fn2 = file.path( spinterpdir,paste("spatial.interpolation.se",  y, "rdata", sep=".") )
				save( P, file=fn1, compress=T )
				save( V, file=fn2, compress=T )
			}
      endtime = Sys.time()
      return (endtime - starttime)
    }
    
  }
