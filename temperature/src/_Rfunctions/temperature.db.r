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
        if (file.exists(outfile)) load( outfile )
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          PS = PS[ id, ]
        }
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
      outdir =  file.path( project.datadirectory("temperature"), "data", "interpolated", "complete", p$spatial.domain )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.datadirectory("temperature"), "data", "interpolated", "complete", "SSE" )
      dir.create(outdir, recursive=T, showWarnings=F)

      if (DS=="complete") {
        outfile =  file.path( outdir, paste( "PS", year, "rdata", sep= ".") )
        PS = NULL
        if ( file.exists( outfile ) ) load( outfile )
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          PS = PS[ id, ]
        }
        return (PS)
      }

      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (is.null(ip)) ip = 1:p$nruns
      
      # depth is the primary constraint 
      Z = bathymetry.db( p=p, DS="baseline" )  # SS to a depth of 500 m  the default used for all planar SS grids
      Z$id = 1:nrow(Z)
      Z$z = NULL
  
      CL = temperature.db(p=p, DS="climatology")
      names(CL) = c("plon", "plat", "tmean.cl", "tamp.cl", "wmin.cl", "thp.cl", "tsd.cl") 
      
      CL = merge( Z, CL,  by =c("plon", "plat"), all.x=T, all.y=F, sort=F ) ## should not be required but in case ordining get messed up

      for (iy in ip) {
        yr = p$runs[iy, "yrs"]
        print (yr)
        outfile =  file.path( outdir, paste( "PS", yr, "rdata", sep= ".") )
		
        E = hydro.modelled.db( DS="bottom.statistics.annual", p=p, yr=yr  ) 
        names(E)[ which(names(E)=="tamplitude") ] = "tamp"  # fix this at the level of "bottom statistics"
        names(E)[ which(names(E)=="thalfperiod") ] = "thp"

        PS = merge( CL, E,  by =c("plon", "plat"), all.x=T, all.y=F, sort=F)
        
        PS = PS[ order( PS$id), ]
        PS$id = NULL

        save (PS, file=outfile, compress=T )
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
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          P = P[ id, ]
        }
        return ( P )
      }
     	
			if (DS %in% c("spatial.interpolation.se")) {
        V = NULL
				fn2 = file.path( spinterpdir, paste("spatial.interpolation.se",  yr, "rdata", sep=".") )
        if (file.exists( fn2) ) load(fn2)
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          V = V[ id, ]
        }
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
