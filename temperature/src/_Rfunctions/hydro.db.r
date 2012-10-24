

  hydro.db = function( ip=NULL, p=NULL, DS=NULL, yr=NULL, vname=NULL, additional.data=c("groundfish", "snowcrab"), ...) {
    
    # data source is http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/climate_e.htm

    if ( DS %in% c("osd.rawdata", "osd.rawdata.all", "osd.rawdata.all.redo", "osd.rawdata.singleyear", "osd.rawdata.singleyear.redo","osd.rawdata.allfiles.redo" )) {
      # must download manually to this directory and run gzip
      # from choijae; "Jc#00390" :: (http://www.mar.dfo-mpo.gc.ca/science/ocean/database/data_query.html) 
      # depths: 500,500, "complete profile"   .. raw data  for the SS
      # (USER Defined -- region: jc.ss")
      
      loc = file.path( project.directory("temperature"), "data", "climate", "rawdata", p$spatial.domain )
			dir.create( loc, recursive=T, showWarnings=F )
      
      outfilename = file.path( project.directory("temperature"), "data", "climate", paste("climate", p$spatial.domain, "rdata", sep=".") )
			if ( DS == "osd.rawdata.all") {
        load( outfilename) 
        return (data)
      }
     

      if ( DS == "osd.rawdata.all.redo") {
        data = NULL
        for ( y in yr ) {
          print (y)
          data = rbind( data, hydro.db( DS="osd.rawdata", yr=y, p=p ) )
        }
        save(data, file=outfilename, compress=T)
        return ("complete")
      }
    
    
      if ( DS == "osd.rawdata" ) {
        fn = file.path(  project.directory("temperature"), "data", "climate", "annual", p$spatial.domain, paste( "osd.rawdata", yr, "rdata", sep=".") )
        data = NULL
				if (file.exists ( fn ) ) load(fn)
        return (data )
      }


      if ( DS %in% c("osd.rawdata.singleyear.redo", "osd.rawdata.allfiles.redo") ) {  
        outloc = file.path( project.directory("temperature"), "data", "climate", "annual", p$spatial.domain ) 
		  	inloc = file.path( project.directory("temperature"), "data", "climate", "rawdata", p$spatial.domain ) 

				dir.create( outloc, recursive=T, showWarnings=F )
				varlist = c("DEPTH","PRESSURE","CRUISE_DATE","LATITUDE" ,"LONGITUDE" ,"TEMPERATURE" ,"SALINITY" ,"SIGMAT" )
				
				if ( DS=="osd.rawdata.allfiles.redo" ) {
					fn.all = list.files( path=inloc, pattern="osd.clim.*.gz", full.names=T) 
					data = NULL
					for (fn in fn.all) {
						f = read.table( gzfile(fn), header=T, as.is=T, sep=",", na.strings="9999")
						f = f[,varlist] 
						fyears = as.numeric( matrix( unlist( strsplit( f$CRUISE_DATE, "/" ) ), ncol=3, byrow=T) [,3] )
						years = sort( unique( fyears ))
						for (yrs in years) {
							fn.out = file.path( outloc,  paste( "osd.rawdata", yrs, "rdata", sep=".") )
							print( paste(yrs, ":", fn.out) )
							data = f[ which( fyears == yrs) ,]
							names(data) = tolower( names(data) )
							data$date = chron( dates.=data$cruise_date, format=c(dates="d/m/y"), out.format=c(dates="year-m-d")  )
							data$cruise_date = NULL
							save( data, file=fn.out, compress=T) 

						}
					}

				} 
        
        if (DS=="osd.rawdata.singleyear.redo" ) {
					for ( y in yr) {
						data = NULL
						fn.all = list.files( path=inloc, pattern="osd.clim.*.gz", full.names=T) 
						fn = fn.all[ grep (as.character(y), fn.all) ]
						f = read.table( gzfile(fn), header=T, as.is=T, sep=",", na.strings="9999")
						data = f[,varlist]
						 
						fn.out = file.path( outloc, paste( "osd.rawdata", y, "rdata", sep=".") )
						names(data) = tolower( names(data) )
						data$date = chron( dates.=data$cruise_date, format=c(dates="d/m/y"), out.format=c(dates="year-m-d")  )
						data$cruise_date = NULL
						save( data, file=fn.out, compress=T)
					}
				} 
        
        if (DS=="osd.oneoff.singleyear.redo" ) {
					  ## this is a data dump directly from Roger Petipas	
            data = NULL
						fn.all = list.files( path=inloc, pattern="partial.*.gz", full.names=T) 
						fn = fn.all[ grep (as.character(yr), fn.all) ]
						f = read.table( gzfile(fn), header=T, as.is=T, sep=",", na.strings="9999")
						colnames(f) =  c( "cruiseid", "latitude", "longitude", "cruise_date", "time", 
                "pressure", "temperature", "salinity", "sigmat", "stationid" ) 
            f$depth = decibar2depth ( P=f$pressure, lat=f$latitude )
            data = f[,tolower(varlist)]

						fn.out = file.path( outloc, paste( "osd.rawdata", yr, "rdata", sep=".") )
						names(data) = tolower( names(data) )
						data$date = chron( dates.=data$cruise_date, format=c(dates="d/m/y"), out.format=c(dates="year-m-d")  )
						data$cruise_date = NULL
						save( data, file=fn.out, compress=T)
					
				}

        return ("Complete")
      }  
      
         
    }

    # ----------------  

    if (DS %in% c( "profiles.annual.redo", "profiles.annual" ) ) {
      # convert annual depth profiles from raw data into and Rdata file and then extract bottom temperatures 
      
      if (DS=="profiles.annual") {
        fn = file.path(  project.directory("temperature"), "data", "climate", "profiles", p$spatial.domain,
            paste("depthprofiles", yr, "rdata", sep="."))
        Y = NULL
				if (file.exists( fn) ) load (fn )
        return(Y)
      }
 
      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (!is.null(p$env.init)) for( i in p$env.init ) source (i)
      if (is.null(ip)) ip = 1:length(yr)

      # bring in snow crab, groundfish and OSD data ...
      require(chron)
      dir.create( file.path(  project.directory("temperature"), "data", "climate", "profiles", p$spatial.domain ) , recursive=T )

      for (iy in ip) {
        yt = yr[iy]
  
        Y = hydro.db( DS="osd.rawdata", yr=yt, p=p )
          debug = F
          if  (debug) {
            #  add SSE as it seems to contain more data than the canada.east ! check this with OSD people why 
            #  this add together canada.east and SSE and 
            psse = spatial.parameters( type="SSE" )
            ysse = hydro.db( DS="osd.rawdata", yr=yt, p=psse )
            Y = rbind( Y, ysse )
          }
          if ( !is.null(Y) ) {
            Y$id =  paste( Y$longitude, Y$latitude, Y$dayno, sep="~" )
            Y$yr =  as.numeric( as.character( years( Y$date ) ) )
            Y$dayno = convert.datecodes(Y$date, "julian") 
            Y$weekno = ceiling ( Y$dayno / 365 * 52 )
            Y$depth = decibar2depth ( P=Y$pressure, lat=Y$latitude )
            Y$pressure = NULL
            Y$oxyml = NA
            Ynames = names(Y) 
          }

        if ("groundfish" %in% additional.data ) {
					loadfunctions( "groundfish")
          gf = groundfish.db( "gshyd.georef" )
					gf = gf[ which( gf$yr == yt ) , ]
					if (nrow(gf) > 0) {
						gf$sigmat = NA
						names(gf) = c( "id", "depth", "temperature", "salinity", "oxyml", "longitude", "latitude", "yr", "weekno", "dayno", "date", "sigmat"  )
						Y = rbind( Y, gf[, Ynames] )
					}
				}

        if ("snowcrab" %in% additional.data ) {
	        loadfunctions( "snowcrab", functionname="initialise.local.environment.r")
				  
					sn.profiles = try( minilog.db( DS="basedata", Y=yt ), silent=T )
					if ( ! "try-error" %in% class(sn.profiles)  & !is.null( nrow(sn.profiles)) ) {
						sn.profiles = sn.profiles[, c("temperature", "depth", "unique_id") ] 
						sn = snowcrab.db( DS="set.clean" ) 
						sn$unique_id = as.character( sn$minilog_uid )
						sn = sn[, c("chron", "unique_id", "lon", "lat") ]
						sn = sn[ which( !is.na( sn$unique_id) ) , ]

						sn = merge( sn.profiles, sn, by="unique_id", all.x=F, all.y=T, sort=F )
						names(sn) = c("id", "temperature", "depth", "date", "longitude", "latitude" )
						sn$yr =  as.numeric( as.character( years( sn$date ) ) )
						sn$dayno = convert.datecodes(sn$date, "julian") 
						sn$weekno = ceiling ( sn$dayno / 365 * 52 )
						sn$salinity = NA
						sn$sigmat = NA
						sn$oxyml = NA
						sn$temperature = as.numeric( sn$temperature )
						sn$depth = as.numeric( sn$depth )

						Y = rbind( Y, sn[, Ynames] )
					}
				}

        if ( is.null(Y) ) return()
        iiY = which(duplicated(Y))
        
        if (length(iiY)>0) Y = Y [ -iiY, ]

        fn = file.path(  project.directory("temperature"), "data", "climate", "profiles", p$spatial.domain,  
						paste("depthprofiles", yt, "rdata", sep="."))
        print( fn  )
        save( Y, file=fn, compress=T )
      }
      return ("Completed")

    }


    # ----------------  

    if (DS %in% c( "bottom.annual", "bottom.annual.redo" ) ) {
      # extract bottom temperatures 
      
      if (DS=="bottom.annual") {
        fn = file.path( project.directory("temperature"), "data", "climate", "bottom", p$spatial.domain, paste("bottom", yr, "rdata", sep="."))
        Z = NULL
				if (file.exists(fn) ) load (fn )
        return(Z)
      }

      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (!is.null(p$env.init)) for( i in p$env.init ) source (i)
      if (is.null(ip)) ip = 1:length(yr)
      dir.create( file.path(  project.directory("temperature"), "data", "climate", "bottom", p$spatial.domain ) , recursive=T )

      for (iy in ip) {
        yt = yr[iy]
        
        Y = hydro.db( DS="profiles.annual", yr=yt, p=p )
        # Bottom temps
        if (is.null(Y)) next()
				Y$id =  paste( round(Y$longitude,2), round(Y$latitude,2), Y$dayno, sep="~" )
        ids =  sort( unique( Y$id ) )
        res = copy.data.structure( Y)   

        for (i in ids ) {
          Z = Y[ which( Y$id == i ), ]
          jj = which( is.finite( Z$depth ) )
          if ( length(jj) < 3 ) next() 
          zmax = max( Z$depth, na.rm=T ) - 10  # accept any depth within 10 m of the maximum depth
          kk =  which( Z$depth >= zmax ) 
          R = Z[ which.max( Z$depth ) , ]
          
          R$temperature = mean( Z$temperature[kk] , na.rm=T ) 
          R$salinity = mean( Z$salinity[kk] , na.rm=T )
          R$sigmat = mean( Z$sigmat[kk] , na.rm=T )
          R$oxyml = mean( Z$oxyml[kk] , na.rm=T )
          res = rbind( res, R )
        
        }
        
        Z = res
        fn = file.path(  project.directory("temperature"), "data", "climate", "bottom", p$spatial.domain, paste("bottom", yt, "rdata", sep="."))
				print (fn)
        save( Z, file=fn, compress=T)
      }
      return ("Completed")

    }



    # -----------------

    if (DS %in% c( "bottom.gridded", "bottom.gridded.redo" , "bottom.gridded.all", "bottom.gridded.all.redo" )){
      # this is stored locally and not as an archive for speed and flexibility
      #
      if (DS == "bottom.gridded.all" ) {
        outdir =  file.path( project.directory("temperature"), "data", p$spatial.domain, "bottom" )
				fn = file.path( outdir, paste( "bottom.allyears", "rdata", sep="." ) )
        O = NULL
				if (file.exists(fn) ) load(fn)
        return (O)
      }
      
      if (DS == "bottom.gridded.all.redo" ) {
				outdir = file.path( project.directory("temperature"), "data", p$spatial.domain, "bottom" )
				dir.create( outdir, recursive=T, showWarnings=F )
        fn = file.path( outdir, paste( "bottom.allyears", "rdata", sep="." ) )
        O = NULL
        for (y in yr) {
          print (y) 
          On = hydro.db(p=p, DS="bottom.gridded", yr=y) 
					if ( is.null( On) ) next()
					O = rbind( O, On )
        }
        save(O, file=fn, compress=T)
        return ("Complete")
      }


      if (DS == "bottom.gridded" ) {
				fng = file.path(  project.directory("temperature"), "data", p$spatial.domain, "bottom", "annual", 
						paste( "bottom", yr, "rdata", sep="." ) )
        tp = NULL
				if (file.exists(fng) ) load(fng)
        return ( tp )
      }
       
  
      if (DS == "bottom.gridded.redo" ) {
        for (y in yr) {
   		  	outdir = file.path( project.directory("temperature"), "data", p$spatial.domain, "bottom", "annual" )
			 	  dir.create( outdir, recursive=T, showWarnings=F )
          fn = file.path( outdir , paste( "bottom", y, "rdata", sep="." ) )
          print(fn)
          tp = NULL
          pcanada.east = p
          pcanada.east$spatial.domain="canada.east" 
          # must "force" the following to read data from a larger spatial extent 
          # (as this is all that is currently stored) .. can used SSE specific 
          # but that increases storgage and data duplication .. no need
          tp = hydro.db( p=pcanada.east, DS="bottom.annual", yr=y )
         
          if (is.null( tp) ) next()
					tp = rename.df( tp, "longitude", "lon")
          tp = rename.df( tp, "latitude", "lat")
          tp = rename.df( tp, "temperature", "t")
          tp = rename.df( tp, "depth", "z")
          tp$date = NULL
					# tp$depth = NULL
         
          igood = which( tp$lon >= p$corners$lon[1] & tp$lon <= p$corners$lon[2] 
              &  tp$lat >= p$corners$lat[1] & tp$lat <= p$corners$lat[2] )
          tp = tp[igood, ]

          tp = lonlat2planar( tp, proj.type=p$internal.projection )
          
					tp$lon = grid.internal( tp$lon, p$lons )
          tp$lat = grid.internal( tp$lat, p$lats )
        
					tp$plon = grid.internal( tp$plon, p$plons )
          tp$plat = grid.internal( tp$plat, p$plats )
	
					tp = tp[ which( is.finite( tp$lon + tp$lat + tp$plon + tp$plat ) ) , ]
          
					save( tp, file=fn, compress=T)
        }
      }
      return ( "Completed rebuild"  )
      
    }


    if (DS %in% c(  "temporal.interpolation", "temporal.interpolation.se", "temporal.interpolation.redo" )){
         
			# interpolated predictions for only missing data
			
			outdir =  file.path( project.directory("temperature"), "data", p$spatial.domain, "predictions", "temporal" )
      dir.create( outdir, recursive=T, showWarnings=F )
			
			# bigmemory's backingdir does not seem to be working? ... defaulting to home directory
			
			p$fn.tbot = file.path( "interpolated.bigmemory.rdata.tmp" )
			p$fn.tbot.se = file.path( "interpolated.se.bigmemory.rdata.tmp" )

      if (DS %in% c("temporal.interpolation")) {
          fn1 = file.path( outdir, paste( "temporal.interpolation", yr, "rdata", sep=".") )
          if (file.exists( fn1) ) load(fn1)
          return ( tinterp )
      }
      
			if (DS %in% c("temporal.interpolation.se")) {
          fn1 = file.path( outdir, paste( "temporal.interpolation.se", yr, "rdata", sep=".") )
          if (file.exists( fn1) ) load(fn1)
          return ( tinterp.se )
      }

			# require( mgcv ) # for "gam"
			P = bathymetry.db( p=p, DS="baseline" )
      p$nP = nrow(P);	rm(P); gc()
			p$wtimes = 1:52 
			p$nw = length(p$wtimes)
			p$ny = length(yr)
			p$tyears = yr
	
			p$optimizers = c( "nlm", "bfgs", "perf", "optim", "newton",  "nlm.fd") # optimizers for gam
			p$nMin.tbot = 200 # min number of data points req before attempting to model timeseries.
			p$dist.multiplier = c( 1, 2, 3, 4, 5, 7.5, 10 ) # additional distance multipliers to extend search for data
			p$dist.km = 10 # search "radius" (actually a box)

			require( bigmemory )

			nr = p$nP
			nc = p$nw * p$ny
			
			bf1 = basename(p$fn.tbot) 
			bf2 = basename(p$fn.tbot.se) 
			
			df1 = paste(bf1, "desc",sep=".")
			df2 = paste(bf2, "desc",sep=".")

			# backingdirectory location does not seem to work in V 4.2.3 .. defaulting to home dir
			tbot = big.matrix(nrow=nr, ncol=nc, type="double" , init=NA, backingfile=bf1, descriptorfile=df1   )  
			tbot.se = big.matrix(nrow=nr, ncol=nc, type="double", init=NA, backingfile=bf2, descriptorfile=df2  )
			
			p$tbot.desc = describe(tbot)
			p$tbot.se.desc = describe(tbot.se)

			require(snow)
			cl = makeCluster( spec=p$clusters, type="SOCK" )
			ssplt = lapply( clusterSplit( cl, 1:p$nP ), function(i){i} )
			clusterApplyLB( cl, ssplt, temporal.interpolation, p=p )
			stopCluster( cl )


			tbot <- attach.big.matrix( p$tbot.desc )
			tbot.se <- attach.big.matrix( p$tbot.se.desc )

			for ( r in 1:length(p$tyears) ) {
				yt = p$tyears[r]
				fn1 = file.path( outdir, paste( "temporal.interpolation", yt, "rdata", sep=".") )
				fn2 = file.path( outdir, paste( "temporal.interpolation.se", yt, "rdata", sep=".") )
				
				cstart = (r-1) * p$nw 
				col.ranges = cstart + (1:p$nw) 
				tinterp = tbot[,col.ranges]
				tinterp.se = tbot.se[,col.ranges]
				save( tinterp, file=fn1, compress=T) 
				save( tinterp.se, file=fn2, compress=T) 
			}
		
			file.remove( p$fn.tbot , p$fn.tbot.se )
			file.remove( paste( c(p$fn.tbot , p$fn.tbot.se), "desc", sep=".") )


			return ("completed temporal interpolations ")
    }
   
    # --------------------

    if (DS %in% c(  "temporal.interpolation.RAM", "temporal.interpolation.se.RAM", "temporal.interpolation.redo.RAM" )){
         
			# interpolated predictions for only missing data
			
			outdir =  file.path( project.directory("temperature"), "data", p$spatial.domain, "predictions", "temporal" )
      dir.create( outdir, recursive=T, showWarnings=F )
			
      if (DS %in% c("temporal.interpolation.RAM")) {
          fn1 = file.path( outdir, paste( "temporal.interpolation", yr, "rdata", sep=".") )
          if (file.exists( fn1) ) load(fn1)
          return ( tinterp )
      }
      
			if (DS %in% c("temporal.interpolation.se.RAM")) {
          fn1 = file.path( outdir, paste( "temporal.interpolation.se", yr, "rdata", sep=".") )
          if (file.exists( fn1) ) load(fn1)
          return ( tinterp.se )
      }

			# require( mgcv ) # for "gam"
			P = bathymetry.db( p=p, DS="baseline" )
      p$nP = nrow(P);	rm(P); gc()
			p$wtimes = 1:52 
			p$nw = length(p$wtimes)
			p$ny = length(yr)
			p$tyears = yr
	
			p$optimizers = c( "perf", "bfgs", "nlm", "optim", "newton",  "nlm.fd") # optimizers for gam
			p$nMin.tbot = 200 # min number of data points req before attempting to model timeseries.
			p$dist.multiplier = c( 1, 2, 3, 4, 5, 7.5, 10 ) # additional distance multipliers to extend search for data
			p$dist.km = 10 # search "radius" (actually a box)

			require( bigmemory )

			nr = p$nP
			nc = p$nw * p$ny
			
			tbot = big.matrix(nrow=nr, ncol=nc, type="double" , init=NA )  
			tbot.se = big.matrix(nrow=nr, ncol=nc, type="double", init=NA )
			
      p$tbot.desc = describe(tbot)
			p$tbot.se.desc = describe(tbot.se)


			require(snow)
			cl = makeCluster( spec=p$clusters, type="SOCK" )
			ssplt = lapply( clusterSplit( cl, 1:p$nP ), function(i){i} )
			clusterApplyLB( cl, ssplt, temporal.interpolation.RAM, p=p )
			stopCluster( cl )

			tbot <- attach.big.matrix( p$tbot.desc )
			tbot.se <- attach.big.matrix( p$tbot.se.desc )

			for ( r in 1:length(p$tyears) ) {
				yt = p$tyears[r]
				fn1 = file.path( outdir, paste( "temporal.interpolation", yt, "rdata", sep=".") )
				fn2 = file.path( outdir, paste( "temporal.interpolation.se", yt, "rdata", sep=".") )
				
				cstart = (r-1) * p$nw 
				col.ranges = cstart + (1:p$nw) 
				tinterp = tbot[,col.ranges]
				tinterp.se = tbot.se[,col.ranges]
				save( tinterp, file=fn1, compress=T) 
				save( tinterp.se, file=fn2, compress=T) 
			}
		
			file.remove( p$fn.tbot , p$fn.tbot.se )
			file.remove( paste( c(p$fn.tbot , p$fn.tbot.se), "desc", sep=".") )

			return ("completed temporal interpolations ")
    }
    



		# -------------------



    if (DS %in% c(  "spatial.interpolation", "spatial.interpolation.se", "spatial.interpolation.redo" )){
			
			# interpolated predictions over only missing data
			outdir =  file.path( project.directory("temperature"), "data", p$spatial.domain, "predictions", "spatial" )
			if (p$spatial.domain=="snowcrab") {
        outdir = file.path( project.directory("temperature"), "data", "SSE", "predictions", "spatial" )
      }
    
			if (DS %in% c("spatial.interpolation")) {
        P = NULL
        fn1 = file.path( outdir,paste("spatial.interpolation",  yr, "rdata", sep=".") )
        if (file.exists( fn1) ) load(fn1)
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          P = P[ id, ]
        }
        return ( P )
      }
     	
			if (DS %in% c("spatial.interpolation.se")) {
        V = NULL
				fn2 = file.path( outdir,paste("spatial.interpolation.se",  yr, "rdata", sep=".") )
        if (file.exists( fn2) ) load(fn2)
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          V = V[ id, ]
        }
        return ( V )
      }
         
      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (!is.null(p$env.init)) for( i in p$env.init ) source (i)
      if (is.null(ip)) ip = 1:length(yr)

      require( gstat )
			require (mgcv )

      for ( r in ip ) { 
        y = yr[r]
 
        P = hydro.db( p=p, DS="temporal.interpolation", yr=y  )
        P[ P < -1.9 ] = NA
        P[ P > 34 ] = NA
							
        V = hydro.db( p=p, DS="temporal.interpolation.se", yr=y  )
				V[ V < 0.1 ] = 100  # shrink weighting of unreasonably small SEs
				W = 1 / V^2 
 
        O = bathymetry.db( p=p, DS="baseline" )
				print ( paste("Year:", y)  )
        for ( ww in 1:52 ) {
          print ( paste( "Week:", ww) )
          todo = 1
          while ( todo > 0 )  {
            ai = which(is.finite(P[,ww]))
            aj = setdiff( 1:nrow(P), ai)
						pp = pp.se = NULL

            # inverse distance weighted interpolation (power = 0.5) to max dist of 10 km
            gs = gstat( id="t", formula=P[ai,ww]~1 , locations=~plon+plat, data=O[ai,], nmax=100, maxdist=25, set=list(idp=.5), weights=W[ai,ww]) 
						preds = predict( object=gs, newdata=O[aj,]  ) 

            P[aj,ww] = preds[,3]
						V[aj,ww] = sqrt( V[aj,ww]^2 + preds[,4]^2 )     # assume additive error 
						
						rm( preds, gs ); gc()
            
						ai = which(is.finite( P[,ww] ))
            aj = setdiff( 1:nrow(P), ai )
            last = todo
            todo = length( aj )
            if (todo == last) break  # converged
          } 
          rm ( ai, aj, pp, pp.se ); gc()
        }
       
				fn1 = file.path( outdir,paste("spatial.interpolation",  y, "rdata", sep=".") )
				fn2 = file.path( outdir,paste("spatial.interpolation.se",  y, "rdata", sep=".") )
				save( P, file=fn1, compress=T )
				save( V, file=fn2, compress=T )
 
			}
      return ("Completed")
    }
 

    if (DS %in% c(  "bottom.statistics.annual", "bottom.statistics.annual.redo" )){
      
			outdir = file.path(project.directory("temperature"),  "data", p$spatial.domain, "stats" ) 
      dir.create( outdir, showWarnings=F )

			if (DS %in% c("bottom.statistics.annual")) {
        O = NULL
        fn = file.path( outdir, paste("bottom.statistics.annual",  yr, "rdata", sep=".") )
        if (file.exists( fn) ) load(fn)
        return ( O )
      }
        
      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (!is.null(p$env.init)) for( i in p$env.init ) source (i)
      if (is.null(ip)) ip = 1:length(yr)
 
      require( gstat )

      for ( r in ip ) { 
        y = yr[r]
				print ( paste("Year:", y)  )
				
				O = bathymetry.db( p=p, DS="baseline" )
        P = hydro.db( p=p, DS="spatial.interpolation", yr=y  )
     		P[ P < -2 ] = -2  # shrink weighting of unreasonably small SEs
			  P[ P > 30 ] = 30 
			  ibaddata = which( !is.finite(P) )
				P[ ibaddata ] = mean(P, na.rm=T )
				
				V = hydro.db( p=p, DS="spatial.interpolation.se", yr=y  )
				V[ V < 0.1 ] = 100  # shrink weighting of unreasonably small SEs
			  V[ which( !is.finite(V)) ] = 1000 # "
				V[ ibaddata ] = 10000 # " smaller still

        O$yr = y
        O$wmin = NA
        O$wmax = NA
        O$tmin = NA
        O$tmax = NA
        O$tsd = NA
        O$tmean = NA
        O$tamplitude = NA
        O$thalfperiod = NA
        
        O$wmin = apply( P, 1, which.min )
        O$wmax = apply( P, 1, which.max )
				O$tmin = apply( P, 1, min )
        O$tmax = apply( P, 1, max )

				W = 1/V^2   # weights: inverse variance, normalised
				W = W / rowSums(W)
				O$tmean = apply( P*W, 1, sum, na.rm=T)

				SS = (P-O$tmean)^2 # sums of squares
				O$tsd  = apply( SS*W, 1, sum, na.rm=T ) # weighted seasonal mean sums of squares

				O$tamplitude = O$tmax- O$tmin  # approximate as sinusoid can span 2 yrs
				
				# half-period .. also approximate as sinusoid can also span 2 yrs
				# sin tranf required to make circular and then take difference and rescale
        O$thalfperiod = abs( sin(O$wmax/52*pi) - sin(O$wmin/52*pi) ) * 52/pi 
      
        fn =  file.path( outdir, paste("bottom.statistics.annual", y, "rdata", sep=".") )
        save( O, file=fn, compress=T )
    
        rm (O, P) ; gc()
      }
      return ("Completed")
    }
 

    if (DS %in% c( "bottom.mean", "bottom.mean.redo" )){
			
			# take a mean across all years
			 
			if (DS=="bottom.mean") {
				fn = file.path( project.directory("temperature"), "data", p$spatial.domain, "predictions", paste(vname, "rdata", sep=".") )
				P = NULL
				if (file.exists(fn)) load(fn)
				return(P)
			}

			if (!is.null(p$env.init)) for( i in p$env.init ) source (i)
      if (is.null(ip)) ip = 1:length(vname)
 
			for ( iv in ip ) {
				vn = vname[iv]
				B = NULL
				for (y in yr ) {
					H = hydro.db( p=p, DS="bottom.statistics.annual", yr=y ) 
					B = cbind( B, H[, vn] )
				}
				
				P = bathymetry.db( p=p, DS="baseline" )
				P$new = rowMeans(B, na.rm=T)
				P$z = NULL
				names(P) = c("plon", "plat", vn)
				fn = file.path( project.directory("temperature"), "data", p$spatial.domain, "predictions", paste(vn, "rdata", sep=".") )
				save(P, file=fn, compress=T)

			}

      return( "Completed" )
    }

    }
  

