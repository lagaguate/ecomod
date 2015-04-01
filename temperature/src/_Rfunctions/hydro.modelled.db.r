
  hydro.modelled.db = function( ip=NULL, p, DS, vname, yr=NULL ) {
    
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 


    if (DS %in% c(  "bottom.statistics.annual", "bottom.statistics.annual.redo" )){
      
			tstatdir = project.datadirectory("temperature",  "data", "stats", p$spatial.domain ) 
      dir.create( tstatdir, showWarnings=F, recursive = TRUE )

			if (DS %in% c("bottom.statistics.annual")) {
        O = NULL
        fn = file.path( tstatdir, paste("bottom.statistics.annual",  yr, "rdata", sep=".") )
        if (file.exists( fn) ) load(fn)
        return ( O )
      }
        
      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if ( is.null(ip)) ip = 1:p$nruns

      for ( r in ip ) { 
        y = p$runs[r, "yrs"]
				print ( paste("Year:", y)  )
				
				O = bathymetry.db( p=p, DS="baseline" )
        P = temperature.interpolations( p=p, DS="spatial.interpolation", yr=y  )
     		P[ P < -2 ] = -2  
			  P[ P > 30 ] = 30 
			  ibaddata = which( !is.finite(P) )
				P[ ibaddata ] = mean(P, na.rm=T )
				
				V = temperature.interpolations( p=p, DS="spatial.interpolation.se", yr=y  )
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
				O$tmin = apply( P, 1, quantile, probs=0.005 )
        O$tmax = apply( P, 1, quantile, probs=0.995 )

				W = 1/V^2   # weights: inverse variance, normalised
				W = W / rowSums(W)
				O$tmean = apply( P*W, 1, sum, na.rm=T)

				SS = (P-O$tmean)^2 # sums of squares
				O$tsd  = apply( SS*W, 1, sum, na.rm=T ) # weighted seasonal mean sums of squares

				O$tamplitude = O$tmax- O$tmin  # approximate as sinusoid can span 2 yrs

				# half-period .. also approximate as sinusoid can also span 2 yrs
				# sin tranf required to make circular and then take difference and rescale
        O$thalfperiod = abs( sin(O$wmax/52*pi) - sin(O$wmin/52*pi) ) * 52/pi 
      
        fn =  file.path( tstatdir, paste("bottom.statistics.annual", y, "rdata", sep=".") )
        save( O, file=fn, compress=T )
    
        rm (O, P) ; gc()
      }
      return ("Completed")
    }
 

    if (DS %in% c( "bottom.mean", "bottom.mean.redo" )){
			
			# global / climatological mean across all years
			tstatdir = project.datadirectory("temperature", "data", "stats", p$spatial.domain )
      dir.create( tstatdir, showWarnings=F, recursive = TRUE  )

			if (DS=="bottom.mean") {
				fn = file.path(tstatdir, paste(vname, "rdata", sep=".") )
				P = NULL
				if (file.exists(fn)) load(fn)
				return(P)
			}

      if ( is.null(ip) ) ip = 1:p$nruns
 
			for ( iv in ip ) {
				vn = p$runs[iv, "vname"]
				B = NULL
				for (y in p$tyears ) {
					H = hydro.modelled.db( p=p, DS="bottom.statistics.annual", yr=y ) 
					B = cbind( B, H[, vn] )
				}
				
				P = bathymetry.db( p=p, DS="baseline" )
				P$new = rowMeans(B, na.rm=T)
				P$z = NULL
				names(P) = c("plon", "plat", vn)
				fn = file.path( tstatdir, paste(vn, "rdata", sep=".") )
				save(P, file=fn, compress=T)

			}

      return( "Completed" )
    }





  }



