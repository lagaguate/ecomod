
  temperature.interpolations = function( ip=NULL, p=NULL, DS=NULL, yr=NULL) {

    if (DS %in% c(  "temporal.interpolation", "temporal.interpolation.se", "temporal.interpolation.redo" )){
      
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
		  	
      tbot <- attach.big.matrix( p$descriptorfile.tbot  )
			tbot.se <- attach.big.matrix( p$descriptorfile.tbotse  )
      
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
 

		# -------------------


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

      O = bathymetry.db( p=p, DS="baseline" )
      O$z = NULL

      if ( is.null(ip) ) ip = 1:p$nruns
      for ( r in ip ) { 
        y = p$runs[r, "yrs"]
        P = temperature.interpolations( p=p, DS="temporal.interpolation", yr=y  )
        V = temperature.interpolations( p=p, DS="temporal.interpolation.se", yr=y  )
        # real data are set at se=0  .. they are real zero values
        TRv = quantile( V, probs=c(0.005, 0.995), na.rm=TRUE  )   
        V[ V < TRv[1] ] = TRv[1] 
        V[ V > TRv[2] ] = TRv[2] 
        W = 1 / V^2 
 
				print ( paste("Year:", y)  )
        for ( ww in 1:52 ) {
          print ( paste( "Week:", ww) )
          ai = which(is.finite(P[,ww]))
          Tdat = P[ai,ww]
          gs = NULL
          for ( distance in p$dist.km ) {
            # inverse distance weighted interpolation (power = 0.5) to max dist of 10 km
            gs = try( 
              gstat( id="t", formula=Tdat~1, locations=~plon+plat, data=O[ai,], 
                     maxdist=distance, set=list(idp=.5), weights=W[ai,ww])
              , silent=TRUE ) 

            if ( ! ( "try-error" %in% class(gs) ) ) break() 
          }

          if ( ( "try-error" %in% class(gs) ) ) {
              # last try drop weights with all data .. max distance
              gs = try( 
                gstat( id="t", formula=Tdat~1, locations=~plon+plat, data=O[ai,], 
                     maxdist=distance, set=list(idp=.5) )
                , silent=TRUE ) 
          }
 
          if ( "try-error" %in% class(gs) )  next()  # give up

          count = 0
          todo = 1
          aj = which( ! is.finite(P[,ww]) )
      
          TR = quantile(  P[,ww], probs=c(0.005, 0.995), na.rm=TRUE )
          TR[1] = max( TR[1], -3)
          TR[2] = min( TR[2], 30)
          
          while ( todo > 0 )  {
            preds = predict( object=gs, newdata=O[aj,]  )
            extrapolated1 = which( preds[,3] < TR[1] )
            extrapolated2 = which( preds[,3] > TR[2] )
            if (length( extrapolated1 ) > 0 ) preds[ extrapolated1, 3] = TR[1]
            if (length( extrapolated2 ) > 0 ) preds[ extrapolated2, 3] = TR[2]
            P[aj,ww] = preds[,3]
						V[aj,ww] = sqrt( V[aj,ww]^2 + preds[,4]^2 )     # assume additive error 
            aj = which( ! is.finite(P[,ww]) )
            last = todo
            todo = length( aj )
            count = count + 1
            if ( (todo == last) | (count > 10) ) {
              # stuck in a loop or converged.. take a global mean 
              if (todo > 0) {
                P[aj,ww] = median( P[,ww], na.rm=TRUE )
                V[aj,ww] = median( V[,ww], na.rm=TRUE )
              }
              break() 
            }
          } 
          rm ( aj ); gc()
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

