
  temperature.interpolations = function( ip=NULL, p=NULL, DS=NULL, yr=NULL) {
    
    if (exists( "init.files", p)) loadfilelist( p$init.files ) 
    if (exists( "libs", p)) loadlibraries( p$libs ) 
 

    if (DS %in% c(  "temporal.interpolation", "temporal.interpolation.se", "temporal.interpolation.redo" )){
         
			# interpolated predictions for only missing data
		
      tinterpdir = project.directory("temperature", "data", "interpolated", "temporal", p$spatial.domain  )
      dir.create( tinterpdir, recursive=T, showWarnings=F )
			
			# bigmemory's backingdir does not seem to be working? ... defaulting to home directory
		
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

 
			nr = p$nP
			nc = p$nw * p$ny

      if (p$use.bigmemory.file.backing) {
      
        basenm = file.path( make.random.string("interpolated.bigmemory.rdata.tmp") )
        p$fn.tbot =  paste( basenm, "pred", sep="." )
        p$fn.tbot.se = paste( basenm, "se", sep="." )
        bf1 = basename(p$fn.tbot) 
        bf2 = basename(p$fn.tbot.se) 
        df1 = paste(bf1, "desc",sep=".")
        df2 = paste(bf2, "desc",sep=".")
        tbot = big.matrix(nrow=nr, ncol=nc, type="double" , init=NA,   backingfile=bf1, descriptorfile=df1   )  
        tbot.se = big.matrix(nrow=nr, ncol=nc, type="double", init=NA, backingfile=bf2, descriptorfile=df2  )

      } else {
      
        tbot = big.matrix(nrow=nr, ncol=nc, type="double" , init=NA, shared=TRUE  )  
        tbot.se = big.matrix(nrow=nr, ncol=nc, type="double", init=NA, shared=TRUE  )
      
      }

      # required to operate with bigmemory objects in parallel 
      p$tbot.desc = describe(tbot)
      p$tbot.se.desc = describe(tbot.se)

      
      if (length( p$clusters) == 1 ) {
        temporal.interpolation ( p=p )
      } else {
        parallel.run( clusters=p$clusters, n=p$nruns, temporal.interpolation, p=p )
      }

			tbot <- attach.big.matrix( p$tbot.desc )
			tbot.se <- attach.big.matrix( p$tbot.se.desc )

			for ( r in 1:length(p$tyears) ) {
				yt = p$tyears[r]
				fn1 = file.path( tinterpdir, paste( "temporal.interpolation", yt, "rdata", sep=".") )
				fn2 = file.path( tinterpdir, paste( "temporal.interpolation.se", yt, "rdata", sep=".") )
				
				cstart = (r-1) * p$nw 
				col.ranges = cstart + (1:p$nw) 
				tinterp = tbot[,col.ranges]
				tinterp.se = tbot.se[,col.ranges]
				save( tinterp, file=fn1, compress=T) 
				save( tinterp.se, file=fn2, compress=T) 
			}
		
      if (p$use.bigmemory.file.backing) {
  			file.remove( p$fn.tbot , p$fn.tbot.se )
	  		file.remove( paste( c(p$fn.tbot , p$fn.tbot.se), "desc", sep=".") )
      }

			return ("completed temporal interpolations ")
    }
   


		# -------------------



    if (DS %in% c(  "spatial.interpolation", "spatial.interpolation.se", "spatial.interpolation.redo" )){
			   
			# interpolated predictions over only missing data
			spinterpdir =  file.path( project.directory("temperature"), "data", "interpolated", "spatial", p$spatial.domain )
			if (p$spatial.domain=="snowcrab") {
        spinterpdir = file.path( project.directory("temperature"), "data", "interpolated", "spatial", "SSE" )
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

      if ( exists("init.files", p) ) loadfunctions( p$init.files ) 
      if ( exists("libs", p) ) loadlibraries( p$libs ) 
      if ( is.null(ip) ) ip = 1:length(p$nruns)

      for ( r in ip ) { 
        y = p$runs[r, "yrs"]
        P = temperature.interpolations( p=p, DS="temporal.interpolation", yr=y  )
        P[ P < -1.9 ] = NA
        P[ P > 34 ] = NA
							
        V = temperature.interpolations( p=p, DS="temporal.interpolation.se", yr=y  )
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

            # testing
            testing= FALSE
            if (testing) {
              gs =  try(
                gstat( id="t", formula=P[ai,ww]~1 , locations=~plon+plat, data=O[ai,], nmax=100, maxdist=25, set=list(idp=.5), weights=W[ai,ww]) 
              , silent=T ) 
              if ( "try-error" %in% class(e) ) { 
                # simplest default inverse distance weighted interpolation (power = 0.5) to max dist of 10 km
                gs = gstat( id="t", formula=P[ai,ww]~1 , locations=~plon+plat, data=O[ai,], nmax=100, maxdist=25, set=list(idp=.5), weights=W[ai,ww]) 
              }
            }

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
       
				fn1 = file.path( spinterpdir,paste("spatial.interpolation",  y, "rdata", sep=".") )
				fn2 = file.path( spinterpdir,paste("spatial.interpolation.se",  y, "rdata", sep=".") )
				save( P, file=fn1, compress=T )
				save( V, file=fn2, compress=T )
 
			}
      return ("Completed")
    }
 

    
  } 

