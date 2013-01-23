
  temperature.interpolations = function( ip=NULL, p=NULL, DS=NULL, yr=NULL, method="FILE" ) {
    
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

			# require( mgcv ) # for "gam"
			P = bathymetry.db( p=p, DS="baseline" )
      p$nP = nrow(P);	rm(P); gc()
			p$wtimes = 1:52 
			p$nw = length(p$wtimes)
			p$ny = length(p$tyears)
	
			p$optimizers = c( "perf", "nlm", "bfgs", "optim", "newton",  "nlm.fd") # optimizers for gam
			p$nMin.tbot = 200 # min number of data points req before attempting to model timeseries.
			p$dist.multiplier = c( 1, 2, 3, 4, 5, 7.5, 10 ) # additional distance multipliers to extend search for data
			p$dist.km = 10 # search "radius" (actually a box)

			require( bigmemory )

			nr = p$nP
			nc = p$nw * p$ny

      p$fn.tbot = file.path( "interpolated.bigmemory.rdata.tmp" )
	  	p$fn.tbot.se = file.path( "interpolated.se.bigmemory.rdata.tmp" )
      
      bf1 = basename(p$fn.tbot) 
      bf2 = basename(p$fn.tbot.se) 
          
      df1 = paste(bf1, "desc",sep=".")
      df2 = paste(bf2, "desc",sep=".")
		

      if (method == "FILE" ) {

        # backingdirectory location does not seem to work in V 4.2.3 .. defaulting to home dir
        tbot = big.matrix(nrow=nr, ncol=nc, type="double" , init=NA,   backingfile=bf1, descriptorfile=df1   )  
        tbot.se = big.matrix(nrow=nr, ncol=nc, type="double", init=NA, backingfile=bf2, descriptorfile=df2  )

      } else if (method == "RAM" ) {

        tbot = big.matrix(nrow=nr, ncol=nc, type="double" , init=NA )  
        tbot.se = big.matrix(nrow=nr, ncol=nc, type="double", init=NA )

      }

      # required to operate in parallel 
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
				fn1 = file.path( tinterpdir, paste( "temporal.interpolation", yt, "rdata", sep=".") )
				fn2 = file.path( tinterpdir, paste( "temporal.interpolation.se", yt, "rdata", sep=".") )
				
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
			spinterpdir =  file.path( project.directory("temperature"), "data", "interpolated", "spatial", p$spatial.domain )
			if (p$spatial.domain=="snowcrab") {
        spinterpdir = file.path( project.directory("temperature"), "data", "interpolated", "spatial", "SSE" )
      }
    
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
         
      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (!is.null(p$env.init)) for( i in p$env.init ) source (i)
      if (is.null(ip)) ip = 1:length(p$tyears)

      require( gstat )
			require (mgcv )

      for ( r in ip ) { 
        y = p$tyears[r]
 
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

