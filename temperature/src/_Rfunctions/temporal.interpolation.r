
  temporal.interpolation = function( ip=NULL, p=NULL ) {
         			
		  ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (exists( "init.files", p)) loadfilelist( p$init.files ) 
      if (exists( "libs", p)) loadlibraries( p$libs ) 
      if (is.null(ip)) ip = 1:p$nP
				
			tbot <- attach.big.matrix( p$tbot.desc )
			tbot.se <- attach.big.matrix( p$tbot.se.desc )

			#require(ff) 
			#options(fffinalizer='close') # do not delete ff file, just close it
			 
			P = bathymetry.db( p=p, DS="baseline" )
			O = hydro.db( p=p, DS="bottom.gridded.all"  )
			O = O[, c("plon", "plat", "yr", "weekno", "t", "z", "salinity") ]
			O0 = O[ which( is.finite(O$t)) ,]
			
			rm (O) ; gc()	
			
			for ( mm in ip ) {
				Pi=P[mm,]
			  print (mm)			
				for ( dm in p$dist.km ) { 
					drange = c(-1,1) * dm
					plon0 = Pi$plon + drange
					plat0 = Pi$plat + drange
					i = which( O0$plon > plon0[1] & O0$plon < plon0[2] & O0$plat > plat0[1] & O0$plat < plat0[2] ) 
					if (length(i) < p$nMin.tbot ) { # nMin.tbot is the prefered number of data points
						next() 
					} else { 
						break()
					}
				}						
				
				if (length(i) < p$nMin.tbot ) return(NA) # not enough data nearby to complete interpolation

				O = O0[i,] # faster to reduce the size of O
			
				# weight data in space: inverse distance squared
				O$w = 1 / (( Pi$plon - O$plon)**2 + (Pi$plat - O$plat)**2 )
				O$w[ which( is.infinite( O$w ) ) ] = 1
							
        out = list()
        nmodels = 8
        # the models that have been blanked out below are consistently poor in terms of AIC 
        # but a few of the simplest are retained in case there is no optimal solution using all covariates
        j = 1 # output counter
        for ( nn in 1:nmodels ) {
          model = switch( nn,
            try( gam( t ~ s(yr) + s(weekno), data=O ) ),
            try( gam( t ~ s(yr) + s(weekno, k=2, bs="cc"), data=O , optimizer="perf") ),
#            try( gam( t ~ s(weekno, yr) + s(weekno, k=2, bs="cc") + s(yr) , data=O , optimizer="perf") ),
#            try( gam( t ~ s(weekno, yr) + s(weekno, k=2, bs="cc") + s(yr) , data=O , optimizer="perf", weights=w ) ),
#            try( gam( t ~ s(weekno, yr) + s(weekno, bs="cc") + s(yr) , data=O , optimizer="perf", weights=w ) ),
#            try( gam( t ~ s(weekno, yr) + s(weekno, k=2, bs="cc") + s(yr) + s(z) , data=O , optimizer="perf") ),
#            try( gam( t ~ s(weekno, yr) + s(weekno, k=2, bs="cc") + s(yr) + s(z) , data=O , optimizer="perf", weights=w) ),
            try( gam( t ~ s(weekno, yr) + s(weekno, bs="cc") + s(yr) + s(z) , data=O , optimizer="perf", weights=w) ),
            try( gam( t ~ s(weekno, yr) + s(weekno, k=2, bs="cc") + s(yr) + s(plon, plat) + s(z), data=O, optimizer="perf" )) ,
            try( gam( t ~ s(weekno, yr) + s(weekno, bs="cc") + s(yr) + s(plon, plat) + s(z), data=O, optimizer="perf" )) ,
            try( gam( t ~ s(weekno, yr) + s(weekno, k=2, bs="cc") + s(yr) + s(plon, plat) + s(z), data=O, weights=w, optimizer="perf" )) ,
            try( gam( t ~ s(weekno, yr) + s(weekno, bs="cc") + s(yr) + s(plon, plat) + s(z), data=O, weights=w, optimizer=c("outer", "nlm" ) )), # in case perf does not converge
            try( gam( t ~ s(weekno, yr) + s(weekno, k=2, bs="cc") + s(yr) + s(plon, plat, z) + s(z), data=O, optimizer="perf" )) 
#            try( gam( t ~ s(weekno, yr) + s(weekno, k=2, bs="cc") + s(yr) + s(plon, plat, z) + s(z), data=O, weights=w, optimizer="perf" )) 
          )
          if ( "try-error" %in% class(model) ) next()
          out[[j]] = model
          j = j+1
        }
        
        if (j==1) next() # no success 

        aics = lapply( out, AIC )
        sel = which.min( aics)
        e = out[[sel]]  # final model
        # print(aics)
        # print(sel)

        rm(out) ; gc()

				OP2 =  OP = expand.grid( plon=Pi$plon, plat=Pi$plat, weekno=p$wtimes, yr=p$tyears, z=Pi$z )

				preds = try( predict( e, newdata=OP, type="response", se.fit=T ) ) 
				if ( class(preds)=="try-error"  ) {
					print ( "Prediction failure" )
					next()
				}
          
        ii = which( O$plon==Pi$plon & O$plat==Pi$plat )
        if ( length (ii) > 0 ) {
          	OP2 = merge( OP2, O, by=c("plon", "plat", "yr", "weekno"), all.x=T, all.y=F )
          	orig = xtabs( t ~ yr+weekno, data=OP2 )
          	jj = which( is.finite( orig ) )
          	preds$fit[jj] = orig[jj]
        }

				tbot[ mm,] <- preds$fit
				tbot.se[mm,] <- preds$se
			
			} # end each point
	    return ( ip )
    }
    


