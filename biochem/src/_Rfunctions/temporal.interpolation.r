
  temporal.interpolation = function( ip=NULL, p=NULL ) {
         			
		  ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
      if (is.null(ip)) ip = 1:p$nP
			
			require(mgcv)
			require(bigmemory)
			
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
			  # print (mm)			
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
				
				if (length(i) < p$nMin.tbot/4 ) {   
					# there really is not enough data
					return(NA) # 	print ( "Not enough data .. giving up" )
				}

				O = O0[i,] # faster to reduce the size of O
			
				# temporal interpolation using a sinusoidal function
				O$cos.w = cos( 2*pi*O$weekno/52 )
				O$sin.w = sin( 2*pi*O$weekno/52 )

				# weight data in time and space: inverse distance and inverse time(year) squared
				wspace = 1 / (( Pi$plon - O$plon)**2 + (Pi$plat - O$plat)**2 )
				wspace[ which( is.infinite( wspace ) ) ] = 1
				O$w = wspace    
							
				#wtime = 1 / ( yr - O$yr)**2 
				#wtime[ which( is.infinite( wtime ) ) ] = 1
				# O$w = wspace * wtime  

				 # to add an offset to a trig function (b) must add cos to a sin::
			 	 # y ~ a + c*sin(x+b)
	 			 # y ~ a + c*sin(b)*cos(x) + c*cos(b)*sin(x)  
				 #   .. as C*sin(x+b) = C*( cos(b) * sin(x) + sin(b) * cos(x) )
				 # y ~ b0 + b1*x1 + b2*x2
				 # where: 
				 #   a = b0
				 #   c^2 = b1^2 + b2^2 = c^2*(sin^2(b) + cos^2(b))
				 #   c = sqrt(b1^2 + b2^2)
				 #   b1/b2 = tan(b)  
				 #   b = arctan(b1/b2)
			
			  offsetvalue = 10  # used to temporarily make all temperatures positive valued to allow lognormal distributional modelling
				O$t = O$t + offsetvalue
				
				for ( o in p$optimizers ) {
					print (o )
					optimizer = c( "outer", o ) 
					if (o=="perf") optimizer=o
						e = try ( 
							gam( t ~ s(cos.w, sin.w) + as.factor(yr) + s(plon, plat, z), data=O, weights=w, optimizer=optimizer ), 
							silent=T
						)
					if ( ! ("try-error" %in% class(e) ) ) break()  # take the first successful solution
				}
								
        # try a simpler, unweighted model
				if ( "try-error" %in% class(e) ) { 
					for ( o in c("bfgs", "perf") ) {
						print (o )
						optimizer = c( "outer", o ) 
						if (o=="perf") optimizer=o
							e = try ( 
								gam( t ~ s(cos.w, sin.w) + as.factor(yr) + s(plon, plat, z), data=O, optimizer=optimizer ), 
								silent=T
							)
						if ( ! ("try-error" %in% class(e) ) ) break()  # take the first successful solution
					}
				}
		
				for ( o in p$optimizers ) {
					print (o )
					optimizer = c( "outer", o ) 
					if (o=="perf") optimizer=o
						e = try ( 
							gam( t ~ s(cos.w, sin.w, yr) + s(plon, plat, z), data=O, weights=w, optimizer=optimizer ), 
							silent=T
						)
					if ( ! ("try-error" %in% class(e) ) ) break()  # take the first successful solution
				}

				# try a simpler, unweighted model
				if ( "try-error" %in% class(e) ) { 
					for ( o in c("bfgs", "perf") ) {
						print (o )
						optimizer = c( "outer", o ) 
						if (o=="perf") optimizer=o
							e = try ( 
								gam( t ~ s(cos.w, sin.w, yr) + s(plon, plat, z), data=O, optimizer=optimizer ), 
								silent=T
							)
						if ( ! ("try-error" %in% class(e) ) ) break()  # take the first successful solution
					}
				}
				
				# if still not working, try a simpler model still
				if ( "try-error" %in% class(e) ) { 
					for ( o in c("bfgs", "perf") ) {
						print (o )
						optimizer = c( "outer", o ) 
						if (o=="perf") optimizer=o
							e = try ( 
								gam( t ~ s(cos.w, sin.w, yr) + s(plon, plat), data=O, weights=w, optimizer=optimizer ), 
								silent=T
							)
						if ( ! ("try-error" %in% class(e) ) ) break()  # take the first successful solution
					}
				}
						
				# if still not working, try as a last resort, a simple temporal model only
				if ( "try-error" %in% class(e) ) { 
					for ( o in c("bfgs", "perf") ) {
						print (o )
						optimizer = c( "outer", o ) 
						if (o=="perf") optimizer=o
							e = try ( 
								gam( t ~ s(cos.w, sin.w, yr), data=O, weights=w, optimizer=optimizer ), 
								silent=T
							)
						if ( ! ("try-error" %in% class(e) ) ) break()  # take the first successful solution
					}
				}
				
			 # if still not working, try as a last resort, a simple temporal model only
				if ( "try-error" %in% class(e) ) { 
					for ( o in c("bfgs", "perf") ) {
						print (o )
						optimizer = c( "outer", o ) 
						if (o=="perf") optimizer=o
							e = try ( 
								gam( t ~ s(cos.w, sin.w, yr), data=O, optimizer=optimizer ), 
								silent=T
							)
						if ( ! ("try-error" %in% class(e) ) ) break()  # take the first successful solution
					}
				}
				
				if ( "try-error" %in% class(e) ) next()  # no solutions

				debug = F
				if (debug) {
					summary(e); AIC(e)
					plot(e, all.terms=T)
				}
			
				OP2 =  OP = expand.grid( plon=Pi$plon, plat=Pi$plat, weekno=p$wtimes, yr=p$tyears, z=Pi$z )
				OP$cos.w = cos( 2*pi*OP$weekno/52 )
				OP$sin.w = sin( 2*pi*OP$weekno/52 )

				preds = try( predict( e, newdata=OP, type="response", se.fit=T ) ) 
				if ( class(preds)=="try-error"  ) {
					print ( "Prediction failure" )
					next()
				}
							
				pp = NULL
				pp = preds$fit - offsetvalue
				
				
				#pp = xtabs(  pp ~ yr+weekno, data=OP )
				
				#pp.se = xtabs( preds$se ~ yr+weekno, data=OP )
				
				# x11(); plot( (t-offsetvalue)~weekno, data=O, pch=20, ylim=c(-1,20) ); for (i in 1:52) lines( pp[i,] )

				# return real data back to matrix
				#ii = which( O$plon==Pi$plon & O$plat==Pi$plat )
				#if ( length (ii) > 0 ) {
					# print( "debug" )
				#	OP2 = merge( OP2, O, by=c("plon", "plat", "yr", "weekno"), all.x=T, all.y=F )
				#	orig = xtabs( t ~ yr+weekno, data=OP2 )
				#	jj = which( is.finite( orig ) )
				#	pp[jj] = orig[jj]
				# }
				
				tbot[ mm,] <- pp
				tbot.se[mm,] <- preds$se
			
			} # end each point
	    return ( ip )
    }
    


