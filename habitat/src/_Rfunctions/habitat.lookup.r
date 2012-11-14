 
  habitat.lookup = function( x, p, datatype="all.data", discretization.scale = 0, dist.scale=5, 
    keep.lon.lat=F, truncatequantiles=c(0.01, 0.99) ) {
    
    # truncation by quantiles is the default behaviour 
    # .. to turn off, an explicit truncatequantiles=FALSE must be given

    require(fields)

    # x must contain plon, plat, and chron
    
		nx = nrow(x)
		x$hid = 1:nx
		x0 = data.frame( hid = x$hid )
    
    if (exists( "yr",x ) )  yrs = sort( unique( x$yr ))

    if (any( grepl( "chron", names(x) ) )) {
      require(chron)
      x$yr = as.numeric( as.character( years( x$chron )))
      yrs = sort( unique( x$yr ))
      if (is.null ( yrs) ) stop()
    }  
    
    if ( !any(grepl( "plon", names(x) ) )) {
      x = lonlat2planar (x, proj.type=p$internal.projection ) 
    }

    x = x[ which(is.finite( x$plon+x$plat ) ) ,]  # don't worry these will be merged back into "x0" (above)
	
    if ( p$spatial.domain == "snowcrab" ) p$spatial.domain = "SSE"

		if ( p$spatial.domain %in% c("SSE") ) {
			x$plon = round( x$plon, discretization.scale )
			x$plat = round( x$plat, discretization.scale )
		}	

		# dist.scale is the max dist to examine for closest neighbour
				
		out = NULL

		if ( datatype=="all.data" ) { 
		
			for (Y in yrs) { 
				print( Y )
				ii = which( x$yr == Y )
				if (length( ii) == 0) next()  
        H = x[ii,]  

        PS = habitat.db( DS="complete", p=p, year=Y )
				if (is.null(PS)) next ()
				PS$yr = NULL
				PS$plon = round( PS$plon, discretization.scale )
				PS$plat = round( PS$plat, discretization.scale )
				
				tmp = NULL
				tmp = merge( H, PS, by=c("plon", "plat" ), all.x=T, all.y=F, suffixes=c("", ".redundant") )
				
        oo =  grep("redundant", names( tmp)) 
				if (length(oo) > 0 ) {
					print( "Redundant vars found: Error in merge? Dropping the following..." )
          print( names(tmp)[oo] )
          tmp = tmp[, -oo ] 
				}

				miss = which( !is.finite( tmp$z ) )
				if( length(miss) >0 ) {
          hvars = setdiff( names( tmp) , c("plon", "plat", "yr", "chron", "hid" )  )
					distances =  rdist( PS[,c("plon", "plat")], H[miss, c("plon", "plat")] )
					for( jj in 1:length(miss) ) {
						dd = which.min( distances[,jj] )
						if (distances[ dd, jj ] < dist.scale ) {
							tmp[ miss[jj], hvars ] = PS[dd, hvars ]
						}
					}
				}	
				out = rbind( out, tmp )
				
			}

      if ( !truncatequantiles ) {
        # limit to certain quantiles as the interpolation/extrapolation can give extreme results
        for (i in hvars) out[,i] = truncate.distribution( out[,i], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }

			if (keep.lon.lat) { 
        to.remove = which(names(out) %in% c("chron")) 
      } else {
        to.remove = which(names(out) %in% c("plon","plat","chron", "yr")) 
      }

      res = out 
      if (length(to.remove)>0) res = res[ , - to.remove ]
			
      res = merge( x0, res, by="hid", all.x=T, all.y=F, sort=T ) 
	    res = res[ order(res$hid) , ]
			res$hid = NULL

			if ( nrow(res) != nx ) {
				print( "Merge error -- duplicated coords" )
				stop()
			}
		  return( res )

		}

		if ( datatype=="temperature.weekly" ) { 

			if (! exists("weekno", x) ) {
        x$dayno = convert.datecodes(x$chron, "julian")  # from  source ("/home/jae/ecomod/common/functions.date.r")
			  x$weekno = ceiling (x$dayno / 365 * 52 )
			}
      weeknos = sort( unique( x$weekno ) )

			O = bathymetry.db( p=p, DS="baseline" )
		  O$plon = round( O$plon, discretization.scale )
			O$plat = round( O$plat, discretization.scale )
			O$z = NULL
			O$row = 1:nrow(O)
      
			for (Y in yrs) {
				print( Y )
		  
				PS = hydro.db( p=p, DS="spatial.interpolation", yr=Y  )
				if (is.null(PS)) next ()
     		
				PS[ PS < -2 ] = -2
			  PS[ PS > 30 ] = 30 
			
				ii = which( x$yr == Y )
				if ( length(ii) == 0 ) next()

				H = x[ii,]  

				tmp = NULL
				tmp = merge( H, O, by=c("plon", "plat" ), all.x=T, all.y=F, sort=F )
				
				hij = matrix( NA, ncol=2, nrow=length(ii) )	
				hij[,1] = tmp$row
				hij[,2] = tmp$weekno

				tmp$t = as.numeric( PS[ hij ] )

				miss = which( !is.finite( tmp$t ) )
				if( length(miss) >0 ) {
          distances =  rdist( O[,c("plon", "plat")], H[miss, c("plon", "plat")] )
					for( jj in 1:length(miss) ) {
						dd = which.min( distances[,jj] )
						if (distances[ dd, jj ] < dist.scale ) {
							tmp$t[ miss[jj] ] = PS[dd, tmp$weekno[miss[jj]] ]
						}
					}
				}
				out = rbind( out, tmp )
			} # end for years

			if ( is.null( out) )  return( x$t )  # nothing to do, return itself

      
      if ( !truncatequantiles ) {
        # limit to certain quantiles as the interpolation/extrapolation can give extreme results
        hvars = setdiff( names(out) , c("hid", "t" )  )
        for (i in hvars) out[,i] = truncate.distribution( out[,i], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }

      res = merge( x0, out[ , c("hid", "t" )], by="hid", all.x=T, all.y=F, sort=T )
	    res = res[ order(res$hid) , ]
			res$hid = NULL

			if ( nrow(res) != nx ) {
				print( "Merge error -- duplicated coords" )
				stop()
			}
		  return( res$t )


		}
		
	

    if ( datatype=="speciesarea" ) { 

      for (Y in yrs) { 
				print( Y )
				ii = which( x$yr == Y )
				H = x[ii,]  
    
        SAG = sar.interpolate( p=p, yr=y, modtype=p$speciesarea.modeltype )
 				if (is.null(SAG)) next ()
				
        SAG$yr = NULL
				SAG$plon = round( SAG$plon, discretization.scale )
				SAG$plat = round( SAG$plat, discretization.scale )
        
				tmp = NULL
				tmp = merge( H, SAG, by=c("plon", "plat" ), all.x=T, all.y=F, suffixes=c("", ".redundant") )
				
				if (length( grep("redundant", names( tmp ) ) > 0 )) {
					print( "Error in merge" )
					stop()
				}

				miss = which( !is.finite( tmp$z ) )
				if( length(miss) >0 ) {
					hvars = setdiff( names( tmp) , c("plon", "plat", "yr", "chron", "hid" )  )
					distances =  rdist( PS[,c("plon", "plat")], H[miss, c("plon", "plat")] )
					for( jj in 1:length(miss) ) {
						dd = which.min( distances[,jj] )
						if (distances[ dd, jj ] < dist.scale ) {
							tmp[ miss[jj], hvars ] = SAG[ dd, hvars ]
						}
					}
				}
				out = rbind( out, tmp )
				
			}
      rm (SAG) 
      
      if ( !truncatequantiles ) {
        # limit to certain quantiles as the interpolation/extrapolation can give extreme results
        hvars = setdiff( names(out) , c("plon","plat","chron", "yr")  )
        for (i in hvars) out[,i] = truncate.distribution( out[,i], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }

			res = out[ , - which(names(out) %in% c("plon","plat","chron", "yr")) ]
			res = merge( x0, res, by="hid", all.x=T, all.y=F, sort=T )
	    res = res[ order(res$hid) , ]
			res$hid = NULL

			if ( nrow(res) != nx ) {
				print( "Merge error -- duplicated coords" )
				stop()
			}
		  return( res )

		}
		
    
    if ( datatype=="depth" ) { 
  
			O = bathymetry.db( p=p, DS="baseline" )
		  O$plon = round( O$plon, discretization.scale )
			O$plat = round( O$plat, discretization.scale )

			tmp = NULL
			tmp = merge( x, O, by=c("plon", "plat" ), all.x=T, all.y=F, suffixes=c("",".bathymetry") )

			miss = which( !is.finite( tmp$z ) )
			if( length(miss) >0 ) {
        if ( exists( "z.bathymetry", tmp ) ) {
          tmp$z[miss] = tmp$z.bathymetry[miss]
          tmp$z.bathymetry = NULL
        }
        miss = which( !is.finite( tmp$z ) )
        if ( length(miss) >0 ) {
          distances =  rdist( O[,c("plon", "plat")], tmp[miss, c("plon", "plat")] )
          for( jj in 1:length(miss) ) {
            dd = which.min( distances[,jj] )
            if (distances[ dd, jj ] < dist.scale  ) {
              tmp$z[ miss[jj] ] = O$z[dd]
            }
          }
        }
			}

			res = merge( x0, tmp[ , c("hid", "z" )], by="hid", all.x=T, all.y=F, sort=T )
		  res = res[ order(res$hid) , ]
			res$hid = NULL

			if ( nrow(res) != nx ) {
				print( "Merge error -- duplicated coords" )
				stop()
			}
		  return( res$z )
		
		}

		
    if ( datatype=="substrate" ) {

      S =  substrate.db ( p=p, DS="planar")
      S$substrate.mean = log(S$grainsize)
      S$grainsize = NULL

			S$plat = round( S$plat, discretization.scale )
			S$plon = round( S$plon, discretization.scale )


			tmp = NULL
			tmp = merge( x, S, by=c("plon", "plat" ), all.x=T, all.y=F )

			miss = which( !is.finite( tmp$substrate.mean ) )
			if( length(miss) >0 ) {
				distances =  rdist( S[,c("plon", "plat")], x[miss, c("plon", "plat")] )
				for( jj in 1:length(miss) ) {
					dd = which.min( distances[,jj] )
					if (distances[ dd, jj ] < dist.scale  ) {
						tmp$substrate.mean[ miss[jj] ] = S$substrate.mean[dd]
					}
				}
			}

			res = merge( x0, tmp[ , c("hid", "substrate.mean" )], by="hid", all.x=T, all.y=F, sort=T )
		  res = res[ order(res$hid) , ]
			res$hid = NULL
			
      if ( nrow(res) != nx ) {
				print( "Merge error -- duplicated coords" )
				stop()
			}
		  return( res$substrate.mean )

		}
		
    if ( datatype=="time.invariant" ) { 
      if ( ! exists( "z", x ) ) x$z = habitat.lookup( x=x, p=p, datatype="depth", 
          discretization.scale=discretization.scale, dist.scale=dist.scale )
      if ( ! exists("substrate.mean", x) ) x$substrate.mean = habitat.lookup( x=x, p=p, 
          datatype="substrate", discretization.scale=discretization.scale, dist.scale=dist.scale )
		}

		if ( datatype=="time.variant" ) { 

		}

			
	}


