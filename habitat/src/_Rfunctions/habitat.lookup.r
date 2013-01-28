 
  habitat.lookup = function( x, p, datatype="all.data", discretization.scale = 0, dist.scale=5, 
    keep.lon.lat=F, truncatequantiles=c(0.01, 0.99) ) {
    
    # truncation by quantiles is the default behaviour 
    # .. to turn off, an explicit truncatequantiles=FALSE must be given
		# dist.scale is the max dist to examine for closest neighbour

		if ( datatype=="all.data" ) { 
        
      require(fields)
      # x must contain plon, plat, and chron
      nx = nrow(x)
      x$hid = 1:nx
      x0 = data.frame( hid = x$hid )

      if ( !any(grepl( "plon", names(x) ) )) {
        x = lonlat2planar (x, proj.type=p$internal.projection ) 
      }
      if ( p$spatial.domain == "snowcrab" ) p$spatial.domain = "SSE"
      x$plon = round( x$plon, discretization.scale )
      x$plat = round( x$plat, discretization.scale )
      x = x[ which(is.finite( x$plon+x$plat ) ) ,]  # don't worry these will be merged back into "x0" (above)
      
      if (exists( "yr",x ) )  yrs = sort( unique( x$yr ))
      if (any( grepl( "chron", names(x) ) )) {
        require(chron)
        x$yr = as.numeric( as.character( years( x$chron )))
        yrs = sort( unique( x$yr ))
        if (is.null ( yrs) ) stop()
      }  

      if (! exists("weekno", x) ) {
        x$dayno = convert.datecodes(x$chron, "julian")  # from  source ("/home/jae/ecomod/common/functions.date.r")
        x$weekno = ceiling (x$dayno / 365 * 52 )
      }
      weeknos = sort( unique( x$weekno ) )

      out = NULL
		
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

      td = temperature.lookup( x=x, p=p, discretization.scale=discretization.scale, 
        dist.scale=dist.scale )
      if ( !truncatequantiles ) {
        # limit to certain quantiles as the interpolation/extrapolation can give extreme results
        hvars = setdiff( names(td) , c("hid", "t" )  )
        for (i in hvars) td[,i] = truncate.distribution( td[,i], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }
      return( td$t )
		}
	

    if ( datatype=="speciesarea" ) { 
      sar = speciesarea.lookup( x=x, p=p, discretization.scale=discretization.scale, 
        dist.scale=dist.scale )
      if ( !truncatequantiles ) {
        hvars = setdiff( names(sar) , c("plon","plat","chron", "yr")  )
        for (i in hvars) sar[,i] = truncate.distribution( sar[,i], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }
      return(sar)
		}
		
    
    if ( datatype=="depth" ) { 
			z = bathymetry.lookup( x=x, p=p, discretization.scale=discretization.scale, 
        dist.scale=dist.scale )
      if ( !truncatequantiles ) {
        z = truncate.distribution( z, Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }
      return( z )
		}

		
    if ( datatype=="substrate" ) {
      s = substrate.lookup(  x=x, p=p, discretization.scale=discretization.scale, 
        dist.scale=dist.scale )
      if ( !truncatequantiles ) {
        s = truncate.distribution( s, Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }
      return (s)
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


