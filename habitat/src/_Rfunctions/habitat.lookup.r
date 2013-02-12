 
  habitat.lookup = function( x, p, datatype="default", discretization.scale = 0, dist.scale=5, 
    keep.lon.lat=F, truncatequantiles=c(0.01, 0.99) ) {
    
    # wrapping function to provide a common intercae to various habitat related lookup routines
    
    # truncation by quantiles is the default behaviour 
    # .. to turn off, an explicit truncatequantiles=FALSE must be given
		# dist.scale is the max dist to examine for closest neighbour
  
    if ( datatype=="default" ) { 
      out = habitat.lookup.default ( x=x, p=p, discretization.scale=discretization.scale, dist.scale=dist.scale )
      if ( !any(!truncatequantiles) ) {
        hvars = setdiff( names(out) , names(x)  )
        # limit to certain quantiles as the interpolation/extrapolation can give extreme results
        for (i in hvars) out[,i] = truncate.distribution( out[,i], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }
      if (keep.lon.lat) { 
        to.remove = which(names(out) %in% c("chron")) 
      } else {
        to.remove = which(names(out) %in% c("plon","plat","chron", "yr")) 
      }
      if (length(to.remove)>0) out = out[ , - to.remove ]
      return (out)
		}
	

		if ( datatype %in% c("all.data", "all") ) { 
      out = habitat.lookup.all( x=x, p=p, discretization.scale=discretization.scale, dist.scale=dist.scale ) 
 
      if ( !any(!truncatequantiles) ) {
        # limit to certain quantiles as the interpolation/extrapolation can give extreme results
        hvars = setdiff( names(out) , names(x)  )
        for (i in hvars) out[,i] = truncate.distribution( out[,i], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }

      if (keep.lon.lat) { 
        to.remove = which(names(out) %in% c("chron")) 
      } else {
        to.remove = which(names(out) %in% c("plon","plat","chron", "yr")) 
      }

      if (length(to.remove)>0) out = out[ , - to.remove ]
      return (out)
		}
		
  

		if ( datatype=="temperature.weekly" ) { 

      td = temperature.lookup( x=x, p=p, discretization.scale=discretization.scale, 
        dist.scale=dist.scale )
      if ( !any(!truncatequantiles) ) {
        # limit to certain quantiles as the interpolation/extrapolation can give extreme results
        td = truncate.distribution( td, Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }
      return( td )
		}
	

    if ( datatype=="speciesarea" ) { 
      sar = speciesarea.lookup( x=x, p=p, discretization.scale=discretization.scale, 
        dist.scale=dist.scale )
      if ( !any(!truncatequantiles) ) {
        hvars = setdiff( names(sar) , c("plon","plat","chron", "yr")  )
        for (i in hvars) sar[,i] = truncate.distribution( sar[,i], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }
      return(sar)
		}
		
    
    if ( datatype=="depth" ) { 
			z = bathymetry.lookup( x=x, p=p, discretization.scale=discretization.scale, 
        dist.scale=dist.scale )
      if ( !any(!truncatequantiles) ) {
        z = truncate.distribution( z, Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
      }
      return( z )
		}

		
    if ( datatype=="substrate" ) {
      s = substrate.lookup(  x=x, p=p, discretization.scale=discretization.scale, 
        dist.scale=dist.scale )
      if ( !any(!truncatequantiles) ) {
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


