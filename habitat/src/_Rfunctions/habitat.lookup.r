 
  habitat.lookup = function( x, p=NULL, DS="default", max.distance=5, truncatequantiles=c(0.005, 0.995) ) {
    
    # wrapping function to provide a common intercae to various habitat related lookup routines
    # truncation by quantiles is the default behaviour, to turn off, an explicit truncatequantiles=FALSE must be given
    # x must contain plon, plat, and chron
    
    loadfunctions( "utility" )
    RLibrary (p$libs) 
 
    coords = c("plon", "plat" )
    out = NULL
    nx = nrow(x)
    x$hid = 1:nx
    res = x # store for return

    # spatial information
    if ( !exists("plon", x) ) x = lonlat2planar (x, proj.type=p$internal.projection )
    if ( p$spatial.domain == "snowcrab" ) p$spatial.domain = "SSE"
      
    x$plon = grid.internal( x$plon, p$plons )
    x$plat = grid.internal( x$plat, p$plats )
    x = x[ which(is.finite( x$plon+x$plat ) ) ,]  # don't worry these will be merged back into "res" (above)

    xnames = names(x)
    
    # --------------------
    # time-invariant items 
    if ( DS %in% c( "depth", "depth.all", "substrate", "time.invariant", "baseline" )) {
      print( paste( "Looking up ", DS) )

      H = habitat.lookup.datasource( DS, p=p )  # bring in appropriate habitat data source
      H$plon = grid.internal( H$plon, H$plons )
      H$plat = grid.internal( H$plat, H$plats )
	    H = H[ which( is.finite( H$plon + H$plat)), ]
      Hnames = names(H)
      out = merge( x, H, by=coords, all.x=T, all.y=F, suffixes=c("", ".duplicated"), sort=FALSE )
      rm(x); gc()
      newvars = setdiff( Hnames, xnames ) 
      if (length(newvars)==0) newvars= setdiff( intersect( xnames, Hnames ), coords )
      outnames = names(out)
      oo = grep("duplicated", outnames) 
      if (length(oo) > 0 ) {
        for ( o in oo ) {
          vn = gsub(  ".duplicated", "", outnames[o] )
          vnd = outnames[o]
          im = which( !is.finite( out[ , vn ] ) ) # missing in input data
          if (length( im) > 0 ) out[im,vn] = out[im,vnd] # overwrite missing with proposals
          out[,vnd] = NULL
        }
      }
      # still missing .. interpolate ... actually pick closest location and copy
      im = which( !is.finite( out[, vn] ) )
      if ( length( im ) > 0 ) {
        distances =  rdist( H[,coords], out[im, coords ] )
        distances[ which(distances > max.distance) ] = NA
        for( jj in 1:length( im ) ) {
          dd = which.min( distances[,jj] )
          if (length(dd)==0) next()
          out[ im[jj],vn ] = H[dd,vn]
        }
      }
    }
  
    # -------------------
    # yearly-varying items:
    if ( DS %in% c( "default", "all", "all.data", "environmentals", "temperature.all" )) {
      if (! exists( "yr", x) ){
        if ( any( grepl( "chron", names(x) ) )) {
          x$yr = as.numeric( as.character( years( x$chron )))
        }
      } 
      if (! exists( "yr", x ) ) stop( "yr is required")  # required
 
      yrs = sort( unique( x$yr ))
      print( paste( "Looking up ", DS) )
     
      H = habitat.lookup.datasource( DS, yr=yrs[1], p=p  )  # test load to obtain variable names
      Hnames = names(H)
      newvars = setdiff( Hnames, xnames ) 
      if (length(newvars)==0) newvars= setdiff( intersect( xnames, Hnames ), coords )
      rm (H); gc()

      out = NULL
      for (yr in yrs) { 
        print( yr )
        H = habitat.lookup.datasource( DS, yr=yr, p=p  )  # bring in appropriate habitat data source

        H$plon = grid.internal( H$plon, H$plons )
        H$plat = grid.internal( H$plat, H$plats )
	      H = H[ which( is.finite( H$plon + H$plat)), ]
        
        ii = which( x$yr == yr )
        if (length( ii) == 0) next()  
        X = merge( x[ii,], H, by=coords, all.x=T, all.y=F, suffixes=c("", ".duplicated"), sort=FALSE )
        Xnames = names(X)

        oo = grep("duplicated", Xnames) 
        if (length(oo) > 0 ) {
          for ( o in oo ) {
            vn = gsub(".duplicated", "", Xnames[o],  )
            vnd = Xnames[o]
            im = which( !is.finite( X[ , vn ] ) ) # missing in input data
            if (length( im) > 0 ) X[im,vn] = X[im,vnd] # overwrite missing with proposals
            X[,vnd] = NULL
          }
        }

        # still missing .. interpolate
        im = which( !is.finite( X[, vn] ) )
        if ( length( im ) > 0 ) {
          distances =  rdist( H[,coords], X[im, coords ] )
          distances[ which(distances > max.distance) ] = NA
           for( jj in 1:length( im ) ) {
            dd = which.min( distances[,jj] )
            if (length(dd)==0) next()
            X[ im[jj],vn ] = H[dd,vn]
          }
        }
        out = rbind( out, X )
      } # end time invariant
    }

    # -------------------
    # weekly-varying items:
    if ( DS %in% c( "temperature", "temperature.weekly" )) {
      if (! exists( "yr", x) ) {
        if (any( grepl( "chron", names(x) ) )) {
          x$yr = as.numeric( as.character( years( x$chron )))
        }
      } 
      if (! exists( "yr", x ) ) stop( "yr is required")  # required
      
      if (! exists("weekno", x) ) {
        x$dayno = convert.datecodes(x$chron, "julian")  
        x$weekno = ceiling (x$dayno / 365 * 52 )
      }
      if (! exists( "weekno", x ) ) stop( "weekno is required")  # required

      print( "Looking up temperature at weekly scales" )

      yrs = sort( unique( x$yr ))

      B = bathymetry.db( p=p, DS="baseline" ) # already discretized to internal plons and plats
      B$row = 1:nrow(B)
      B$z = NULL

      O = NULL
      for (yr in yrs) { 
        print( yr )
  
        ii = which( x$yr == yr )
        if (length( ii) == 0) next()  
        X = merge( x[ii,], B, by=coords, all.x=T, all.y=F, sort=F, suffixes=c("", ".duplicated") )
          
        V = matrix( NA, ncol=2, nrow=length(ii) )	
        V[,1] = X$row
        V[,2] = X$weekno

        H = habitat.lookup.datasource( DS=DS, yr=yr, p=p  )  # bring in appropriate habitat data source
        if (is.null(H)) next()
        X$t.H = H[V]
        vn = newvars = "t" 
        vnd = "t.H"
        if (exists(vn, X)) {
          im = which( !is.finite( X[,vn] ) ) # missing in input data
          if (length( im) > 0 ) X[im,vn] = X[im,vnd] # overwrite missing with proposals
        } else {
          X[,vn] =  X[,vnd]
        }
        X[,vnd] = NULL

        # still missing .. interpolate
        im = which( !is.finite( X[, vn] ) )
        if ( length( im ) > 0 ) {
          distances =  rdist( B[,coords], X[im, coords ] )
          distances[ which(distances > max.distance) ] = NA
          for( jj in 1:length( im ) ) {
            dd = which.min( distances[,jj] )
            if (length(dd) > 0) X[ im[jj],vn ] = H[ dd, X[ im[jj],"weekno"] ]
          }
        }
        O = rbind( O, X )
      } # end time invariant
      out = O
    }
    
    # -------------------------------
    # final processing and formatting
      varstodrop = which( names(res) %in% newvars )
      if (length(varstodrop) > 0) res = res[ , -varstodrop ] # drop duplicates in advance of merge

      res = merge( res, out[, c("hid", newvars )], by="hid", all.x=T, all.y=F, sort=T )
      res = res[ order( res$hid ) , ]
      if ( nrow( res ) != nx ) {
        print( "Merge error -- duplicated coords" )
        stop()
      }
 
      if ( !any(!truncatequantiles) ) {
        for (nv in newvars) {
          res[,nv] = truncate.distribution( res[, nv], Ql=truncatequantiles[1], Qu=truncatequantiles[2] )
        }
      }
   
      to.remove = which(names(res) %in% c("hid")) 
      if (length(to.remove)>0) res = res[ , - to.remove ]
    
      return (res)
  }


