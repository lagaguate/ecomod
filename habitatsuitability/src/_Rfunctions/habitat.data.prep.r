 

  habitat.data.prep = function( X, p ) {
       
      X$yr = convert.datecodes(X$chron, "year")
      X$julian = convert.datecodes(X$chron, "julian")
      X$month = floor(X$julian/365*12) + 1 

      # ---- convert to planar coords and discretize to allow merging of habitat info ...
      X = X[ which( 
        X$lon >= p$corners$lon[1] & X$lon <= p$corners$lon[2] &
        X$lat >= p$corners$lat[1] & X$lat <= p$corners$lat[2] 
      ),]

      X = lonlat2planar( X, proj.type=p$internal.projection )  # utm20, WGS84 (snowcrab geoid) 
      X$plon = grid.internal( X$plon, p$plons )
      X$plat = grid.internal( X$plat, p$plats )

      X = X[ which( is.finite( X$plon + X$plat) ) , ]
      
      X = X[ filter.region.polygon( X, region=p$studyarea ) , ]


      #----------------------------
      # look up missing environmental data
      sp.br=c(1, 5, 10) # distances to interpolate ... short-scale only (km) if no exact match
        
      g = bathymetry.db( p, DS="Z.planar" )
      names(g)[which( names(g)=="z")] = "z.lookup"
      n0 = nrow(X)
      X = merge( X, g, c("plon", "plat"), all.x=T, all.y=F, sort=F )
      if ( n0 != nrow(X) )  stop("Merge error")
      
      z.accepted = c(10, 700)
      to.drop = unique( c( which( X$z.lookup < z.accepted[1] ) , which( X$z.lookup > z.accepted[2] ) ) )
      if (length( to.drop) >0)   X = X[ -to.drop, ]  

      # unreliable depth records (possibly due to improper lon/lat) or just bad data .. use lookup table instead
      iiz = which( abs( X$z - X$z.lookup) > 20 ) 
      if (length( iiz ) >0) X$z[ iiz ] =  X$z.lookup[ iiz ]
      
      iizna = which( !is.finite( X$z ) ) 
      if (length( iizna ) >0)  X$z[ iizna ] = X$z.lookup [iizna ]
      
      X$z.lookup = NA
      X$z.lookup = habitat.lookup.simple( X, p=p, vnames="z", lookuptype="depth", sp.br=sp.br  )  # takes 14GB!
      iizna = which( !is.finite( X$z ) ) 
      if (length( iizna ) >0)  X$z[ iizna ] = X$z.lookup [iizna ]
      X$z.lookup = NULL
      X = X[ which( is.finite( X$z ) ), ]
      gc()

      # this breaks it down by year and so memory is not really a limitation
      X$t = NA
      X$t.lookup = habitat.lookup.simple( X, p=p, vnames="t", lookuptype="temperature.weekly", sp.br=sp.br  )
      iit = which( abs( X$t - X$t.lookup) > 5 ) # potentially unreliable temperatures due to improper lon/lat or bad data
      if (length( iit ) >0) X$t[ iit ] =  X$t.lookup[ iit ]
      
      iitna = which( ! is.finite( X$t ) ) 
      if (length( iitna ) >0) X$t[ iitna ] =  X$t.lookup[ iitna ]
      X$t.lookup = NULL
 
      X = X[ which( is.finite( X$t ) ), ]
      gc()

      XsH = habitat.lookup.grouped( X, p=p, lookuptype="all.data", sp.br=sp.br )
      names(XsH)[which( names(XsH)=="t")] = "t.annual"
      XsH$z = NULL
        
      X = cbind( X, XsH)

    return (X)
  }



