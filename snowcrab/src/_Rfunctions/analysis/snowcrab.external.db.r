
  snowcrab.external.db = function( p=NULL, DS="", vname="" ) {
  
    if ( DS=="det.snowcrab.in.groundfish.survey" ) {
      loadfunctions( "groundfish", functionname="current.year.r" )
      loadfunctions( "groundfish", functionname="load.groundfish.environment.r" )


      ct = groundfish.db( "det" )
      ct = filter.taxa( x=ct, method="snowcrab")
     
      imale = which( ct$sex==1 )
      ifemale = which( ct$sex==2 )
      
      ct$sex = NA 
      ct$sex[ imale ] = male 
      ct$sex[ female ] = female 

      ct$cw = ct$len
      
      ct$mat = NULL  # cannot be determined 
      ct$len =NULL
      ct$spec= NULL
      ct$age =NULL
      ct$fshno = NULL
          
      # only a few types of translations are possible, based upon size classes
      # as there is no chela or abdomen or maturit y information
      # f7, f8, f9, f10, m7, to m13
      vt = vname 
      vt = gsub( "^totno.", "", vt )
      vt = gsub( "^totmass.", "", vt )
      vt = gsub( ".no$", "", vt )
      vt = gsub( ".mass$", "", vt )
      
      if ( vt %in% c("R0", "m.com"  ) ) {
        vi = which( ct$sex==male & ct$cw>=95 & ct$cw<200  ) 
      } else {
        vi = filter.class( ct, vt ) 
      }

      ct = ct[ vi, ]

      return(ct)
    }
  
    if ( DS %in% c("set.snowcrab.in.groundfish.survey", "set.snowcrab.in.groundfish.survey.redo"  ) ) {

      fn = file.path( project.directory("snowcrab"), "R", paste("set.groundfish", vname, "rdata", sep=".") )

      if ( DS=="set.snowcrab.in.groundfish.survey" ) {
        sm = NULL
        if ( file.exists( fn) )  load( fn )
        return (sm)
      }

      loadfunctions( "groundfish", functionname="current.year.r" )
      loadfunctions( "groundfish", functionname="load.groundfish.environment.r" )

      ct = snowcrab.external.db( DS="det.snowcrab.in.groundfish.survey", vname=vname )
      if (nrow(ct)==0) return()

      uu = sum.data( ct, factors="id", variable="number" )
      names(uu) =c( "id", "totno" )

      sm = groundfish.db( "sm.base" )
      sm = lonlat2planar( sm, proj.type=p$internal.projection )  # utm20, WGS84 (snowcrab geoid) 
      sm$plon = grid.internal( sm$plon, p$plons )
      sm$plat = grid.internal( sm$plat, p$plats )

      sm = sm[ which( is.finite( sm$plon + sm$plat) ) , ]
      if (nrow(sm)==0) return()

      sm = merge (sm, uu, by = "id", sort=F, all.x=T, all.y=F )
      oo = as.data.frame( matrix( unlist( strsplit( sm$id, ".", fixed=T )), ncol=2, byrow=T ))
      names(oo) = c("trip", "set" )
      oo$trip = as.character( oo$trip)
      oo$set = as.numeric( as.character( oo$set ))
      sm = cbind( sm, oo)
      sm$survey = "groundfish"
      sm$metric = "number"
      sm$z = sm$sdepth
      sm$t = sm$temp
      sm$sa = sm$sakm2
      sm$totno [ which( !is.finite( sm$totno)) ] = 0 # assume to be real zeros as dervied from trawls
      sm$temp = NULL
      sm$sdepth = NULL
      sm$cftow = NULL
      
      sm$yr = convert.datecodes(sm$chron, "year")
      sm$julian = convert.datecodes(sm$chron, "julian")
      sm$weekno = floor(sm$julian/365*52) + 1 


      # zero values before ~ 1998 are uncertain 
      # as sampling program was not capturing consistent information on invertebrates.
      # to be safe, assume only 2000 and later to be "real" zeros
      # sm$totno[ which(sm$yr < 2000 & sm$totno==0) ] = NA 
      
      #----------------------------
      # look up missing environmental data
      sp.br=c(1, 5, 10) # distances to interpolate ... short-scale only (km) if no exact match
        
      g = bathymetry.db( p,DS="Z.planar" ) 
      
      names(g)[which( names(g)=="z")] = "z.lookup"
      n0 = nrow(sm)
      sm = merge( sm, g, c("plon", "plat"), all.x=T, all.y=F, sort=F )
      if ( n0 != nrow(sm) )  stop("Merge error")
      
      # unreliable depth records (possibly due to improper lon/lat) or just bad data .. use lookup table instead
      iiz = which( abs( sm$z - sm$z.lookup) > 20 ) 
      sm$z[ iiz ] =  sm$z.lookup[ iiz ]
      
      iizna = which( !is.finite( sm$z ) ) 
      sm$z[ iizna ] = sm$z.lookup [iizna ]
      
      sm$z.lookup = NA
      sm$z.lookup = habitat.lookup.simple( sm, p=p, vnames="z", lookuptype="depth", sp.br=sp.br  )  # takes 14GB!
      iizna = which( !is.finite( sm$z ) ) 
      sm$z[ iizna ] = sm$z.lookup [iizna ]
      sm$z.lookup = NULL
      sm = sm[ which( is.finite( sm$z ) ), ]
      gc()

      # this breaks it down by year and so memory is not really a limitation
      sm$t = NA
      sm$t.lookup = habitat.lookup.simple( sm, p=p, vnames="t", lookuptype="temperature.weekly", sp.br=sp.br  )
      iit = which( abs( sm$t - sm$t.lookup) > 5 ) # potentially unreliable temperatures due to improper lon/lat or bad data
      sm$t[ iit ] =  sm$t.lookup[ iit ]
      
      iitna = which( ! is.finite( sm$t ) ) 
      sm$t[ iitna ] =  sm$t.lookup[ iitna ]
      sm$t.lookup = NULL
 
      sm = sm[ which( is.finite( sm$t ) ), ]
      gc()

      WsH = habitat.lookup.grouped( sm, p=p, lookuptype="all.data", sp.br=sp.br )
      names(WsH)[which( names(WsH)=="t")] = "tmean.annual"
      WsH$z = NULL
        
      sm = cbind( sm, WsH)
      rm (WsH); gc()


      # ---------------------------------
      # bring in fisheries stats
      sm = logbook.fisheries.stats.merge( sm )
      
      # iii =  which( is.finite( sm$totno ) )
      #  sm = sm[ iii, ]

      si = which( sm$totno > 0 )
      sn = which( sm$totno == 0)

      sm$presence = NA
      sm$presence[ si ] = 1
      sm$presence[ sn ] = 0

      # convert to quantile and then a z-score 
      sm$q = NA
      sm$q[si] = quantile.estimate ( sm$totno[si]  )  # convert to quantiles

      maxq = max( sm$q[ which( sm$q < 1 ) ] , na.rm=T )  # keep it from being > 1 
      sm$q[ which(sm$q==1) ] = maxq
      sm$qn = qnorm( sm$q )
 
	    regs = c("cfanorth", "cfasouth", "cfa4x" )
      sm$cfa = NA
      for (r in regs) {
        jj = filter.region.polygon( sm[, c("plon", "plat") ], region=r, planar=T, proj.type=p$internal.projection ) 
        sm$cfa[jj] = r
      }

      sm$total.landings.scaled = scale( sm$total.landings, center=T, scale=T )

      # convert non-zero values quantiles, 
      sm$z = log( sm$z )
      sm$sa[ which( !is.finite( sm$sa )  ) ] = median( sm$sa[ which( is.finite( sm$sa )  ) ] )
      # if ( exists("t.annual", sm) ) sm$tmean.annual = sm$t.annual

      save ( sm, file=fn, compress=T )
      
      return ( fn )

    }

  }

  

