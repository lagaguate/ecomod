
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

      sm = NULL
      if ( DS=="set.snowcrab.in.groundfish.survey" ) {
        if ( file.exists( fn) )  load( fn )
        return (sm)
      }

      loadfunctions( "groundfish", functionname="current.year.r" )
      loadfunctions( "groundfish", functionname="load.groundfish.environment.r" )

      ct = snowcrab.external.db( DS="det.snowcrab.in.groundfish.survey", vname=vname )
      if (nrow(ct)==0) return()

      uu = sum.data( ct, factors="id", variable="number" )
      names(uu) =c( "id", "n" )

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
      sm$n [ which( !is.finite( sm$n)) ] = 0 # assume to be real zeros as dervied from trawls
      sm$temp = NULL
      sm$sdepth = NULL
      sm$cftow = NULL
      
      sm$yr = convert.datecodes(sm$chron, "year")
      sm$julian = convert.datecodes(sm$chron, "julian")
      sm$weekno = floor(sm$julian/365*52) + 1 
      
      #----------------------------
      # look up missing environmental data
      sp.br=c(1, 5, 10) # distances to interpolate ... short-scale only (km) if no exact match
       
		# bring in time invariant features:: depth
			print ("Bring in depth")
      todrop = which(sm$z < 20 | sm$z > 500 )
      if (length( todrop) > 0 ) sm$z [todrop ] = NA
      sm$z = habitat.lookup.simple( sm,  p=p, vnames="z", lookuptype="depth" )
      sm$z = log( sm$z )
			
		  # bring in time varing features:: temperature
			print ("Bring in temperature")
      sm$t = habitat.lookup.simple( sm,  p=p, vnames="t", lookuptype="temperature.weekly" )

			# bring in all other habitat variables, use "z" as a proxy of data availability
			# and then rename a few vars to prevent name conflicts
			print ("Bring in all other habitat variables")
      sH = habitat.lookup.grouped( sm,  p=p, lookuptype="all.data", sp.br=sp.br )
      sH$z = NULL 
			sH$yr = NULL
			sH$weekno = NULL
      sH
      vars = names (sH )

      sm = cbind( sm, sH )
		
      # return planar coords to correct resolution
      sm = lonlat2planar( sm, proj.type=p$internal.projection )
     
      sm = sm[ which( is.finite( sm$z + sm$t + sm$substrate.mean ) ), ]
      gc()

      # convert non-zero values quantiles, 
      oo = which( !is.finite( sm$sa )  )
      if (length(oo)>0 ) sm$sa[oo ] = median( sm$sa, na.rm=TRUE )

	    regs = c("cfanorth", "cfasouth", "cfa4x" )
      sm$cfa = NA
      for (r in regs) {
        jj = filter.region.polygon( sm[, c("plon", "plat") ], region=r, planar=T, proj.type=p$internal.projection ) 
        sm$cfa[jj] = r
      }


      # ---------------------------------
      # bring in fisheries stats
      sm = logbook.fisheries.stats.merge( sm )
      
      save ( sm, file=fn, compress=T )
      
      return ( fn )

    }

  }

  

