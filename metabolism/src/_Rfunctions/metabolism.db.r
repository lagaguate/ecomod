
  metabolism.db = function( DS="", p=NULL, yr=NULL ) {
  
    if (DS %in% c( "metabolism", "metabolism.redo" ) ) {
 
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.metabolism.rdata" )
        
      if (DS=="metabolism") {
        set = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( set )
      }

      set = bio.db( DS="set" ) # kg/km^2, no/km^2
      # set = set[ which( set$data.source=="groundfish") , ]

      # filter area
      igood = which( set$lon >= p$corners$lon[1] & set$lon <= p$corners$lon[2] 
            &  set$lat >= p$corners$lat[1] & set$lat <= p$corners$lat[2] )
      if (length(igood)>0) set = set[igood, ]

      if ( p$season != "allseasons" ) {
        set = set[ filter.season( set$julian, period=p$season, index=T ) , ]
      }

      # last filter on set:: filter years
      set = set[ which(set$yr %in% p$yearstomodel) , ]
     
      
      set = lonlat2planar( set, proj.type=p$internal.projection, ndigits=2 )
      set$platplon = paste( round( set$plat ), round(set$plon), sep="_" )
   
      truncate.variables = FALSE 
      if (truncate.variables) {
        # range limits:
          ultotwgt=10^5; set$totwgt[ which(set$totwgt > ultotwgt )] = ultotwgt
          ultotno =10^6; set$totno [ which(set$totno  > ultotno )]  = ultotno
          ulmr    =10^2; set$mr    [ which(set$mr     > ulmr )]     = ulmr
          ulmrA   =10^2; set$mrA   [ which(set$mrA    > ulmrA )]    = ulmrA
#         ulsmr   =10^-2; set$smr   [ which(set$smr    > ulsmr )]    = ulsmr
#         ulsmrA  =10^-2; set$smrA  [ which(set$smrA   > ulsmrA )]   = ulsmrA
          ulmw    =5;    set$meanwgt[ which(set$meanwgt > ulmw )]   = ulmw
              
        probs = c(0.005, 0.995)
        for (ww in p$varstomodel) {
          qnts = quantile(  set[ which(set[,ww]>0) , ww], probs=c(0, 0.95), na.rm=T ) # quantiles of positive valued data
          
          iq0 = which(set[,ww] < qnts[1])
          if (length(iq0) > 0)  set[ iq0, ww] = qnts[1] / 2  # assume this is the detection limit   
          
          iq1 = which(set[,ww] > qnts[2])
          if (length(iq1) > 0) set[ which(set[,ww] > qnts[2]), ww] = qnts[2]   
        }
      }


      # bring in environmental data

      # P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
			# P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )


      # set = merge( set, P0, by="platplon", all.x=T, all.Y=F, sort= F, suffixes=c("",".P0") )
			oo = which(!is.finite( set$plon+set$plat ) )
      if (length(oo)>0)  set = set[ -oo, ]  # a required field for spatial interpolation
		  # rm(P0); gc()

      if (!exists( "z", set)) set$z = NA
			set$z = habitat.lookup.simple( set,  p=p, vnames="z", lookuptype="depth", sp.br=p$interpolation.distances ) 
			
      if (!exists( "t", set)) set$t = NA
      set$t = habitat.lookup.simple( set,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances )

# should move the rescaling elsewhere ... TODO ... and add mass estimated from allomtry with length ...ie after condition and length-weight modelling/analysis  
      # set$totno = set$totno / 10^3
      # set$totwgt = set$totwgt / 10^3
   
      set = habitat.lookup.data( p=p, sc=set, modtype="full" )

      save( set, file=fn, compress=T )
      
      return (fn) 
    }

  }


