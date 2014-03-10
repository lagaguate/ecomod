
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

      # filter area
      igood = which( set$lon >= p$corners$lon[1] & set$lon <= p$corners$lon[2] 
            &  set$lat >= p$corners$lat[1] & set$lat <= p$corners$lat[2] )
      if (length(igood)>0) set = set[igood, ]

      if ( p$season != "allseasons" ) {
        set = set[ filter.season( set$julian, period=p$season, index=T ) , ]
      }

      # last filter on set:: filter years
      set = set[ which(set$yr %in% p$yearstomodel) , ]
  
      oo = which(!is.finite( set$plon+set$plat ) )
      if (length(oo)>0)  set = set[ -oo, ]  # a required field for spatial interpolation
    
      set = lonlat2planar( set, proj.type=p$internal.projection, ndigits=2 )
      set$platplon = paste( round( set$plat ), round(set$plon), sep="_" )
	 
      truncate.variables = FALSE 
      if (truncate.variables) {
        probs = c(0.005, 0.995)
        for (ww in p$varstomodel) {
          qnts = quantile(  set[ which(set[,ww]>0) , ww], probs=c(0, 0.95), na.rm=T ) # quantiles of positive valued data
          iq0 = which(set[,ww] < qnts[1])
          if (length(iq0) > 0)  set[ iq0, ww] = qnts[1] / 2  # assume this is the detection limit   
          iq1 = which(set[,ww] > qnts[2])
          if (length(iq1) > 0) set[ which(set[,ww] > qnts[2]), ww] = qnts[2]   
        }
      }
   
      save( set, file=fn, compress=T )
      return (fn) 
    }

  }


