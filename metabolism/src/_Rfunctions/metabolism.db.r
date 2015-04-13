
  metabolism.db = function( DS="", p=NULL, yr=NULL ) {
  
    if (DS %in% c( "metabolism", "metabolism.redo" ) ) {
 
      ddir = file.path( project.datadirectory("metabolism"), "data" )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, paste( "set.metabolism",  p$spatial.domain, p$taxa, p$season, ".rdata", sep=".") )
        
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
	
      set = habitat.lookup( set, p=p, DS="environmentals" )

      save( set, file=fn, compress=T )
      return (fn) 
    }

  }


