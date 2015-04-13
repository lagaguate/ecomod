
  condition.db = function( DS="", p=NULL, yr=NULL ) {
 
    if (DS %in% c( "condition", "condition.redo" ) ) {
 
      ddir = file.path( project.datadirectory("condition"), "data" )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
     
      infix = paste(p$spatial.domain, p$season, sep=".")

      fn = file.path( ddir, paste("set.condition", infix, "rdata", sep=".") )
        
      if (DS=="condition") {
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
  	
      # match sets and other data sources
      det = bio.db( DS="det" ) # kg/km^2, no/km^2
      det = det[ which( det$id %in% unique( set$id) ), ]
      det = det[, c("id", "spec_bio", "residual", "cfdet" ) ]
      
      # merge in taxa-specifc condition estimates ....
      sm = set[, c("id", "yr" )]  # # yr is a dummy variable so that sm remains a data frame
      for (tx in p$varstomodel ) {
        ii = taxonomy.filter.taxa( det$spec_bio, taxafilter=tx )
        if (is.null ( ii) ) {
          sm[,tx]= NA
          next()
        }
        smd = applyMean( det[ ii, c("id", "residual", "cfdet")] )
        names(smd) = c( "id", tx ) 
        sm = merge( sm, smd, by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      }
      sm$yr = NULL
      set = merge( set, sm, by="id", all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c("", ".sm") )
      
      set = habitat.lookup( set, p=p, DS="environmentals" )
      
      save( set, file=fn, compress=T )
      return (fn) 
    }
  }


