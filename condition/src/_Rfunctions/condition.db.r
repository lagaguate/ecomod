
  condition.db = function( ip=NULL, DS="", p=NULL, yr=NULL ) {
  
 
    if (DS %in% c( "condition", "condition.redo" ) ) {
 
      ddir = file.path( project.directory("condition"), "data", p$spatial.domain, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.condition.rdata" )
        
      if (DS=="condition") {
        sm = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( sm )
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
      set = lonlat2planar( set, proj.type=p$internal.projection, ndigits=2 )
      set$platplon = paste( round( set$plat ), round(set$plon), sep="_" )
   
      # match sets and other data sources
      det = det[ which( det$id %in% unique( set$id) ), ]

      # merge in taxa-specifc condition estimates ....
      sm = set[, "id"]
      for (tx in p$varstomodel ) {
        ii = spec.filter( det$spec, tx )
        sm = merge( sm, 
          applyMean( det[ , c("id", nv, "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
 
        applyMean() 
        set = merge( set, s )
      }

    

      # averages of these variables from det
      newvars = c( "residual"  ) 
      for ( nv in newvars ) {
     }


      set = merge( set, sm, by ="id", all.x=TRUE, all.y=FALSE, sort=FALSE )




      set = set[ which( set$id %in% unique( c(unique( det$id) )) ),]
    

      
      save( sm, file=fn, compress=T )
      return (fn) 
    }
    
  }


