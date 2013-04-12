
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


      det = bio.db( DS="det" ) # size information, no, cm, kg
      set = bio.db( DS="set" ) # kg/km^2, no/km^2

      # filter area
      igood = which( set$lon >= p$corners$lon[1] & set$lon <= p$corners$lon[2] 
            &  set$lat >= p$corners$lat[1] & set$lat <= p$corners$lat[2] )
      if (length(igood)>0) set = set[igood, ]
      set = set[ which( set$id %in% unique( c(unique( det$id) )) ),]
      
      if ( p$season != "allseasons" ) {
        set = set[ filter.season( set$julian, period=p$season, index=T ) , ]
     }

      # last filter on set:: filter years
      set = set[ which(set$yr %in% p$yearstomodel) , ]
      set = lonlat2planar( set, proj.type=p$internal.projection, ndigits=2 )
      set$platplon = paste( round( set$plat ), round(set$plon), sep="_" )
      
      set = set [, c("id", "chron", "yr", "julian",  
                 "sa", "platplon", "z", "t", "sal", "oxyml", "oxysat", "settype")]
  

      # match sets and other data sources
      det = det[ which( det$id %in% unique( set$id) ), ]
		  det = det[ -which( !is.finite( det$mass + det$len + det$spec + det$cf ) ), ]
      det$sex[ which( !is.finite(det$sex)) ] = 2 # unknown sexes 
              
        # sex codes (snowcrab standard)
        #  male = 0 
        #  female = 1
        #  sex.unknown = 2

     
      sm = merge( det, set, by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
     
      sm = sm[, c( "id", "platplon", "yr", "julian", "chron", "z", "t", "sal", "oxysat", "cf",
          "sex", "mass", "len", "spec", "mat", "sa" )]

      save( sm, file=fn, compress=T )
      return (fn) 
    }
 
    # -----------


    
    if (DS %in% c( "condition.merged", "condition.merged.redo" ) ) {
 		
      require( chron) 

      ddir = file.path( project.directory("condition"), "data", p$spatial.domain, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.condition.merged.rdata" )
        
      if (DS=="condition.merged") {
        CO = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( CO )
      }
      
      P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
			P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )

      sm =  condition.db( DS="condition", p=p )
      

      CO = merge( sm, P0, by="platplon", all.x=T, all.Y=F, sort= F, suffixes=c("",".P0") )
      CO$z.P0 = NULL
			
      CO = CO[ which( is.finite( CO$plon ) ) , ]  # a required field for spatial interpolation
      CO = CO[ which( is.finite( CO$plat ) ) , ]  # a required field for spatial interpolation

      save( CO, file=fn, compress=T )
			return (fn)
    }
    
  }


