
  metabolism.db = function( ip=NULL, DS="", p=NULL, yr=NULL ) {
  

    if (DS %in% c( "metabolism", "metabolism.redo" ) ) {
 
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.metabolism.rdata" )
        
      if (DS=="metabolism") {
        MR = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( MR )
      }


      set = bio.db( DS="set" ) # kg/km^2, no/km^2
      cat = bio.db( DS="cat" )

      # filter area
      igood = which( set$lon >= p$corners$lon[1] & set$lon <= p$corners$lon[2] 
            &  set$lat >= p$corners$lat[1] & set$lat <= p$corners$lat[2] )
      if (length(igood)>0) set = set[igood, ]

      # filter taxa
      # det$spec = taxa.specid.correct( det$spec )
      det = filter.taxa( det, method=p$taxa )
      cat = filter.taxa( cat, method=p$taxa )
      set = set[ which( set$id %in% unique( c(unique( det$id), unique(cat$id))) ),]
      
      if ( p$season != "allseasons" ) {
        set = set[ filter.season( set$julian, period=p$season, index=T ) , ]
     }

      # last filter on set:: filter years
      set = set[ which(set$yr %in% p$yearstomodel) , ]
   
      cat = cat[ which( cat$id %in% unique( set$id) ), ]
 
      set = set [, c("id", "chron", "yr", "julian",  
                 "sa", "lon", "lat", "z", "t", "sal", "oxyml", "oxysat", "settype", "cf")]
    
      MR = merge( set, ... ) ## any?

      save( MR, file=fn, compress=T )
      
      return (fn) 
    }

    # -----------



    if (DS %in% c( "metabolism.filtered", "metabolism.filtered.redo" ) ) {
 
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.metabolism.filtered.rdata" )
        
      if (DS=="metabolism.filtered") {
        MR = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( MR )
      }
      
      MR =  metabolism.db( DS="metabolism", p=p )

      MR = lonlat2planar( MR, proj.type=p$internal.projection, ndigits=2 )
      MR$platplon = paste( round( MR$plat ), round(MR$plon), sep="_" )

      MR = MR[, c( "id", "platplon", "yr", "julian", "chron", "z", "t", "sal", "oxysat", "cf",
          "totno", "totwgt", "mr", "mrA", "smr", "smrA", "meanwgt", "meanlen" )]
    
      # check for duplicates --- should not be required but just in case
      for ( y in p$yearstomodel ) {
        yy = which (MR$yr == y)
        ii = which( duplicated( MR$id[yy] ) )
        
        if (length(ii) > 0) {
          print( "The following sets have duplicated positions. The first only will be retained" )
          print( MR[yy,] [ duplicates.toremove( MR$id[yy] ) ] )
          MR = MR[ - ii,]
        }
      }
    
      truncate.variables = TRUE
      if (truncate.variables) {
        # range limits:
          ultotwgt=10^5; MR$totwgt[ which(MR$totwgt > ultotwgt )] = ultotwgt
          ultotno =10^6; MR$totno [ which(MR$totno  > ultotno )]  = ultotno
          ulmr    =10^2; MR$mr    [ which(MR$mr     > ulmr )]     = ulmr
          ulmrA   =10^2; MR$mrA   [ which(MR$mrA    > ulmrA )]    = ulmrA
#         ulsmr   =10^-2; MR$smr   [ which(MR$smr    > ulsmr )]    = ulsmr
#         ulsmrA  =10^-2; MR$smrA  [ which(MR$smrA   > ulsmrA )]   = ulsmrA
          ulmw    =5;    MR$meanwgt[ which(MR$meanwgt > ulmw )]   = ulmw
              
        probs = c(0.005, 0.995)
        for (ww in p$varstomodel) {
          qnts = quantile(  MR[ which(MR[,ww]>0) , ww], probs=c(0, 0.95), na.rm=T ) # quantiles of positive valued data
          
          iq0 = which(MR[,ww] < qnts[1])
          if (length(iq0) > 0)  MR[ iq0, ww] = qnts[1] / 2  # assume this is the detection limit   
          
          iq1 = which(MR[,ww] > qnts[2])
          if (length(iq1) > 0) MR[ which(MR[,ww] > qnts[2]), ww] = qnts[2]   
        }
      }
      save( MR, file=fn, compress=T )
      return (fn) 
    }
    
    
    # -----------
    
    if (DS %in% c( "metabolism.merged", "metabolism.merged.redo" ) ) {
 		
      require( chron) 

      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.metabolism.merged.rdata" )
        
      if (DS=="metabolism.merged") {
        MR = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( MR )
      }
      
      P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
			P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )

      mm =  metabolism.db( DS="metabolism.filtered", p=p )

      MR = merge( mm, P0, by="platplon", all.x=T, all.Y=F, sort= F, suffixes=c("",".P0") )
			MR = MR[ -which(!is.finite( MR$plon+MR$plat ) ) , ]  # a required field for spatial interpolation
		  rm(mm, P0); gc()

      if (!exists( "z", MR)) MR$z = NA
			MR$z = habitat.lookup.simple( MR,  p=p, vnames="z", lookuptype="depth", sp.br=p$interpolation.distances ) 
			
      if (!exists( "t", MR)) MR$t = NA
      MR$t = habitat.lookup.simple( MR,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances )

# should move the rescaling elsewhere ... TODO ... and add mass estimated from allomtry with length ...ie after condition and length-weight modelling/analysis  
      MR$totno = MR$totno / 10^3
      MR$totwgt = MR$totwgt / 10^3
		
			save( MR, file=fn, compress=T )
			return (fn)
    }
    
 
  }


