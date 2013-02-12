
  metabolism.db = function( ip=NULL, DS="", p=NULL, yr=NULL ) {
  
 
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
		
			save( MR, file=fn, compress=T )
			return (fn)
    }
    
   
    # -----------

    if (DS %in% c( "metabolism", "metabolism.redo" ) ) {
 
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.metabolism.rdata" )
        
      if (DS=="metabolism") {
        MR = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( MR )
      }


      det = bio.db( DS="det" ) # size information, no, cm, kg
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
     
      # match sets and other data sources
      det = det[ which( det$id %in% unique( set$id) ), ]
      cat = cat[ which( cat$id %in% unique( set$id) ), ]
 
      sm = set [, c("id", "chron", "yr", "julian",  
                 "sa", "lon", "lat", "z", "t", "sal", "oxyml", "settype", "cf")]
      oo =  which( !duplicated(sm$id) )
      if (length(oo) > 0 ) sm = sm[ oo, ] 
   
      print( "Lookup of temperature and habitat data, prior to modelling") # used for Arrhenius correction
      
      sm = lonlat2planar( sm, proj.type=p$internal.projection )  # plon+plat required for lookups
      sm$z = habitat.lookup.simple( sm,  p=p, vnames="z", lookuptype="depth", sp.br=p$interpolation.distances   ) 
      sm$t = habitat.lookup.simple( sm,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 
  
    
      # summary stats for each trip.set
      sm$oxysat = compute.oxygen.saturation( t.C=sm$t, sal.ppt=sm$sal, oxy.ml.l=sm$oxyml)
 
      # compute a few stats (from cat)
      # cat$totno and cat$totmass have already been cf corrected ---> already in per km2
      qtotno  = sumById( ee=cat$totno , id=cat$id,  idnames=c("id","totno" ) ) # no/km^2
      qtotwgt = sumById( ee=cat$totmass, id=cat$id,  idnames=c("id","totwgt" ) ) # kg/km^2
      
      # stats derived from det  -- det$cf is a copy of set$cf ..identical 
      qtotwgt_d = sumById( ee=det$mass*det$cf, id=det$id,  idnames=c("id","totwgt_d" ) )
      qtotlen_d = sumById( ee=det$len*det$cf, id=det$id,  idnames=c("id","totlen_d" ) )
      qtotno_d  = sumById( ee=det$cf, id=det$id,  idnames=c("id","totno_d" ) )

      sm = merge(sm, qtotno, by=c("id"), sort=F, all.x=T, all.y=F)
      sm = merge(sm, qtotwgt, by=c("id"), sort=F,  all.x=T, all.y=F)
      sm = merge(sm, qtotwgt_d, by=c("id"), sort=F,  all.x=T, all.y=F)
      sm = merge(sm, qtotlen_d, by=c("id"), sort=F,  all.x=T, all.y=F)
      sm = merge(sm, qtotno_d, by=c("id"), sort=F,  all.x=T, all.y=F)

      rm(set); gc()
  
      det = merge( det, sm[,c("t","id")], by="id", all.x=T, all.y=F, sort=F )
      detmr = metabolic.rates ( det$mass, det$t, tK=10 )
      det = cbind( det, detmr )

      mr0 = sumById( ee=det$mr*det$cf, id=det$id,  idnames=c("id","mr" ) )
      mrA = sumById( ee=det$mrA*det$cf, id=det$id,  idnames=c("id","mrA" ) )
  
      # merge data together
      MR = merge(x=sm, y=mr0, by="id", all.x=T, all.y=F)
      MR = merge(x=MR, y=mrA, by="id", all.x=T, all.y=F)
   
      # calculate mass-specific rates in the whole sm
      MR$smr = MR$mr / MR$totwgt_d
      MR$smrA = MR$mrA / MR$totwgt_d

      MR$smr[ which(!is.finite(MR$smr)) ] = 0
      MR$smrA[ which(!is.finite(MR$smr)) ] = 0

      # MR = MR[ which(MR$mr >= 0) ,]
      # MR = MR[ which(MR$mrA >= 0) ,]
      # MR = MR[ which(MR$mrA >=0), ]
      
      MR$meanwgt = MR$totwgt / MR$totno
      MR$meanlen = MR$totlen / MR$totno
      
      save( MR, file=fn, compress=T )
      
      return (fn) 
    }

  }


