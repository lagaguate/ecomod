
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
    
      # check for duplicates
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
    
   
    if (DS %in% c( "metabolism", "metabolism.redo" ) ) {
 
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      
      fn = file.path( ddir, "set.metabolism.rdata" )
        
      if (DS=="metabolism") {
        MR = NULL
        if (file.exists( fn) ) load( fn ) 
        return ( MR )
      }


      loadfunctions("groundfish")

      det = groundfish.db( DS="det" ) # size information, no, cm, kg
      set = groundfish.db( "set" ) # kg/km^2, no/km^2
      
      set = set[ which(set$settype %in% c(1,2)) , ]  
        # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
        #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
        #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography

      # filter area
      igood = which( set$lon >= p$corners$lon[1] & set$lon <= p$corners$lon[2] 
            &  set$lat >= p$corners$lat[1] & set$lat <= p$corners$lat[2] )
      set = set[igood, ]

      # filter taxa
      # det$spec = taxa.specid.correct( det$spec, method=p$taxa )
      det = filter.taxa( det, method=p$taxa )
      set = set[ which( set$id %in% unique( det$id) ),]
      
      if ( p$season != "allseasons" ) {
        set = set[ filter.season( set$julian, period=p$season, index=T ) , ]
        det = det[ which( det$id %in% unique( set$id) ), ]
      }

      # filter years
      set = set[ which(set$yr %in% p$yearstomodel) , ]

      sm = set [, c("id", "chron", "yr", "julian", "strat", "dist", 
                 "sakm2", "lon", "lat", "sdepth", "temp", "sal", "oxyml", "settype", "cf")]
      sm = rename.df( sm, "sdepth", "z")
      sm = rename.df( sm, "temp", "t")
      sm = rename.df( sm, "sakm2", "sa")
      
      sm = sm[ !duplicated(sm$id) ,] 
   
      print( "Lookup of temperature and habitat data, prior to modelling") # used for Arrhenius correction
      
      sm = lonlat2planar( sm, proj.type=p$internal.projection )  # plon+plat required for lookups
      sm$z = habitat.lookup.simple( sm,  p=p, vnames="z", lookuptype="depth", sp.br=p$interpolation.distances   ) 
      sm$t = habitat.lookup.simple( sm,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 
  
      
      # save( sm, file="~/sm.tmp", compress= T )
      # load( "~/sm.tmp" )


      # summary stats for each trip.set
      sm$oxysat = compute.oxygen.saturation( t.C=sm$t, sal.ppt=sm$sal, oxy.ml.l=sm$oxyml)
 
      # compute a few stats (from set)
      # set$totno and set$totwgt have already been cf corrected ---> already in per km2
      qtotno  = sumById( ee=set$totno , id=set$id,  idnames=c("id","totno" ) ) # no/km^2
      qtotwgt = sumById( ee=set$totwgt, id=set$id,  idnames=c("id","totwgt" ) ) # kg/km^2
      
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


