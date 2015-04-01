
  lobster.db = function( DS="", p="" ) {
    
    if (DS %in% c("lfa41", "lfa41.redo") ) {
      fn = file.path( project.datadirectory("lobster"), "data", "lfa41.rdata" )
      if ( DS=="lfa41") {
        load( fn)
        return (lob)
      }
      fn.lfa41 = file.path( project.datadirectory("lobster"), "data", "lfa41.txt" )
      lob = read.table( fn.lfa41, header=T, as.is=T, sep="\t" )
      names(lob) = tolower( names(lob))
      names(lob) [which(names(lob)=="ddlat")] = "lat"
      names(lob) [which(names(lob)=="ddlon")] = "lon"
      names(lob) [which(names(lob)=="date_fished")] = "date"
      lob$lon = -lob$lon
      lob$z = log( lob$depth_fm * 1.8288  )  # natural log by default
      lob$depth_fm = NULL
      lob$catchrate = lob$lobster / lob$trap_hauls
      lob$chron = chron( dates.=lob$date, times.="12:00:00", format=c(dates="d/m/y",  times = "h:m:s"), out.format=c( "year-m-d", "h:m:s") )
      lob$yr = convert.datecodes(lob$chron, "year")
      lob$julian = convert.datecodes(lob$chron, "julian")
      
      lob$id = paste( lob$cfv_no, rownames(lob), sep="~" )

      # drop data with geographics (n=3)
      i = which(lob$lon > - 10 |lob$lat < 10   )
      lob = lob[-i ,]

      lb = quantile(  lob$catchrate[ which(lob$catchrate>0)], probs=p$threshold.quantile, na.rm=T )
      lob$PA = 0
      lobster.present = which( lob$catchrate > lb )  ## many NA's which represent no catches 
      lob$PA[ lobster.present ] = 1
      lob$percentile = quantile.estimate( lob$catchrate  )  # convert to percentiles
      lob$percentile[ which( !is.finite(  lob$percentile ) ) ] = 0

      lob$source = "lobster"
      lob$abundance.index.raw = lob$catchrate

      save( lob, file=fn, compress=T ) 
      return( lob )
    }

    if ( DS %in% c("snowcrab", "snowcrab.redo") ) {
      fn = file.path( project.datadirectory("lobster"), "data", "lobster.from.snowcrab.surveys.rdata" )
      if (DS =="snowcrab") {
        load(fn)
        return(sc)
      }
      ca = snowcrab.db( DS ="cat.georeferenced" ) 
      ca$id = paste( ca$trip, ca$set, sep="." )
  
      ca.lob = ca[ which( ca$spec %in% p$taxa.codes ) , c("id", "totno") ] 
      
      sc = snowcrab.db( DS ="set.complete" ) 
      sc$id = paste( sc$trip, sc$set, sep="." )

      i = which(duplicated( sc$id ))
      if (length(i)>0  ) stop()
      sc = merge( sc, ca.lob, by="id", all.x=T, all.y=F, sort=F)
      sc$PA = 0
      lb = quantile(  sc$totno[ which(sc$totno>0)], probs=p$threshold.quantile, na.rm=T )
      lobster.present = which( sc$totno > lb ) 
      sc$PA[ lobster.present ] = 1
      sc$percentile = quantile.estimate( sc$totno  )  # convert to percentiles
      sc$percentile[ which( !is.finite(  sc$percentile ) ) ] = 0
      sc$source = "snowcrab"
      sc$abundance.index.raw = sc$totno

      save( sc, file=fn, compress=T )
      return (sc) 
    } 
 
    # --------------------

    if (DS %in% c("groundfish", "groundfish.redo") ) {
      fn = file.path( project.datadirectory("lobster"), "data", "lobster.from.groundfish.surveys.rdata" )
      if (DS =="groundfish") {
        load(fn)
        return(gf)
      }
      # groundfish 
      # there was a periods where lobster was not recorded 
      # and these records may need to be thrown out **************

      set = groundfish.db( "set.base" )
      i=which(duplicated( set$id ))
      if (length(i) >0) set = set[ -i ,]

      cat = groundfish.db( "cat" )
      cat = cat[, c("spec", "id", "totwgt", "totno") ]
      cat = cat[ which( cat$spec %in% p$taxa.codes ) ,]
      i=which(duplicated( cat$id ))
      cat = cat[ -i ,]
      gf = merge(set, cat, by="id", sort=F, all.x=T, all.y=F)
      gf$PA = 0
      lb = quantile(  gf$totwgt[ which(gf$totwgt>0)], probs=p$threshold.quantile, na.rm=T )
      lobster.present = which( gf$totwgt > lb )  
      gf$PA[ lobster.present ] = 1
      gf$z = log( gf$sdepth)
      gf$t = gf$temp
   
      gf$percentile = quantile.estimate( gf$totwgt  )  # convert to percentiles
      gf$percentile[ which( !is.finite(  gf$percentile ) ) ] = 0

      gf$source = "groundfish"
      gf$abundance.index.raw = gf$totwgt 

      save( gf, file=fn, compress=T )
      return (gf) 
    } 
    
    # --------------------

    if (DS %in% c("complete", "complete.redo") ) {
      fn = file.path( project.datadirectory("lobster"), "data", "lobster.complete.rdata" )
      if (DS =="complete") {
        load(fn)
        return(L)
      }

      lob = lobster.db( "lfa41", p=p)
      lob$t = NA
      lob$sal = NA
      lob$oxysat = NA
      lob$substrate.mean = NA
      lob$dZ = NA
      lob$ddZ = NA
      lob$tmean = NA
      lob$tamp = NA
      lob$wmin = NA
      lob$thp = NA
      lob$tsd = NA
      lob$tmean.cl = NA
      lob$tamp.cl = NA
      lob$wmin.cl = NA
      
      sc = lobster.db( "snowcrab", p=p)
      sc$sal = NA
      sc$oxysat = NA

      gf = lobster.db( "groundfish", p=p)
      gf$substrate.mean = NA
      gf$dZ = NA
      gf$ddZ = NA
      gf$tmean = NA
      gf$tamp = NA
      gf$wmin = NA
      gf$thp = NA
      gf$tsd = NA
      gf$tmean.cl = NA
      gf$tamp.cl = NA
      gf$wmin.cl = NA
      
      tokeep = c( "id", "source", "lon", "lat",  "chron", "yr", "julian", "z", "t", "sal",
        "oxysat", "abundance.index.raw", "PA", "percentile",  "substrate.mean", "dZ", "ddZ", "tmean", 
        "tamp", "wmin", "thp", "tsd", "tmean.cl", "tamp.cl", "wmin.cl", "tsd.cl" ) 
      L = rbind( lob[,tokeep], gf[,tokeep], sc[,tokeep] )

      save( L, file=fn, compress=T )
      return(L)
    } 


  }



