
  groundfish.db = function(  DS="complete", p=NULL, taxa="all", r2crit=0.75, threshold=0, type="number", season="summer"  ) {
  
    loc = file.path( project.directory("groundfish"), "data" )
    DataDumpFromWindows = F
    if ( DataDumpFromWindows ) {
      project.directory("taxonomy") = loc = file.path("C:", "datadump")
    }
    dir.create( path=loc, recursive=T, showWarnings=F )
  
    if (DS %in% c("odbc.redo") ) {
      
      # ODBC data dump of groundfish tables
      groundfish.db( DS="gscat.odbc.redo" )
      groundfish.db( DS="gsdet.odbc.redo" )
      groundfish.db( DS="gsinf.odbc.redo" )
      groundfish.db( DS="gsmissions.odbc.redo" ) #  not working?
      
      update.infrequently = F
      if (update.infrequently) {
        # the following do not need to be updated annually
        groundfish.db( DS="gscoords.odbc.redo" )
        groundfish.db( DS="spcodes.odbc.redo" )
        groundfish.db( DS="gslist.odbc.redo" )
        groundfish.db( DS="gsstratum.odbc.redo" )
      }
    
    }

 # ----------------------

    if (DS %in% c("spcodes", "spcodes.odbc", "spcodes.redo", "spcodes.odbc.redo", "gstaxa", "gstaxa.redo"  ) ) {
      
      fnspc = file.path( loc, "spcodes.rdata" )
         
      if ( DS %in% c( "spcodes", "spcodes.odbc", "gstaxa" ) ) {
        load( fnspc )
        return( spcodes )
      }

      if ( DS %in% c( "spcodes.odbc.redo", "spcodes.redo", "gstaxa.redo" ) ) {
        require(RODBC)
        connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user,
            pwd=oracle.personal.password, believeNRows=F)
        spcodes =  sqlQuery(connect, "select * from groundfish.gsspecies", as.is=T) 
        odbcClose(connect)
        names(spcodes) =  tolower( names(spcodes) )
        save(spcodes, file=fnspc, compress=T)
        print("Should follow up with a refresh of the taxonomy.db " )
        return( fnspc )
      }
    }


    # --------------------

 
    if (DS %in% c("gscat", "gscat.redo", "gscat.odbc", "gscat.odbc.redo"  ) ) {
      
      fn = file.path( loc,"gscat.rdata")
      fncat = file.path( loc, "gscat0.rdata" )
      
      if ( DS=="gscat" ) {
        load( fn )
        return (gscat)
      }
      if ( DS=="gscat.odbc" ) {
        load( fncat )
        return(gscat)
      }

      if ( DS=="gscat.odbc.redo" ) {
        require(RODBC)
        connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, 
          pwd=oracle.personal.password, believeNRows=F)
        gscat = sqlQuery( connect, "select * from groundfish.gscat" )
        odbcClose(connect)
        names(gscat) =  tolower( names(gscat) )
        save(gscat, file=fncat, compress=T)
        return (fncat)
      }
      
      # else default 

      gscat = groundfish.db( DS="gscat.odbc")
      
      # update taxa codes to a clean state:
      gscat$spec = taxa.specid.correct( gscat$spec ) 
     
      min.number.observations.required = 3
      species.counts = as.data.frame( table( gscat$spec) )
      species.to.remove = as.numeric( as.character( species.counts[ which( species.counts$Freq < min.number.observations.required) , 1 ] ))

      ii = which( gscat$spec %in% species.to.remove )
      gscat = gscat[ -ii , ]

      gscat$id = paste(gscat$mission, gscat$setno, sep=".")
      gscat$id2 = paste(gscat$mission, gscat$setno, gscat$spec, sep=".")
  
	# 	ll = which( gscat$totno == 0 & gscat$totwgt == 0 ) 
		# 	length(ll) 

			ii = which( gscat$totwgt >= 9999 )  # default code for NAs -- 
      if (length(ii)>0) gscat$totwgt[ii] = NA 
    
			ii = which( gscat$totwgt >= 5000 )  # upper limit of realistic kg/set
      if (length(ii)>0) gscat$totwgt[ii] = 5000
      
			jj = which( gscat$totwgt == 0 )
			if (length(jj)>0) gscat$totwgt[jj] = NA

			kk = which( gscat$totno == 0 ) 
      if (length(kk)>0) gscat$totno[kk] = NA
       
	
      d = which(duplicated(gscat$id2))
      s = NULL
      for (i in d) {
        q = which(gscat$id2 == gscat$id2[i])
        r = which.max(gscat$totno[q])
				gscat$totno[r] = sum( gscat$totno[q], na.rm=T )
				gscat$totwgt[r] = sum( gscat$totwgt[q], na.rm=T )
				gscat$sampwgt[r] = sum( gscat$sampwgt[q], na.rm=T )
        s = c(s, q[-r])
      }
      gscat = gscat[-s,]
 
	
		mw = meansize.crude(Sp=gscat$spec, Tn=gscat$totno, Tw=gscat$totwgt )
		mw2 = meansize.direct() 
		mw = merge(mw, mw2, by="spec", all=T, sort=T, suffixes=c(".crude", ".direct") )
		# directly determined mean size has greater reliability --- replace
		mm = which( is.finite(mw$meanweight.direct))
		mw$meanweight = mw$meanweight.crude
		mw$meanweight[mm] = mw$meanweight.direct[mm]
		mw = mw[which(is.finite(mw$meanweight)) ,]


      ii = which( is.na(gscat$totno) & gscat$totwgt >  0 ) 
      
      print( "Estimating catches from mean weight information... slow ~ 5 minutes")

      if (length(ii)>0) {
        # replace each number estimate with a best guess based upon average body weight in the historical record
        uu = unique( gscat$spec[ii] )
        for (u in uu ) {
          os =  which( mw$spec==u ) 
          if (length( os)==0 ) next()
          toreplace = intersect( ii, which( gscat$spec==u) )
          gscat$totno[toreplace] = gscat$totwgt[toreplace] / mw$meanweight[os]
        }
      }

      jj = which( gscat$totno >  0 & is.na(gscat$totwgt) ) 
      if (length(jj)>0) {
        # replace each number estimate with a best guess based upon average body weight in the historical record
        uu = unique( gscat$spec[jj] )
        for (u in uu ) {
          os =  which( mw$spec==u ) 
          if (length( os)==0 ) next()
          toreplace = intersect( jj, which( gscat$spec==u) )
          gscat$totwgt[toreplace] = gscat$totno[toreplace] * mw$meanweight[os]
        }
      }


      # many cases have measurements but no subsampling info
      gscat$cfsampling = gscat$totwgt / gscat$sampwgt
      gscat$cfsampling[ which( !is.finite(gscat$cfsampling)) ] = 1 # can only assume everything was measured (conservative estimate)

      gscat = gscat[, c("id", "id2", "spec", "totwgt", "totno", "sampwgt", "cfsampling")] # kg, no/set

      save(gscat, file=fn, compress=T)
      return( fn )
    }

  # ----------------------


    if (DS %in% c("gsdet", "gsdet.redo", "gsdet.odbc", "gsdet.odbc.redo") ) {
      fn = file.path( loc,"gsdet.rdata")
      fndet = file.path( loc, "gsdet0.rdata")
    
    # --------- codes ----------------
    # sex: 0=?, 1=male, 2=female,  3=?
    # mat: 0=observed but undetermined, 1=imm, 2=ripening(1), 3=ripening(2), 4=ripe(mature), 
    #      5=spawning(running), 6=spent, 7=recovering, 8=resting
    # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
    #      4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
    #      6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
    # --------- codes ----------------


      if ( DS=="gsdet" ) {
        load( fn )
        return (gsdet)
      }
      if ( DS=="gsdet.odbc" ) {
        load( fndet )
        return (gsdet)
      }
    
      if ( DS=="gsdet.odbc.redo" ) {
        require(RODBC)
        connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, 
          pwd=oracle.personal.password, believeNRows=F)
        gsdet =  sqlQuery(connect, "select * from groundfish.gsdet")
        odbcClose(connect)
        names(gsdet) =  tolower( names(gsdet) )
        save(gsdet, file=fndet, compress=T)
        return (fndet)
      }

      gsdet = groundfish.db( DS="gsdet.odbc" )

      gsdet$id = paste(gsdet$mission, gsdet$setno, sep=".")
      gsdet$id2 = paste(gsdet$mission, gsdet$setno, gsdet$spec, sep=".")
      gsdet = gsdet[, c("id", "id2", "spec", "fshno", "fsex", "fmat", "flen", "fwt", "age") ]  
      names(gsdet)[which(names(gsdet)=="fsex")] = "sex"
      names(gsdet)[which(names(gsdet)=="fmat")] = "mat"
      names(gsdet)[which(names(gsdet)=="flen")] = "len" # cm
      names(gsdet)[which(names(gsdet)=="fwt")]  = "mass" # g
      save(gsdet, file=fn, compress=T)
      return( fn )
    }

 # ----------------------


    if (DS %in% c("gsinf", "gsinf.redo", "gsinf.odbc", "gsinf.odbc.redo") ) {
      fn = file.path( loc,"gsinf.rdata")
      fninf = file.path(loc, "gsinf0.rdata")
      
      if ( DS=="gsinf" ) {
        load( fn )
        return (gsinf)
      }
      if ( DS=="gsinf.odbc" ) {
        load( fninf )
        return (gsinf)
      }
      if ( DS=="gsinf.odbc.redo" ) {
        require(RODBC)
        connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, 
          pwd=oracle.personal.password, believeNRows=F)
        gsinf = sqlQuery(connect, "select * from groundfish.gsinf" )
        odbcClose(connect)
        names(gsinf) =  tolower( names(gsinf) )
        save(gsinf, file=fninf, compress=T)
        return (fninf )
      } 

      gsinf = groundfish.db( DS="gsinf.odbc" )
      names(gsinf)[which(names(gsinf)=="type")] = "settype"
      gsinf$strat = as.character(gsinf$strat)
      gsinf$strat[ which(gsinf$strat=="") ] = "NA"
      gsinf$id = paste(gsinf$mission, gsinf$setno, sep=".")
      d = which(duplicated(gsinf$id))
      if (!is.null(d)) write("error: duplicates found in gsinf")
      gsinf$lat = gsinf$slat/100
      gsinf$lon = gsinf$slong/100
      if (mean(gsinf$lon,na.rm=T) >0 ) gsinf$lon = - gsinf$lon  # make sure form is correct
      gsinf = convert.degmin2degdec(gsinf)
      gsinf$cftow = 1.75/gsinf$dist
      ft2m = 0.3048
      m2km = 1/1000
      nmi2mi = 1.1507794
      mi2ft = 5280
      gsinf$sakm2 = (41 * ft2m * m2km ) * ( gsinf$dist * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2
				oo = which( !is.finite(gsinf$sakm2 )) 
					gsinf$sakm2[oo] = median (gsinf$sakm2, na.rm=T)
				pp = which( gsinf$sakm2 > 0.09 )
					gsinf$sakm2[pp] = median (gsinf$sakm2, na.rm=T)


			gsinf = gsinf[, c("id", "sdate", "time", "strat","area", "dist", "cftow", "sakm2", "settype", "lon", "lat", "surface_temperature","bottom_temperature","bottom_salinity")]
      save(gsinf, file=fn, compress=T)
      return(fn)
    }

 # ----------------------


    if (DS %in% c("gshyd.profiles", "gshyd.profiles.redo", "gshyd.profiles.odbc" , "gshyd.profiles.odbc.redo" ) ) {
      # full profiles
      fn = file.path( loc,"gshyd.profiles.rdata")
      fnhyd = file.path( loc, "gshyd.profiles0.rdata")
      
      if ( DS=="gshyd.profiles" ) {
        load( fn )
        return (gshyd)
      }
      if ( DS=="gshyd.profiles.odbc" ) {
        load( fn )
        return (gshyd)
      }
      if ( DS=="gshyd.profiles.odbc.redo" ) {
        require(RODBC)
        connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, 
          pwd=oracle.personal.password, believeNRows=F)
        gshyd = sqlQuery(connect, "select * from groundfish.gshyd")
        odbcClose(connect)
        names(gshyd) =  tolower( names(gshyd) )
        gshyd$id = paste(gshyd$mission, gshyd$setno, sep=".")
        save(gshyd, file=fnhyd, compress=T)
        return (fnhyd)
      }
      
      gshyd = groundfish.db( DS="gshyd.profiles.odbc" )
      gshyd = gshyd[, c("id", "sdepth", "temp", "sal", "oxyml" )]
      save(gshyd, file=fn, compress=T)
      return( fn )
    }
      
 # ----------------------



    if (DS %in% c("gshyd", "gshyd.redo") ) {
      # hydrographic info at deepest point
      fn = file.path( loc,"gshyd.rdata")
      if ( DS=="gshyd" ) {
        load( fn )
        return (gshyd)
      }
      gshyd = groundfish.db( DS="gshyd.profiles" )
      nr = nrow( gshyd)

      deepest = NULL
      t = which( is.finite(gshyd$sdepth) )
      id = unique(gshyd$id)
      for (i in id) {
        q = intersect( which( gshyd$id==i), t )
        r = which.max( gshyd$sdepth[q] )
        deepest = c(deepest, q[r])
      }
      gshyd = gshyd[deepest,]
      
      oo = which( duplicated( gshyd$id ) )
      if (length(oo) > 0) stop( "Duplicated data in GSHYD" )

      gsinf = groundfish.db( "gsinf" ) 
      gsinf = gsinf[, c("id", "bottom_temperature", "bottom_salinity" ) ] 
      gshyd = merge( gshyd, gsinf, by="id", all.x=T, all.y=F, sort=F )
      
      ii = which( is.na( gshyd$temp) )
      if (length(ii)>0) gshyd$temp[ii] =  gshyd$bottom_temperature[ii]

      jj = which( is.na( gshyd$sal) )
      if (length(jj)>0) gshyd$sal[jj] =  gshyd$bottom_salinity[jj]

      gshyd$bottom_temperature = NULL
      gshyd$bottom_salinity = NULL


      gshyd$sal[gshyd$sal<5 ] = NA
      
      save(gshyd, file=fn, compress=T)
      return( "Done" )
    }

 # ----------------------



    if (DS %in% c("gshyd.georef", "gshyd.georef.redo") ) {
      # hydrographic info georeferenced
      fn = file.path( loc,"gshyd.georef.rdata")
      if ( DS=="gshyd.georef" ) {
        load( fn )
        return (gshyd)
      }
      gsinf = groundfish.db( "gsinf" ) 
      gsinf$date = as.chron( gsinf$sdate )
      gsinf$yr = convert.datecodes( gsinf$date, "year" )
      gsinf$dayno = convert.datecodes( gsinf$date, "julian")
      gsinf$weekno = ceiling ( gsinf$dayno / 365 * 52 )
      gsinf$longitude = gsinf$lon
      gsinf$latitude = gsinf$lat
      gsinf = gsinf[ , c( "id", "lon", "lat", "yr", "weekno", "dayno", "date" ) ]
      gshyd = groundfish.db( "gshyd.profiles" )
      gshyd = merge( gshyd, gsinf, by="id", all.x=T, all.y=F, sort=F )
      gshyd$sal[gshyd$sal<5]=NA
      save(gshyd, file=fn, compress=T)
      return( "Done" )
    }


 # ----------------------



    if (DS %in% c("gsstratum", "gsstratum.obdc.redo") ) {
      fn = file.path( loc,"gsstratum.rdata")
      if ( DS=="gsstratum" ) {
        load( fn )
        return (gsstratum)
      }
      require(RODBC)
      connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, 
          pwd=oracle.personal.password, believeNRows=F)
      gsstratum =  sqlQuery(connect, "select * from groundfish.gsstratum", as.is=T) 
      odbcClose(connect)
      names(gsstratum) =  tolower( names(gsstratum) )
      save(gsstratum, file=file.path(loc,"gsstratum.rdata"), compress=T)
      return( "Done" )
    }

  # ----------------------


    if (DS %in% c("gscoords", "gscoords.odbc.redo") ) {
      # detailed list of places, etc
      fn = file.path( loc,"gscoords.rdata")
      if ( DS=="gscoords" ) {
        load( fn )
        return (gscoords)
      }
      require(RODBC)
      connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, 
          pwd=oracle.personal.password, believeNRows=F)
      coords = sqlQuery(connect, "select * from mflib.mwacon_mapobjects", as.is=T) 
      odbcClose(connect)
      names(coords) =  tolower( names(coords) )
      save(coords, file=fn, compress=T)
      return( "Done" )
    }
 
 # ----------------------
 # ----------------------

 
   if (DS %in% c("gslist", "gslist.odbc.redo") ) {
      fn = file.path( loc,"gslist.rdata")
      if ( DS=="gslist" ) {
        load( fn )
        return (gslist)
      }
      require(RODBC)
      connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, 
          pwd=oracle.personal.password, believeNRows=F)
      gslist = sqlQuery(connect, "select * from groundfish.gs_survey_list")
      odbcClose(connect)
      names(gslist) =  tolower( names(gslist) )
      save(gslist, file=fn, compress=T)
      return( "Done" )
    }

 # ----------------------

    if (DS %in% c("gsmissions", "gsmissions.odbc.redo") ) {
      fnmiss = file.path( loc,"gsmissions.rdata")
      
      if ( DS=="gsmissions" ) {
        load( fnmiss )
        return (gsmissions)
      }
      
      require(RODBC)
      connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, 
          pwd=oracle.personal.password, believeNRows=F)
    # ----------------------
        gsmissions = sqlQuery(connect, "select * from nwags.gsmission_list")
        # gsmissions = sqlQuery(connect, "select MISSION, VESEL, CRUNO from groundfish.gsmissions")
        odbcClose(connect)
        names(gsmissions) =  tolower( names(gsmissions) )
        save(gsmissions, file=fnmiss, compress=T)
      return( fnmiss )
    }

 # ----------------------

    if (DS %in% c("set.base", "set.base.redo") ) {
      fn = file.path( project.directory("groundfish"), "R", "set.base.rdata")
      if ( DS=="set.base" ) {
        load( fn )
        return (set)
      }
      
      require(chron)

      gscat = groundfish.db( "gscat" ) #kg/set, no/set 
      gsinf = groundfish.db( "gsinf" ) 
      set = merge(x=gscat, y=gsinf, by=c("id"), all.x=T, all.y=F, sort=F) 
      rm (gscat, gsinf)     
   
      gshyd = groundfish.db( "gshyd" ) 
      set = merge(x=set, y=gshyd, by=c("id"), all.x=T, all.y=F, sort=F) 
      rm (gshyd)

      gstaxa = taxa.db( "life.history" ) 
      gstaxa = gstaxa[,c("spec", "name.common", "name.scientific", "itis.tsn" )]
      oo = which( duplicated( gstaxa$spec ) )
      if (length( oo) > 0 ) {
        gstaxa = gstaxa[ -oo , ]  # arbitrarily drop subsequent matches
        print( "NOTE -- Duplicated species codes in taxa.db(life.history) ... need to fix taxa.db, dropping for now " )
      }

      set = merge(x=set, y=gstaxa, by=c("spec"), all.x=T, all.y=F, sort=F) 
      rm (gstaxa)

      # initial merge without any real filtering
      save(set, file=file.path( project.directory("groundfish"), "R", "set0.rdata"), compress=T)  

			set = set[ - which( !is.finite( set$sdate)) ,]  # NED1999842 has no accompanying gsinf data ... drop it
      set$chron = as.chron(set$sdate)
      set$sdate = NULL
      set$yr = convert.datecodes(set$chron, "year" )
      set$julian = convert.datecodes(set$chron, "julian")

      save(set, file=fn, compress=T )

      return ( "Done" )
    }
    
     # ----------------------

    if (DS %in% c("det.base", "det.base.redo") ) {
      fn = file.path( project.directory("groundfish"), "R", "det.base.rdata")
      if ( DS=="det.base" ) {
        load( fn )
        return (det)
      }

      det = groundfish.db( "gsdet" )
      
      det = det[, c("id", "id2", "spec", "fshno", "sex", "mat", "len", "mass", "age") ]
      det$mass = det$mass / 1000 # convert from g to kg 
      
      det$mass = log10(det$mass) # log10(kg)
      det$len = log10(det$len)  # log10(cm)

      #qmass = quantile( det$mass, probs=c(0.005, 0.995), na.rm=T )
      # det$mass[ which( det$mass< qmass[1] | det$mass>qmass[2]) ] = NA

      #qlen = quantile( det$len, probs=c(0.005, 0.995), na.rm=T )
      # det$len[ which( det$len < qlen[1] | det$len>qlen[2]) ] = NA

      k = which( is.finite(det$len) & is.finite(det$mass)  )
      R = lm.resid( det[k ,c("mass", "len", "sex", "spec")], threshold=r2crit )
      det$residual = NA
      det$residual[k] = R$residual
      
      det$predicted.mass = NA
      det$predicted.mass[k] = R$predicted.mass

      i = which( !is.finite( det$predicted.mass)  )
      det$predicted.mass[i] = lm.mass.predict ( x=det[i,c("spec","sex","len")], lm=R$lm.summ, threshold=r2crit )
   
      i = which( is.finite(det$residual)  )
      det$pvalue = NA
      det$pvalue[i] = lm.pvalue ( x=det[i,c("spec","sex", "residual")], lm=R$lm.summ, threshold=r2crit )
      
      det$mass = 10^(det$mass)  # re-convert to (kg) (from log10(kg))
      det$len = 10^(det$len)  # re-convert to (kg) (from log10(kg))
      det$predicted.mass = 10^(det$predicted.mass)
      
      oo = which( !is.finite(det$mass) & is.finite(det$predicted.mass) )
      det$mass[oo] = det$predicted.mass[oo]

      save( det, file=fn, compress=T )
      return( "Done"  )
    }
 

 # ----------------------

    if (DS %in% c("set", "set.redo") ) {
      fn = file.path( project.directory("groundfish"), "R", "set.rdata")
      if ( DS=="set" ) {
        load( fn )
        return (set)
      }
      
      set = groundfish.db( "set.base" )  # kg/set, no/set
     
      # combine correction factors or ignore trapability corrections .. 
      # plaice correction ignored as they are size-dependent
      set = correct.vessel(set)
      
      # correction factors for sampling etc after determination of mass and len 
      # for missing data due to subsampling methodology
      # totals in the subsample that was taken should == sampwgt (in theory) but do not 
      # ... this is a rescaling of the sum to make it a proper subsample
      gsdet = groundfish.db( "det" )
      massTotSet = sumById( ee=gsdet$mass, id=gsdet$id2, idnames=c("id2","massTotdet" ) )  
      noTotSet = sumById( ee=rep(1,nrow(gsdet)), id=gsdet$id2, idnames=c("id2","noTotdet" ) )

#dim(set)
#[1] 184372     30

      set = merge( set, massTotSet, by="id2", all.x=T, all.y=F, sort=F )  # set-->kg/km^2, det-->km
      set = merge( set, noTotSet, by="id2", all.x=T, all.y=F, sort=F )    # set-->no/km^2, det-->no
 
#dim(set)
#[1] 184372     32

      set$cfsubsample =  set$totwgt/ set$massTotdet 
      set$cfsubsample [ which ( !is.finite( set$cfsubsample ) ) ] = 1  # assume no subsampling -- all weights determined from the subsample
      set$cfsubsample [ which ( set$cfsubsample==0) ] = 1  # assume no subsampling -- all weights determined from the subsample


      # at the set level, some species are not sampled even though sampwgt's are recorded
      # this makes the total biomass > than that estimated from "DET" 
      # a final correction factor is required to bring it back to the total biomass caught,
      # this must be aggregated across all species within each set :  
      # NOt used right now
      massX = sumById( ee=set$massTotdet*set$cfsubsample, id=set$id, idnames=c("id","massDET" ) )  
      massS = sumById( ee=set$totwgt, id=set$id, idnames=c("id","massSAMP" ) )  
      
      mm = merge( massX, massS, by="id", sort=F)
      mm$cfDet2Set = mm$massSAMP / mm$massDET  # multiplier used for det expansion
     
      set = merge( set, mm[, c("id", "cfDet2Set")], by="id", all.x=T, all.y=F, sort=F)

      set$cf = set$cfvessel * set$cfsampling / set$sakm2 
      # set[ which( !is.finite(set$cf)) ] 

      # the following conversion are done here as sakm2 s not available in "gscat"
      # .. needs to be merged before use from gsinf
      # surface area of 1 standard set: sa =  41 (ft) * N  (nmi); N==1.75 for a standard trawl
      # the following express per km2 and so there is no need to "correct"  to std tow.
      
      set$totwgt  = set$totwgt  * set$cf # convert kg/set to kg/km^2
      set$totno   = set$totno   * set$cf # convert number/set to number/km^2

      save(set, file=fn, compress=T )

      return ( "Done" )
    }
    


 # ----------------------



    if (DS %in% c("det", "det.redo") ) {
      fn = file.path( project.directory("groundfish"), "R", "det.rdata")
      if ( DS=="det" ) {
        load( fn )
        return (det)
      }
      set = groundfish.db( "set" ) # kg/set, no/set 
      set = set[, c("id2", "cf", "spec", "settype" )]
      set = set[ which(is.finite( set$spec+set$settype)) , ]
      if (any(duplicated( set$id) ) ) stop("Duplcated id's" )

      gsdet = groundfish.db( "det.base" )  # kg, cm
      gsdet = gsdet[, c("id", "id2", "fshno", "sex", "mat", "len", "mass", "age", "residual", "pvalue") ]
      det = merge(x=gsdet, y=set, by=c("id2"), all.x=T, all.y=F, sort=F)

      save( det, file=fn, compress=T )
      return( "Done"  )
    }
 


 # ----------------------

  
    if (DS %in% c("sm.base", "sm.base.redo") ) {
      fn = file.path( project.directory("groundfish"), "R", "sm.base.rdata")
      if ( DS=="sm.base" ) {
        load( fn )
        return ( sm )
      }
      sm = groundfish.db( "set")  
      sm = sm[, c("id", "chron", "yr", "julian", "strat", "dist", 
                 "sakm2", "lon", "lat", "sdepth", "temp", "sal", "oxyml", "settype", "cf")]

      sm = sm[ !duplicated(sm$id) ,] 
      sm$oxysat = compute.oxygen.saturation( t.C=sm$temp, sal.ppt=sm$sal, oxy.ml.l=sm$oxyml)
      save ( sm, file=fn, compress=T)
      return( "Done"  )
    }
     
     # ----------------------

    
    if (DS %in% c("catchbyspecies", "catchbyspecies.redo") ) {
     fn = file.path( project.directory("groundfish"), "R", "sm.catchbyspecies.rdata")
     if ( DS=="catchbyspecies" ) {
       load( fn )
       return ( sm )
     }
 
      sm = groundfish.db( "sm.base" ) [, c("id", "yr")] # yr to maintain data structure

      # add dummy variables to force merge suffixes to register
      sm$totno = NA
      sm$totwgt = NA
      sm$ntaxa = NA
      set = groundfish.db( "set" ) 
      set = set[ which(set$settype %in% c(1,2,5)) , ]  # required only here
  
    # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
    #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
    #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography

      set0 = set[, c("id", "spec", "totno", "totwgt")]
      rm(set); gc()
      for (tx in taxa) {
        print(tx)
        i = filter.taxa( x=set0$spec, method=tx, index=T )
        set = set0[i,]
        index = list(id=set$id)
        qtotno = tapply(X=set$totno, INDEX=index, FUN=sum, na.rm=T)
        qtotno = data.frame(totno=as.vector(qtotno), id=I(names(qtotno)))
        qtotwgt = tapply(X=set$totwgt, INDEX=index, FUN=sum, na.rm=T)
        qtotwgt = data.frame(totwgt=as.vector(qtotwgt), id=I(names(qtotwgt)))
        qntaxa = tapply(X=rep(1, nrow(set)), INDEX=index, FUN=sum, na.rm=T)
        qntaxa = data.frame(ntaxa=as.vector(qntaxa), id=I(names(qntaxa)))
        qs = merge(qtotno, qtotwgt, by=c("id"), sort=F, all=T)
        qs = merge(qs, qntaxa, by=c("id"), sort=F, all=T)
        sm = merge(sm, qs, by=c("id"), sort=F, all.x=T, all.y=F, suffixes=c("", paste(".",tx,sep="")) )
      }
      sm$totno = NULL
      sm$totwgt = NULL
      sm$ntaxa = NULL
      sm$yr = NULL
      save ( sm, file=fn, compress=T)
      return( "Done"  )
    }


    # ----------------------


    if (DS %in% c("sm.det", "sm.det.redo") ) {
      fn = file.path( project.directory("groundfish"), "R", "sm_det.rdata")
      if ( DS=="sm.det" ) {
        load( fn )
        return ( sm )
      }
      
      require (Hmisc)
      sm = groundfish.db( "sm.base" ) [, c("id", "yr")] # yr to maintain data structure
      newvars = c("rmean", "pmean", "mmean", "lmean", "rsd", "psd", "msd", "lsd") 
      dummy = as.data.frame( array(data=NA, dim=c(nrow(sm), length(newvars) )))
      names (dummy) = newvars
      sm = cbind(sm, dummy)
      
      det = groundfish.db( "det" )       
     
      det = det[ which(det$settype %in% c(1, 2, 4, 5, 8) ) , ]
    # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
    #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
    #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
      det$mass = log10( det$mass )
      det$len  = log10( det$len )

      det0 = det[, c("id", "spec", "mass", "len", "age", "residual", "pvalue", "cf")]
      rm (det); gc()

      for (tx in taxa) {
        print(tx)
        if (tx %in% c("northernshrimp") ) next
        i = filter.taxa( x=det0$spec, method=tx, index=T )
        det = det0[i,]
        index = list(id=det$id)
        
        # using by or aggregate is too slow: raw computation is fastest using the fast formula: sd = sqrt( sum(x^2)-sum(x)^2/(n-1) ) ... as mass, len and resid are log10 transf. .. they are geometric means

        mass1 = tapply(X=det$mass*det$cf, INDEX=index, FUN=sum, na.rm=T)
        mass1 = data.frame(mass1=as.vector(mass1), id=I(names(mass1)))
        
        mass2 = tapply(X=det$mass*det$mass*det$cf, INDEX=index, FUN=sum, na.rm=T)
        mass2 = data.frame(mass2=as.vector(mass2), id=I(names(mass2)))

        len1 = tapply(X=det$len*det$cf, INDEX=index, FUN=sum, na.rm=T)
        len1 = data.frame(len1=as.vector(len1), id=I(names(len1)))
        
        len2 = tapply(X=det$len*det$len*det$cf, INDEX=index, FUN=sum, na.rm=T)
        len2 = data.frame(len2=as.vector(len2), id=I(names(len2)))

        res1 = tapply(X=det$residual*det$cf, INDEX=index, FUN=sum, na.rm=T)
        res1 = data.frame(res1=as.vector(res1), id=I(names(res1)))
        
        res2 = tapply(X=det$residual*det$residual*det$cf, INDEX=index, FUN=sum, na.rm=T)
        res2 = data.frame(res2=as.vector(res2), id=I(names(res2)))

        pv1 = tapply(X=det$pvalue*det$cf, INDEX=index, FUN=sum, na.rm=T)
        pv1 = data.frame(pv1=as.vector(pv1), id=I(names(pv1)))
        
        pv2 = tapply(X=det$pvalue*det$pvalue*det$cf, INDEX=index, FUN=sum, na.rm=T)
        pv2 = data.frame(pv2=as.vector(pv2), id=I(names(pv2)))

        ntot = tapply(X=det$cf, INDEX=index, FUN=sum, na.rm=T)
        ntot = data.frame(ntot=as.vector(ntot), id=I(names(ntot)))

        qs = NULL
        qs = merge(mass1, mass2, by=c("id"), sort=F, all=T)
        qs = merge(qs, len1, by=c("id"), sort=F, all=T)
        qs = merge(qs, len2, by=c("id"), sort=F, all=T)
        qs = merge(qs, res1, by=c("id"), sort=F, all=T)
        qs = merge(qs, res2, by=c("id"), sort=F, all=T)
        qs = merge(qs, pv1, by=c("id"), sort=F, all=T)
        qs = merge(qs, pv2, by=c("id"), sort=F, all=T)
        qs = merge(qs, ntot, by=c("id"), sort=F, all=T)

        qs$rmean = qs$res1/qs$ntot
        qs$pmean = qs$pv1/qs$ntot
        qs$mmean = qs$mass1/qs$ntot
        qs$lmean = qs$len1/qs$ntot
        
        # these are not strictly standard deviations as the denominator is not n-1 
        # but the sums being fractional and large .. is a close approximation
        # the "try" is to keep the warnings quiet as NANs are produced.
        qs$rsd = try( sqrt( qs$res2 - (qs$res1*qs$res1/qs$ntot) ), silent=T )
        qs$psd = try( sqrt( qs$pv2 - (qs$pv1*qs$pv1/qs$ntot) ), silent=T  )
        qs$msd = try( sqrt( qs$mass2 - (qs$mass1*qs$mass1/qs$ntot) ), silent=T  )
        qs$lsd = try( sqrt( qs$len2 - (qs$len1*qs$len1/qs$ntot)  ), silent=T  )
        
        qs = qs[, c("id","rmean", "pmean","mmean", "lmean", "rsd", "psd", "msd", "lsd")]
        sm = merge(sm, qs, by=c("id"), sort=F, all.x=T, all.y=F, suffixes=c("", paste(".",tx,sep="")) )
      }
      for (i in newvars) sm[,i]=NULL # these are temporary vars used to make merges retain correct suffixes

      sm$yr = NULL  # dummy var

      save ( sm, file=fn, compress=T)
      return( "Done"  )
    }
    
 # ----------------------

    if (DS %in% c("metabolic.rates","metabolic.rates.redo") ) {
      fn = file.path( project.directory("groundfish"), "R", "sm_mrate.rdata" )
      if (DS=="metabolic.rates") {
        load( fn)
        return (sm)
      }
      
      sm = groundfish.db( "sm.base" )      
      x = groundfish.db( "det" )
      x = x[ which(x$settype %in% c(1, 2, 4, 5, 8) ) , ]
    #  settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
    #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
    #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography

      x = merge(x, sm, by="id", all.x=T, all.y=F, suffixes=c("",".sm"), sort=F)

          
      # from Robinson et al. (1983) 
      # specific standard MR = 0.067 M^(-0.24) * exp(0.051 * Temp) 
      # (Temp in deg Celcius; M in grams, MR in ml O2/g/hr)
      b0 = 0.067
      b1 = -0.24
      b2 = 0.051
      
      # 1 ml O2 = 4.8 cal (from paper)
      # 1 W = 7537.2 kcal/yr
      
      # 1 ml O2 / g / hr = 4.8 * 24 * 365 kcal / yr / kg
      #                  = 4.8 * 24 * 365 / (7537.2 W) / kg
      #                  = 5.57873 W / kg
      
      from.ml.O2.per.g.per.hr.to.W.per.kg = 1 / 5.57873
      

    ###################### <<<<<<<<<<<<<<<<< Should bring in habitat variables here to assist with temperature .. lots of missing values!

      x$smr  = b0 * (x$mass*1000)^b1 * exp( b2 * 20.0) * from.ml.O2.per.g.per.hr.to.W.per.kg
      x$smrT = b0 * (x$mass*1000)^b1 * exp( b2 * x$temp) * from.ml.O2.per.g.per.hr.to.W.per.kg

      x$mr  = x$smr  * x$mass
      x$mrT = x$smrT * x$mass

      # Arrhenius correction
      # k = 10^8 # the constant
      # Ea = 10 # energy of activation (kcal/mol) 10 ~ q10 of 2; 25 ~ q10 of 4
      # R = 1.9858775 # gas constant cal/(mole.K)
      # x$mrT = x$mr * k * exp( - Ea * 1000 / (R * (x$temp + 273.150) ))

      prec = 10^5 # just a constant to get xtabs to work with a non-integer
      
      e = x$mr * x$cf  # correction factors required to make areal estimates 
      d = which( is.finite(e) & e> 0 )
        id = as.factor(x$id[d])
        l0 = prec / min(e[d] , na.rm=T)
        data = as.numeric(as.integer(e[d] * l0))
        mr = as.data.frame(xtabs(data ~ id) / l0)
      
      e = x$mrT * x$cf 
      d = which( is.finite(e) & e> 0 )
        id = as.factor(x$id[d])
        l0 = prec / min(e[d], na.rm=T)
        data = as.numeric(as.integer(e[d] * l0))
        mrT = as.data.frame(xtabs(data ~ id) / l0)
       
      e = x$mass * x$cf
      d = which( is.finite(e) & e> 0 )
        id = as.factor(x$id[d])
        l0 = prec / min(e[d], na.rm=T)
        data = as.numeric(as.integer(e[d] * l0))
        massTot = as.data.frame(xtabs(data ~ id) / l0)

      names(mr) = c("id","mr")
      names(mrT) = c("id","mrT")
      names(massTot) = c("id","massTot")

      mr$id = as.character(mr$id)
      mrT$id = as.character(mrT$id)
      massTot$id = as.character(massTot$id)
      mr$mr = as.numeric(mr$mr)
      mrT$mrT = as.numeric(mrT$mrT)
      massTot$massTot = as.numeric(massTot$massTot)
      
      # merge data together
      mr = merge(x=mr, y=mrT, by="id", all.x=T, all.y=F)
      mr = merge(x=mr, y=massTot, by="id", all.x=T, all.y=F)

      # calculate mass-specific rates in the whole set
      mr$smr = mr$mr / mr$massTot
      mr$smrT = mr$mrT / mr$massTot

      mr$smr[ which(mr$massTot==0) ] = NA
      mr$smrT[ which(mr$massTot==0) ] = NA

      mr = mr[mr$mr > 0 ,]
      mr = mr[mr$mrT > 0 ,]
      mr = mr[mr$massTot > 0 ,]
      mr = mr[is.finite(mr$mrT), ]

      mr.lm = summary(lm(log(mr$mr) ~ log(mr$massTot)))
      mrT.lm = summary(lm(log(mr$mrT) ~ log(mr$massTot)))

      mr$mrPvalue =  pnorm(q=mr.lm$residuals, sd=mr.lm$sigma)
      mr$mrPvalueT = pnorm(q=mrT.lm$residuals, sd=mrT.lm$sigma)

      sm = groundfish.db( "sm.base" ) [, c("id", "temp")]
      sm = merge( sm, mr, by=c("id"), sort=F, all.x=T, all.y=F )
      sm$temp = NULL
      save (sm, file=fn, compress=T)
      return( "Done"  )
 
    }

 # ----------------------

    if (DS %in% c("speciescomposition", "ordination.speciescomposition", "scores.speciescomposition", "speciescomposition.redo") ) {
    
      fn = file.path( project.directory("groundfish"), "R", "ordination.rdata" )
      fn.ord = file.path( project.directory("groundfish"), "R", "ordination.all.rdata" )
      fn.scores = file.path( project.directory("groundfish"), "R", "ordination.scores.rdata" )

      if ( DS=="speciescomposition" ) {
        load( fn )
        return (sm)
      } 
   
      if ( DS=="ordination.speciescomposition" ) {
        load( fn.ord )
        return (ord)
      } 
   
      if ( DS=="scores.speciescomposition" ) {
        load( fn.scores )
        return (scores)
      } 
   

    # rescale variables .. log transforms and express per km2 before ordinations
    # ...  large magnitude numbers are needed for ordinations

    # form datasets
    # numbers and weights have already been converted to per km2 and with vessel corrections
      k = 1e4         # a large constant number to make xtabs work
  
      x = groundfish.db( "set" )
      
      x = filter.taxa( x, method=p$taxa )
      
      x = x[ filter.season( x$julian, period=season, index=T ) , ]

      if (type == "number") {
        o = which(is.finite(x$totno))
        m = xtabs( as.numeric(as.integer(totno*k)) ~ as.factor(id) + as.factor(spec), data=x[o,] ) / k
      }
      if (type == "biomass") {
        o = which(is.finite(x$totwgt))
        m = xtabs(as.numeric( as.integer(totwgt*k)) ~ as.factor(id) + as.factor(spec), data=x[o,] ) / k
      }

      finished.j = finished.i = F
      while( !(finished.j & finished.i) ) {
        nr = nrow(m)
        nc = ncol(m)
        rowsm = rowSums(m)
        colsm = colSums(m)
        i = unique( c( which( rowsm/nr <= threshold ), which(rowsm==0 ) ))
        j = unique( c( which( colsm/nc <= threshold ), which(colsm==0 ) ))
        if (length(i) > 0 ) {
          m = m[ -i , ]
        } else {
          finished.i = T 
        }
        if (length(j) > 0 ) {
          m = m[ , -j ]
        } else {
          finished.j = T
        }
      }
      
      minval = min(m[m>threshold], na.rm=T)
      m = log( m + minval )
  
      ord = cca( m )
      sp.sc = scores(ord)$species
      si.sc = scores(ord)$sites

      scores = data.frame( 
        id=rownames(si.sc), 
        ca1=as.numeric(si.sc[,1]), 
        ca2=as.numeric(si.sc[,2]) 
      )
      scores$id = as.character(scores$id)

      save (scores, file=fn.scores, compress=T)
      save (ord, file=fn.ord, compress=T)
     
      sm = groundfish.db( "sm.base" ) [, c("id", "yr")]
      sm = merge(sm, scores, by="id", all.x=T, all.y=F, sort=F)
      sm$yr = NULL
      
      save (sm, file=fn, compress=T)
   
      print( ord$CA$eig[1:10]/sum(ord$CA$eig)*100 )

      cluster.analysis = F
      if (cluster.analysis) {
        X = t(m)
        gstaxa = taxa.db( "life.history" ) 
        names = NULL
        names$spec = as.numeric(dimnames(X)[[1]])
        names = merge(y=names, x=gstaxa, by="spec", all.y=T, all.x=F, sort=F)

        if (chisquared) {
          X = X/sum(X)
          rsums = rowSums(X)
          csums = colSums(X)
          rc = outer(rsums, csums)
          out = (X-rc)/sqrt(rc)
          d = out[lower.tri(out)]

          # mimic a "dist" class
          attr(d, "Size") <- nrow(X <- as.matrix(X))
          attr(d, "Labels") <- names$common
          attr(d, "Diag") <- F
          attr(d, "Upper") <- F
          attr(d, "method") <- "chisquare"
          attr(d, "call") <- "NA"
          class(d) <- "dist"
        }

        if (braycurtis) {
          attr(X, "dimnames")[[1]] = names$common
          d = vegdist(X, method="bray")
        }

        plot(hclust(d, "average"), cex=1); printfigure()
        plot(hclust(d, "ward"), cex=0.5); printfigure()
        plot(hclust(d, "complete"), cex=0.5); printfigure()
        plot(hclust(d, "single"), cex=0.5); printfigure()
        plot(hclust(d, "centroid"), cex=0.5); printfigure()
      
      }
      return ( "Done" )
    }

    # ----------------------

    if (DS %in% c("shannon.information", "shannon.information.redo" ) ) {
    
      fn = file.path( project.directory("groundfish"), "R", "sm_shannon_information.rdata" )

      if (DS=="shannon.information") {
        load( fn )
        return (sm)
      } 
      x = groundfish.db( "set" )
       
      # filter taxa
      x = filter.taxa( x, method=p$taxa )
      x = x[ filter.season( x$julian, period=season, index=T ) , ]

      qn = quantile( x$totno, probs=0.95, na.rm = T ) 
      x$totno [ which( x$totno > qn ) ] = qn

                    # numbers and weights have already been converted to per km2 and with vessel corrections
      k = 1e3         # a large constant number to make xtabs work
      o = which(is.finite(x$totno))
      x = x[o,]
      m = xtabs( as.numeric(as.integer(totno*k)) ~ as.factor(id) + as.factor(spec), data=x ) / k
      
      
      # remove low counts (absence) in the timeseries  .. species (cols) only
      cthreshold = 0.05 * k  # quantiles to be removed 

      finished = F
      while( !(finished) ) {
        i = unique( which(rowSums(m) == 0 ) )
        j = unique( which(colSums(m) <= cthreshold ) )
        if ( ( length(i) == 0 ) & (length(j) == 0 ) ) finished=T
        if (length(i) > 0 ) m = m[ -i , ]
        if (length(j) > 0 ) m = m[ , -j ]
      }

      si = shannon.diversity(m)
        
      sm = groundfish.db( "sm.base" ) [, c("id", "yr")]
      sm = merge( sm, si, by=c("id"), sort=F, all.x=T, all.y=F )
      sm$yr = NULL

      save(sm, file=fn, compress=T)
      return( "Done" )
    }

    # ----------------------

    if (DS %in% c("sm.complete", "sm.complete.redo") ) {
      fn = file.path( project.directory("groundfish"), "R", "sm.rdata")
      if ( DS=="sm.complete" ) {
        load( fn )
        return ( sm )
      }
        
      sm = groundfish.db( "sm.base" )
      sm = lonlat2planar(sm, proj.type=p$internal.projection ) # get planar projections of lon/lat in km
      

      # 1 merge catch
      sm = merge (sm, groundfish.db( "catchbyspecies" ), by = "id", sort=F, all.x=T, all.y=F )

      # 2 merge condition and other determined characteristics
      sm = merge (sm, groundfish.db( "sm.det" ), by = "id", sort=F, all.x=T, all.y=F )


	    # bring in time invariant features:: depth
			print ("Bring in depth")
      sm$sdepth = habitat.lookup.simple( sm,  p=p, vnames="sdepth", lookuptype="depth" )
      sm$z = sm$sdepth  # dummy var for later merges
      # sm$z = log( sm$z )
			
		  
      # bring in time varing features:: temperature
			print ("Bring in temperature")
      sm$temp = habitat.lookup.simple( sm,  p=p, vnames="temp", lookuptype="temperature.weekly" )

			# bring in all other habitat variables, use "z" as a proxy of data availability
			# and then rename a few vars to prevent name conflicts
			print ("Bring in all other habitat variables")
      
      sH = habitat.lookup.grouped( sm,  p=p, lookuptype="all.data", sp.br=seq(5, 25, 5) )
      sH = rename.df( sH, "z", "z.H" )
      sH = rename.df( sH, "zsd", "zsd.H" )
      sH = rename.df( sH, "t", "tmean.annual" )
      sH = rename.df( sH, "tsd", "tsd.annual" )
			sH$yr = NULL
			vars = names (sH )

      sm = cbind( sm, sH )
		
      # return planar coords to correct resolution
      sm = lonlat2planar( sm, proj.type=p$internal.projection )

      
      # 3 merge nss 
      source( file.path( project.directory("sizespectrum"), "src", "functions.sizespectrum.r" ) )
      nss = sizespectrum.db( DS="sizespectrum.stats.filtered", 
          p=list( spatial.domain="SSE", taxa="maxresolved", season="allseasons" ) )
      nss$strat = NULL
      if ( length(w) > 1 ) w = c( "id", setdiff( names(nss), names( sm) ) )
      sm = merge( sm, nss[,w], by="id", sort=FALSE )
      rm (nss)

      
      # 4 merge sar
      source( file.path( project.directory("speciesarea"), "src", "functions.speciesarea.r" ) )
      sar = speciesarea.db( DS="speciesarea.stats.filtered", 
          p=list( spatial.domain="SSE", taxa="maxresolved", season="allseasons" ) )
      if ( length(w) > 1 ) w = c( "id", setdiff( names(sar), names(sm)) )
      sm = merge( sm, sar[,w], by="id", sort=FALSE )
      rm (sar)


      # 5 merge metabolic rates
      source( file.path( project.directory("metabolism"), "src", "functions.metabolism.r" ) )
      meta = metabolism.db( DS="metabolism.filtered", 
          p=list( spatial.domain="SSE", taxa="alltaxa", season="allseasons" ) )
      if ( length(w) > 1 ) w = c( "id", setdiff( names(meta), names(sm)) )
      sm = merge( sm, meta[,w], by="id", sort=FALSE )
      rm (meta)

      # 6 merge species composition
      source( file.path( project.directory("speciescomposition"), "src", "functions.speciescomposition.r" ) )
      sc = speciescomposition.db( DS="speciescomposition.filtered", 
          p=list( spatial.domain="SSE", taxa="maxresolved", season="allseasons" ) )
      w = c( "id", setdiff( names(sc), names(sm)) )
      if ( length(w) > 1 ) sm = merge( sm, sc[,w], by="id", sort=FALSE )
      rm (sc)


      # 7 merge species diversity 
      si = groundfish.db( "shannon.information" )
      w = c( "id", setdiff( names(si), names(sm)) )
      if ( length(w) > 1 ) sm = merge( sm, si[,w], by="id", sort=F )
      
      
      # strata information
      gst = groundfish.db( DS="gsstratum" )
      w = c( "strat", setdiff( names(gst), names(sm)) )
      if ( length(w) > 1 ) sm = merge (sm, gst[,w], by="strat", all.x=T, all.y=F, sort=F)
      sm$area = as.numeric(sm$area)
      
      save ( sm, file=fn, compress=F )
      return( "Done" )
    }
    

  }


