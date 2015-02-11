
  groundfish.db = function(  DS="complete", p=NULL, taxa="all", datayrs=NULL  ) {
    loc = file.path( project.directory("groundfish"), "data" )
    DataDumpFromWindows = F
    if ( DataDumpFromWindows ) {
      project.directory("taxonomy") = loc = file.path("C:", "datadump")
    }
    dir.create( path=loc, recursive=T, showWarnings=F )
    
    if (DS %in% c("odbc.redo") ) {
      
      # ODBC data dump of groundfish tables
      groundfish.db( DS="gscat.odbc.redo", datayrs=datayrs )
      groundfish.db( DS="gsdet.odbc.redo", datayrs=datayrs )
      groundfish.db( DS="gsinf.odbc.redo", datayrs=datayrs )
      groundfish.db( DS="gshyd.profiles.odbc.redo", datayrs=datayrs )

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
        print( fnspc )
        print("Should follow up with a refresh of the taxonomy.db " )
        return( fnspc )
      }
    }


    # --------------------



		if (DS %in% c( "gscat.odbc", "gscat.odbc.redo" ) ) {
      
      fn.root =  file.path( project.directory("groundfish"), "data", "trawl", "gscat" )
			dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )
       
			out = NULL
	    if ( is.null(DS) | DS=="gscat.odbc" ) {
        fl = list.files( path=fn.root, pattern="*.rdata", full.names=T ) 
 				for ( fny in fl ) {
					load (fny)
					out = rbind( out, gscat )
				}
				return (out)
      }

      require(RODBC)
      connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

			for ( YR in datayrs ) {
				fny = file.path( fn.root, paste( YR,"rdata", sep="."))
        gscat = sqlQuery( connect,  paste( 
               "select i.*, substr(mission,4,4) year " , 
        "    from groundfish.gscat i " , 
        "    where substr(MISSION,4,4)=", YR, ";"
        ) )
     
        names(gscat) =  tolower( names(gscat) )
        print(fny)
        save(gscat, file=fny, compress=T)
				gc()  # garbage collection
				print(YR)
			}
   
      odbcClose(connect)             
      return (fn.root)

		}


    # --------------------


 
    if (DS %in% c("gscat", "gscat.redo"  ) ) {
       
      fn = file.path( loc,"gscat.rdata")
      
      if ( DS=="gscat" ) {
        load( fn )
        print('Not tow length corrected')
        return (gscat)
      }

      gscat = groundfish.db( DS="gscat.odbc" )
      gscat$year = NULL

      # remove data where species codes are ambiguous, or missing or non-living items
      xx = which( !is.finite( gscat$spec) ) 
      if (length(xx)>0) gscat = gscat[ -xx, ] 

      ii = taxonomy.filter.taxa( gscat$spec, taxafilter="living.only", outtype="groundfishcodes" )
      gscat = gscat[ ii , ]

      min.number.observations.required = 3
      species.counts = as.data.frame( table( gscat$spec) )
      species.to.remove = as.numeric( as.character( species.counts[ which( species.counts$Freq < min.number.observations.required) , 1 ] ))

      ii = which( gscat$spec %in% species.to.remove )
      gscat = gscat[ -ii , ]
      gscat$id = paste(gscat$mission, gscat$setno, sep=".")
      gscat$id2 = paste(gscat$mission, gscat$setno, gscat$spec, sep=".")
  

      # filter out strange data
			ii = which( gscat$totwgt >= 9999 )  # default code for NAs -- 
      if (length(ii)>0) gscat$totwgt[ii] = NA 
    
			ii = which( gscat$totwgt >= 5000 )  # upper limit of realistic kg/set
      if (length(ii)>0) gscat$totwgt[ii] = 5000
      
			jj = which( gscat$totwgt == 0 )
			if (length(jj)>0) gscat$totwgt[jj] = NA

			kk = which( gscat$totno == 0 ) 
      if (length(kk)>0) gscat$totno[kk] = NA
 		
      ll = which( is.na(gscat$totno) & is.na(gscat$totwgt) ) 
      if (length(ll) > 0) gscat$totno[ ll ] = 1
        
      # as species codes have been altered, look for duplicates and update totals	
      d = which(duplicated(gscat$id2))
      s = NULL
      for (i in d) {
        q = which(gscat$id2 == gscat$id2[i])
				gscat$totno[q[1]] = sum( gscat$totno[q], na.rm=T )
				gscat$totwgt[q[1]] = sum( gscat$totwgt[q], na.rm=T )
				gscat$sampwgt[q[1]] = sum( gscat$sampwgt[q], na.rm=T )
        s = c(s, q[2:length(q)])
      }
      if (length(s)>0) gscat = gscat[-s,]

      oo = which( duplicated( gscat$id2) )
      if ( length( oo )>0 ) {
        print( gscat[ oo , "id2"] )
        stop("Duplcated id2's in gscat"  )
      }

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
 
      gscat = gscat[, c("id", "id2", "spec", "totwgt", "totno", "sampwgt" )] # kg, no/set

      save(gscat, file=fn, compress=T)
      return( fn )
    }

   if (DS %in% c('gsdet.spec','gsdet.spec.redo')) {
          
             fn.root =  file.path( project.directory("groundfish"), "data")
             fi = 'gsdet.spec.rdata'
             dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )
          
          if(DS=='gsdet.spec'){
                load(file.path(fn.root,fi))
                return(species.details)
             }
          
             de  = groundfish.db(DS='gsdet.odbc')
             ins = groundfish.db(DS='gsinf.odbc')
             i1 = which(months(ins$sdate) %in% c('June','July','August'))
             i2 = which(months(ins$sdate) %in% c('February','March','April'))
             i3 = which(ins$strat %in% c(440:495))
             i4 = which(ins$strat %in% c(398:411))
             i5 = which(ins$strat %in% c('5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8','5Z9'))
             ins$series =NA
             ins$series[intersect(i1,i3)] <- 'summer'
             ins$series[intersect(i2,i4)] <- '4vswcod'
             ins$series[intersect(i2,i5)] <- 'georges'

             ins = ins[,c('mission','setno','series')]
             de = merge(de,ins,by=c('mission','setno'))
             de1 = aggregate(clen~spec+year+series,data=de,FUN=sum)
             de2 = aggregate(flen~spec+year+series,data=de,FUN=max)
             de3 = aggregate(flen~spec+year+series,data=de,FUN=min)
             de4 = aggregate(fwt~spec+year+series,data=de,FUN=length)
             species.details = merge(merge(merge(de1,de2,all.x=T,by=c('spec','year','series')),de3,all.x=T,by=c('spec','year','series')),de4,all.x=T,by=c('spec','year','series'))
             names(species.details) <- c('spec','year','series','number.lengths','max.length','min.length','number.weights')
             save(species.details,file=file.path(fn.root,fi))
             return(species.details)
      }


		if (DS %in% c( "gsdet.odbc", "gsdet.odbc.redo" ) ) {
      fn.root =  file.path( project.directory("groundfish"), "data", "trawl", "gsdet" )
			dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )
       
			out = NULL
	    if ( DS=="gsdet.odbc" ) {
        fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  ) 
 				for ( fny in fl ) {
					load (fny)
					out = rbind( out, gsdet )
				}
				return (out)
      }

      require(RODBC)
      connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

			for ( YR in datayrs ) {
				fny = file.path( fn.root, paste( YR,"rdata", sep="."))
        gsdet = sqlQuery( connect,  paste( 
        "select i.*, substr(mission,4,4) year" , 
        "    from groundfish.gsdet i " , 
        "    where substr(mission,4,4)=", YR, ";"
        ) )
        names(gsdet) =  tolower( names(gsdet) )
        gsdet$mission = as.character( gsdet$mission )
        save(gsdet, file=fny, compress=T)
        print(fny)
				gc()  # garbage collection
				print(YR)
			}
      odbcClose(connect)
              
      return (fn.root)

		}
        
    # ----------------------

    if (DS %in% c("gsdet", "gsdet.redo") ) {
    
    # --------- codes ----------------
    # sex: 0=?, 1=male, 2=female,  3=?
    # mat: 0=observed but undetermined, 1=imm, 2=ripening(1), 3=ripening(2), 4=ripe(mature), 
    #      5=spawning(running), 6=spent, 7=recovering, 8=resting
    # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
    #      4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
    #      6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
    # --------- codes ----------------


      fn = file.path( loc,"gsdet.rdata")
      
      if ( DS=="gsdet" ) {
        load( fn )
        return (gsdet)
      }

      gsdet = groundfish.db( DS="gsdet.odbc" )
      gsdet$year = NULL

      oo = which(!is.finite(gsdet$spec) )
      if (length(oo)>0) gsdet = gsdet[-oo,]
      
      # remove data where species codes are ambiguous, or missing or non-living items
      gsdet = gsdet[ taxonomy.filter.taxa( gsdet$spec, taxafilter="living.only", outtype="groundfishcodes" ) , ]


      gsdet$id = paste(gsdet$mission, gsdet$setno, sep=".")
      gsdet$id2 = paste(gsdet$mission, gsdet$setno, gsdet$spec, sep=".")
      gsdet = gsdet[, c("id", "id2", "spec", "fshno", "fsex", "fmat", "flen", "fwt", "age") ]  
      names(gsdet)[which(names(gsdet)=="fsex")] = "sex"
      names(gsdet)[which(names(gsdet)=="fmat")] = "mat"
      names(gsdet)[which(names(gsdet)=="flen")] = "len"  # cm
      names(gsdet)[which(names(gsdet)=="fwt")]  = "mass" # g
      save(gsdet, file=fn, compress=T)

      return( fn )
    }
  
    
    # ----------------------


		if (DS %in% c( "gsinf.odbc", "gsinf.odbc.redo" ) ) {
      
      fn.root =  file.path( project.directory("groundfish"), "data", "trawl", "gsinf" )
			dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )
       
			out = NULL
	    if ( is.null(DS) | DS=="gsinf.odbc" ) {
        fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  ) 
 				for ( fny in fl ) {
					load (fny)
					out = rbind( out, gsinf )
				}
				return (out)
      }

      require(RODBC)
      connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

			for ( YR in datayrs ) {
				fny = file.path( fn.root, paste( YR,"rdata", sep="."))
        gsinf = sqlQuery( connect,  paste( 
        "select gsinf.*, TO_CHAR(SDATE, 'YYYY-MM-DD HH24:MI:SS') SDATECHAR from groundfish.gsinf where EXTRACT(YEAR from SDATE) = ", YR, ";"
        ) )
        names(gsinf) =  tolower( names(gsinf) )
        save(gsinf, file=fny, compress=T)
        print(fny)
				gc()  # garbage collection
				print(YR)
			}
	        
      odbcClose(connect)	              
      return (fn.root)

		}
     
    

  # ----------------------


    if (DS %in% c("gsinf", "gsinf.redo" ) ) {
      fn = file.path( loc,"gsinf.rdata")
      
      if ( DS=="gsinf" ) {
        load( fn )
        return (gsinf)
      }
 
      gsinf = groundfish.db( DS="gsinf.odbc" )
      names(gsinf)[which(names(gsinf)=="type")] = "settype"
 
     
      if (FALSE) {
        # no longer needed .. 
        gsinf$SDATE = ymd_hms(gsinf$SDATECHAR) # use character as date conversions seems faulty ...
        gsinf$SDATECHAR = NULL
      
        # fix some time values that have lost the zeros due to numeric conversion
        gsinf$time = as.character( time( gsinf$sdate) )      
   
        j=nchar(gsinf$time)
        
        tooshort=which(j==3)
        if (length(tooshort)>0) gsinf$time[tooshort]=paste("0",gsinf$time[tooshort],sep="")
        tooshort=which(j==2)
        if (length(tooshort)>0) gsinf$time[tooshort]=paste("00",gsinf$time[tooshort],sep="")
        tooshort=which(j==1)
        if (length(tooshort)>0) gsinf$time[tooshort]=paste("000",gsinf$time[tooshort],sep="")
        
        hours=substring(gsinf$time,1,2)
        mins=substring(gsinf$time,3,4)
        secs="00"
        days = day( gsinf$sdate )
        mons = month( gsinf$sdate )
        yrs = year( gsinf$sdate )
        gsinf$timestamp = paste( gsinf$year, gsinf$mon, gsinf$day, hours, mins, secs, sep="-" )
      }
      
      tzone = "America/Halifax"  ## need to verify if this is correct
  
      #lubridate function 
      gsinf$timestamp = gsinf$sdate = ymd_hms(gsinf$sdatechar) 
      gsinf$edate = gsinf$etime

      #### TODO: and NOTE: Timestamps of "sdate" and "edate" are offset by 1 hr for some reason. 
      ### Perhaps some standard for the DB ..
      ### Here we want it in America/Halifax zone as this matches the scanmar time-stamps
      ###  see for example: gsinf[1:10, c("sdate", "time" ) ]
      ###  Worth following up with the groundfish people or Shelley
     
      # by default it should be the correct timezone ("localtime") , but just in case
      tz( gsinf$sdate) = "America/Halifax"  
      tz( gsinf$edate) = "America/Halifax"  
 
      # gsinf$sdate = gsinf$sdate - dhours(1) 
      # gsinf$edate = gsinf$etime - dhours(1) 
    
      gsinf$mission = as.character( gsinf$mission )
      gsinf$strat = as.character(gsinf$strat)
      gsinf$strat[ which(gsinf$strat=="") ] = "NA"
      gsinf$id = paste(gsinf$mission, gsinf$setno, sep=".")
      d = which(duplicated(gsinf$id))
      if (!is.null(d)) write("error: duplicates found in gsinf")

      gsinf$lat = gsinf$slat/100
      gsinf$lon = gsinf$slong/100
      gsinf$lat.end = gsinf$elat/100
      gsinf$lon.end = gsinf$elong/100

      if (mean(gsinf$lon,na.rm=T) >0 ) gsinf$lon = - gsinf$lon  # make sure form is correct
      if (mean(gsinf$lon.end,na.rm=T) >0 ) gsinf$lon.end = - gsinf$lon.end  # make sure form is correct

      gsinf = convert.degmin2degdec(gsinf)
      gsinf$cftow = 1.75/gsinf$dist  # not used
      ft2m = 0.3048
      m2km = 1/1000
      nmi2mi = 1.1507794
      mi2ft = 5280
      gsinf$sakm2 = (41 * ft2m * m2km ) * ( gsinf$dist * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled in km^2
				oo = which( !is.finite(gsinf$sakm2 )) 
					gsinf$sakm2[oo] = median (gsinf$sakm2, na.rm=T)
				pp = which( gsinf$sakm2 > 0.09 )
					gsinf$sakm2[pp] = median (gsinf$sakm2, na.rm=T)
      gsinf$bottom_depth = rowMeans( gsinf[, c("dmin", "dmax", "depth" )], na.rm = TRUE )  * 1.8288  # convert from fathoms to meters
      ii = which( gsinf$bottom_depth < 10 | !is.finite(gsinf$bottom_depth)  )  # error
      gsinf$bottom_depth[ii] = NA
			gsinf = gsinf[, c("id", "sdate", "edate", "timestamp", "strat", "area", "speed", "dist", 
                        "cftow", "sakm2", "settype", "lon", "lat", "lon.end", "lat.end",
                        "surface_temperature","bottom_temperature","bottom_salinity", "bottom_depth")]
      
      save(gsinf, file=fn, compress=T)
      return(fn)
    }

 # ----------------------


		if (DS %in% c( "gshyd.profiles.odbc" , "gshyd.profiles.odbc.redo" ) ) {
      
      fn.root =  file.path( project.directory("groundfish"), "data", "trawl", "gshyd" )
			dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )
       
			out = NULL
	    if ( is.null(DS) | DS=="gshyd.profiles.odbc" ) {
        fl = list.files( path=fn.root, pattern="*.rdata", full.names=T  ) 
 				for ( fny in fl ) {
					load (fny)
					out = rbind( out, gshyd )
				}
				return (out)
      }

      require(RODBC)
      connect=odbcConnect( oracle.groundfish.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

			for ( YR in datayrs ) {
				fny = file.path( fn.root, paste( YR,"rdata", sep="."))
        gshyd = sqlQuery( connect,  paste( 
        "select i.*, j.YEAR " , 
        "    from groundfish.gshyd i, groundfish.gsmissions j " , 
        "    where i.MISSION(+)=j.MISSION " ,
        "    and YEAR=", YR, ";"
        ) )
        names(gshyd) =  tolower( names(gshyd) )
        if(all(is.na(gshyd$mission))) {
        	#if gshyd is not loaded and the odf files are obtained AMC
		        fy <- file.path(project.directory("temperature"), "data", "archive", "ctd",YR)
		        o <- compileODF(path=fy)
		        gshyd <- makeGSHYD(o)
        }
        gshyd$mission = as.character( gshyd$mission )
        save(gshyd, file=fny, compress=T)
        print(fny)
				gc()  # garbage collection
				print(YR)
			}
			odbcClose(connect)
              
      return ( fn.root )

		}
     
 # ----------------------


   
    if (DS %in% c("gshyd.profiles", "gshyd.profiles.redo" ) ) {
      # full profiles
      fn = file.path( loc,"gshyd.profiles.rdata")
      if ( DS=="gshyd.profiles" ) {
        load( fn )
        return (gshyd)
      }
      
      gshyd = groundfish.db( DS="gshyd.profiles.odbc" )
      gshyd$id = paste(gshyd$mission, gshyd$setno, sep=".")
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
      
      # candidate depth estimates from profiles
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
      gsinf = gsinf[, c("id", "bottom_temperature", "bottom_salinity", "bottom_depth" ) ] 
      gshyd = merge( gshyd, gsinf, by="id", all.x=T, all.y=F, sort=F )
     
      ## bottom_depth is a profile-independent estimate .. asuming it has higher data quality
      ii = which(!is.finite( gshyd$bottom_depth ))
      if (length(ii)>0) gshyd$bottom_depth[ii] = gshyd$sdepth[ii]
      gshyd$sdepth = gshyd$bottom_depth        #overwrite
      ii = which( gshyd$sdepth < 10 )
      if (length(ii)>0) gshyd$sdepth[ii] = NA

      ii = which( is.na( gshyd$temp) )
      if (length(ii)>0) gshyd$temp[ii] =  gshyd$bottom_temperature[ii]

      jj = which( is.na( gshyd$sal) )
      if (length(jj)>0) gshyd$sal[jj] =  gshyd$bottom_salinity[jj]
      gshyd$sal[gshyd$sal<5 ] = NA

      gshyd$bottom_depth = NULL
      gshyd$bottom_temperature = NULL
      gshyd$bottom_salinity = NULL

      
      save(gshyd, file=fn, compress=T)
      return( fn )
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
      return( fn )
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
      save(gsstratum, file=fn, compress=T)
      print(fn)
      return( fn )
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
      print(fn)
      return( fn )
    }
 
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
      print(fn)
      return( fn )
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
        gsmissions = sqlQuery(connect, "select MISSION, VESEL, CRUNO from groundfish.gsmissions")
        odbcClose(connect)
        names(gsmissions) =  tolower( names(gsmissions) )
        save(gsmissions, file=fnmiss, compress=T)
      print(fnmiss)
      return( fnmiss )
    }

 # ----------------------

    if (DS %in% c("cat.base", "cat.base.redo") ) {
      fn = file.path( project.directory("groundfish"), "data", "cat.base.rdata")
      if ( DS=="cat.base" ) {
        load( fn )
        return (cat)
      }
      
      require(chron)

      gscat = groundfish.db( "gscat" ) #kg/set, no/set 
      set = groundfish.db( "set.base" ) 
      cat = merge(x=gscat, y=set, by=c("id"), all.x=T, all.y=F, sort=F) 
      rm (gscat, set)     
   
      gstaxa = taxonomy.db( "life.history" ) 
      gstaxa = gstaxa[,c("spec", "name.common", "name.scientific", "itis.tsn" )]
      oo = which( duplicated( gstaxa$spec ) )
      if (length( oo) > 0 ) {
        gstaxa = gstaxa[ -oo , ]  # arbitrarily drop subsequent matches
        print( "NOTE -- Duplicated species codes in taxonomy.db(life.history) ... need to fix taxonomy.db, dropping for now " )
      }

      cat = merge(x=cat, y=gstaxa, by=c("spec"), all.x=T, all.y=F, sort=F) 
      save(cat, file=fn, compress=T )
      return ( fn )
    }
    
     # ----------------------

    if (DS %in% c("det.base", "det.base.redo") ) {
      fn = file.path( project.directory("groundfish"), "data", "det.base.rdata")
      if ( DS=="det.base" ) {
        load( fn )
        return (det)
      }

      det = groundfish.db( "gsdet" )
      
      det = det[, c("id", "id2", "spec", "fshno", "sex", "mat", "len", "mass", "age") ]
      det$mass = det$mass / 1000 # convert from g to kg 

      save( det, file=fn, compress=T )
      return( fn )
    }
 

 # ----------------------

    if (DS %in% c("cat", "cat.redo") ) {
      fn = file.path( project.directory("groundfish"), "data", "cat.rdata")
      if ( DS=="cat" ) {
        load( fn )
        return (cat)
      }
     
      cat = groundfish.db( DS="cat.base" )  # kg/set, no/set
     
      # combine correction factors or ignore trapability corrections .. 
      # plaice correction ignored as they are size-dependent
      cat = correct.vessel(cat)  # returns cfvessel
     
      # many cases have measurements but no subsampling info  
      # ---- NOTE ::: sampwgt seems to be unreliable  -- recompute where necessary in "det"
      
      # the following conversion are done here as sakm2 s not available in "gscat"
      # .. needs to be merged before use from gsinf
      # surface area of 1 standard set: sa =  41 (ft) * N  (nmi); N==1.75 for a standard trawl
      # the following express per km2 and so there is no need to "correct"  to std tow.
      
      cat$totwgt  = cat$totwgt  * cat$cfset * cat$cfvessel # convert kg/set to kg/km^2
      cat$totno   = cat$totno   * cat$cfset * cat$cfvessel # convert number/set to number/km^2
 
      # cat$sampwgt is unreliable for most data points nned to determine directly from "det"
      cat$sampwgt = NULL
      
      # cat$cfsampling = cat$totwgt / cat$sampwgt
      # cat$cfsampling[ which( !is.finite(cat$cfsampling)) ] = 1 # can only assume everything was measured (conservative estimate)

     
      # cat$sampwgt =  cat$sampwgt * cat$cf   # keep same scale as totwgt to permit computations later on 
      
      save(cat, file=fn, compress=T )

      return (fn)
    }
    


 # ----------------------



    if (DS %in% c("det", "det.redo") ) {
      fn = file.path( project.directory("groundfish"), "data", "det.rdata")
      if ( DS=="det" ) {
        load( fn )
        return (det)
      }
 
      # determine weighting factor for individual-level measurements (len, weight, condition, etc)
      
      # at the set level, some species are not sampled even though sampwgt's are recorded
      # this makes the total biomass > than that estimated from "DET" 
      # an additional correction factor is required to bring it back to the total biomass caught,
      # this must be aggregated across all species within each set :  

      # correction factors for sampling etc after determination of mass and len 
      # for missing data due to subsampling methodology
      # totals in the subsample that was taken should == sampwgt (in theory) but do not 
      # ... this is a rescaling of the sum to make it a 'proper' subsample
      
      det = groundfish.db( "det.base" )  # kg, cm
      
      massTotCat = applySum( det[ ,c("id2", "mass")], newnames=c("id2","massTotdet" ) )  
      noTotCat = applySum( det$id2, newnames=c("id2","noTotdet" ) )  

      cat = groundfish.db( "cat" ) # kg/km^2 and  no/km^2 
      cat = cat[, c("id2", "totno", "totwgt", "cfset", "cfvessel" )]
      cat = merge( cat, massTotCat, by="id2", all.x=T, all.y=F, sort=F )  # set-->kg/km^2, det-->km
      cat = merge( cat, noTotCat, by="id2", all.x=T, all.y=F, sort=F )    # set-->no/km^2, det-->no
 
      cat$cfdet =  cat$totwgt/ cat$massTotdet  # totwgt already corrected for vessel and tow .. cfdet is the multiplier required to make each det measurement scale properly

      # assume no subsampling -- all weights determined from the subsample
      oo = which ( !is.finite( cat$cfdet ) |  cat$cfdet==0 )
      if (length(oo)>0) cat$cfdet[oo] = cat$cfset[oo] * cat$cfvessel[oo]  
      
      # assume remaining have an average subsampling effect
      oo = which ( !is.finite( cat$cfdet ) |  cat$cfdet==0 )
      if (length(oo)>0) cat$cfdet[oo] = median( cat$cfdet, na.rm=TRUE )  

      cat = cat[, c("id2", "cfdet")]
      det = merge( det, cat, by="id2", all.x=T, all.y=F, sort=F)

      save( det, file=fn, compress=T )
      return( fn  )
    }
 


 # ----------------------

  
    if (DS %in% c("set.base", "set.base.redo") ) {
      fn = file.path( project.directory("groundfish"), "data", "set.base.rdata")
      if ( DS=="set.base" ) {
        load( fn )
        return ( set )
      }
      
      gsinf = groundfish.db( "gsinf" ) 
      gshyd = groundfish.db( "gshyd" ) # already contains temp data from gsinf 
      
      set = merge(x=gsinf, y=gshyd, by=c("id"), all.x=T, all.y=F, sort=F) 
      rm (gshyd, gsinf)
    	
      oo = which( !is.finite( set$sdate)) # NED1999842 has no accompanying gsinf data ... drop it
      if (length(oo)>0) set = set[ -oo  ,]  

      set$chron = as.chron(set$sdate)
      set$sdate = NULL
      set$yr = convert.datecodes(set$chron, "year" )
      set$julian = convert.datecodes(set$chron, "julian")

      set = set[, c("id", "chron", "yr", "julian", "strat", "dist", 
                 "sakm2", "lon", "lat", "sdepth", "temp", "sal", "oxyml", "settype")]

      set = set[ !duplicated(set$id) ,] 
      set$oxysat = compute.oxygen.saturation( t.C=set$temp, sal.ppt=set$sal, oxy.ml.l=set$oxyml)
      set$cfset = 1 / set$sakm2

      save ( set, file=fn, compress=T)
      return( fn  )
    }
     
    # ----------------------

    
    if (DS %in% c("catchbyspecies", "catchbyspecies.redo") ) {
     fn = file.path( project.directory("groundfish"), "data", "set.catchbyspecies.rdata")
     if ( DS=="catchbyspecies" ) {
       load( fn )
       return ( set )
     }
 
      set = groundfish.db( "set.base" ) [, c("id", "yr")] # yr to maintain data structure

      # add dummy variables to force merge suffixes to register
      set$totno = NA
      set$totwgt = NA
      set$ntaxa = NA
      cat = groundfish.db( "cat" ) 
      cat = cat[ which(cat$settype %in% c(1,2,5)) , ]  # required only here
  
    # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
    #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
    #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography

      cat0 = cat[, c("id", "spec", "totno", "totwgt")]
      rm(cat); gc()
      for (tx in taxa) {
        print(tx)
        i = taxonomy.filter.taxa( cat0$spec, taxafilter=tx, outtype="groundfishcodes" )
        cat = cat0[i,]
        index = list(id=cat$id)
        qtotno = tapply(X=cat$totno, INDEX=index, FUN=sum, na.rm=T)
        qtotno = data.frame(totno=as.vector(qtotno), id=I(names(qtotno)))
        qtotwgt = tapply(X=cat$totwgt, INDEX=index, FUN=sum, na.rm=T)
        qtotwgt = data.frame(totwgt=as.vector(qtotwgt), id=I(names(qtotwgt)))
        qntaxa = tapply(X=rep(1, nrow(cat)), INDEX=index, FUN=sum, na.rm=T)
        qntaxa = data.frame(ntaxa=as.vector(qntaxa), id=I(names(qntaxa)))
        qs = merge(qtotno, qtotwgt, by=c("id"), sort=F, all=T)
        qs = merge(qs, qntaxa, by=c("id"), sort=F, all=T)
        set = merge(set, qs, by=c("id"), sort=F, all.x=T, all.y=F, suffixes=c("", paste(".",tx,sep="")) )
      }
      set$totno = NULL
      set$totwgt = NULL
      set$ntaxa = NULL
      set$yr = NULL
      save ( set, file=fn, compress=T)
      return( fn  )
    }


    # ----------------------


    if (DS %in% c("set.det", "set.det.redo") ) {
      fn = file.path( project.directory("groundfish"), "data", "set_det.rdata")
      if ( DS=="set.det" ) {
        load( fn )
        return ( set )
      }
      
      require (Hmisc)
      set = groundfish.db( "set.base" ) [, c("id", "yr")] # yr to maintain data structure
      newvars = c("rmean", "pmean", "mmean", "lmean", "rsd", "psd", "msd", "lsd") 
      dummy = as.data.frame( array(data=NA, dim=c(nrow(set), length(newvars) )))
      names (dummy) = newvars
      set = cbind(set, dummy)
      
      det = groundfish.db( "det" )       
     
      #det = det[ which(det$settype %in% c(1, 2, 4, 5, 8) ) , ]
    # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
    #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
    #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
      det$mass = log10( det$mass )
      det$len  = log10( det$len )

#       det0 = det[, c("id", "spec", "mass", "len", "age", "residual", "pvalue", "cf")]
       det0 = det[, c("id", "spec", "mass", "len", "age", "cfdet")]
      rm (det); gc()

      for (tx in taxa) {
        print(tx)
        if (tx %in% c("northernshrimp") ) next
        i = taxonomy.filter.taxa( det0$spec, taxafilter=tx, outtype="groundfishcodes"  )
        det = det0[i,]
        index = list(id=det$id)
        
        # using by or aggregate is too slow: raw computation is fastest using the fast formula: sd = sqrt( sum(x^2)-sum(x)^2/(n-1) ) ... as mass, len and resid are log10 transf. .. they are geometric means

        mass1 = tapply(X=det$mass*det$cfdet, INDEX=index, FUN=sum, na.rm=T)
        mass1 = data.frame(mass1=as.vector(mass1), id=I(names(mass1)))
        
        mass2 = tapply(X=det$mass*det$mass*det$cfdet, INDEX=index, FUN=sum, na.rm=T)
        mass2 = data.frame(mass2=as.vector(mass2), id=I(names(mass2)))

        len1 = tapply(X=det$len*det$cfdet, INDEX=index, FUN=sum, na.rm=T)
        len1 = data.frame(len1=as.vector(len1), id=I(names(len1)))
        
        len2 = tapply(X=det$len*det$len*det$cfdet, INDEX=index, FUN=sum, na.rm=T)
        len2 = data.frame(len2=as.vector(len2), id=I(names(len2)))

        ntot = tapply(X=det$cfdet, INDEX=index, FUN=sum, na.rm=T)
        ntot = data.frame(ntot=as.vector(ntot), id=I(names(ntot)))

        qs = NULL
        qs = merge(mass1, mass2, by=c("id"), sort=F, all=T)
        qs = merge(qs, len1, by=c("id"), sort=F, all=T)
        qs = merge(qs, len2, by=c("id"), sort=F, all=T)

        qs = merge(qs, ntot, by=c("id"), sort=F, all=T)

        qs$mmean = qs$mass1/qs$ntot
        qs$lmean = qs$len1/qs$ntot
        
        # these are not strictly standard deviations as the denominator is not n-1 
        # but the sums being fractional and large .. is a close approximation
        # the "try" is to keep the warnings quiet as NANs are produced.
        qs$msd = try( sqrt( qs$mass2 - (qs$mass1*qs$mass1/qs$ntot) ), silent=T  )
        qs$lsd = try( sqrt( qs$len2 - (qs$len1*qs$len1/qs$ntot)  ), silent=T  )
        
        qs = qs[, c("id","mmean", "lmean",  "msd", "lsd")]
        set = merge(set, qs, by=c("id"), sort=F, all.x=T, all.y=F, suffixes=c("", paste(".",tx,sep="")) )
      }
      for (i in newvars) set[,i]=NULL # these are temporary vars used to make merges retain correct suffixes

      set$yr = NULL  # dummy var

      save ( set, file=fn, compress=T)
      return( fn  )
    }
    
    # ----------------------
    


    if (DS %in% c("set.partial") ) {
      
      # this is everything in groundfish just prior to the merging in of habitat data
      # useful for indicators db as the habitat data are brough in separately (and the rest of 
      # set.complete has not been refactored to incorporate the habitat data

      set = groundfish.db( "set.base" )

      # 1 merge catch
      set = merge (set, groundfish.db( "catchbyspecies" ), by = "id", sort=F, all.x=T, all.y=F )

      # 2 merge condition and other determined characteristics
      set = merge (set, groundfish.db( "set.det" ), by = "id", sort=F, all.x=T, all.y=F )
  
      # strata information
      gst = groundfish.db( DS="gsstratum" )
      w = c( "strat", setdiff( names(gst), names(set)) )
      if ( length(w) > 1 ) set = merge (set, gst[,w], by="strat", all.x=T, all.y=F, sort=F)
      set$area = as.numeric(set$area)
      
      return( set)
    
    }

    # ----------------------
    


    if (DS %in% c("set.complete", "set.complete.redo") ) {
      fn = file.path( project.directory("groundfish"), "data", "set.rdata")
      if ( DS=="set.complete" ) {
        load( fn )
        return ( set )
      }
     
      set = groundfish.db( "set.partial" )
      set = lonlat2planar(set, proj.type=p$internal.projection ) # get planar projections of lon/lat in km
      set$z = set$sdepth 
      set$t = set$temp
      set = habitat.lookup( set, DS="baseline", p=p )
      set$z = log(set$z) 
      # return planar coords to correct resolution
      set = lonlat2planar( set, proj.type=p$internal.projection )
      save ( set, file=fn, compress=F )
      return( fn )
    }
 
}


