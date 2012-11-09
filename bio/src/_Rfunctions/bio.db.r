
  bio.db = function( DS, p=NULL ) {
  
    dir.create( file.path( project.directory("bio"), "data" ), showWarnings=FALSE, recursive=TRUE )

    if (DS %in% c("set","set.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( project.directory("bio"), "data", "set.rdata"  )
      if (DS=="set") {
        if (file.exists( fn) ) load( fn)
				set = set[ which( set$data.source %in% p$data.sources ) ,]
        return ( set )
      }
      
			set.names =  c("data.source", "id", "chron", "yr", "julian", "lon", "lat", "z", "t", "sal", "oxyml", "settype", "sa", "cf") 
      if ( "groundfish" %in% p$data.sources ) {
        # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
        #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
        #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
        y = groundfish.db( "sm.base" )
        y$data.source = "groundfish"
        set = rbind( set, y[ ,c("data.source", "id", "chron", "yr", "julian",  "lon", "lat", "sdepth", "temp", "sal", "oxyml", "settype", "sakm2", "cf") ] )
        names(set) = set.names
        set = set[ set$settype %in% c(1,2,5) ,] # remove bad sets
        rm (y); gc()
      }
      if ( "snowcrab" %in% p$data.sources ) {
        y =  snowcrab.db( DS ="set.clean" )  
        y$data.source = "snowcrab"
        y$id = paste( y$trip, y$set, sep="." )
        y$cf = 1 # assume 100% q
        y$settype = y$towquality # 1=good
        iii = which( y$settype != 1 ) 
        if (length(iii)>0 ) y$settype[iii ] = NA  # should not happen as only good tows have already been selected .. here in case something changes in snow crab data stream
        y$sal = NA
        y$oxyml = NA
        set = rbind( set, y[ , set.names ] )
        rm (y); gc()
      }
      set = set[ which(is.finite(set$lon + set$lat + set$yr ) ) , ]  #  fields are required
      save( set, file=fn, compress=T )
      return (fn) 
    }


    if (DS %in% c("cat","cat.redo") ) {
      # all species caught
      cat = NULL # trip/cat loc information
      fn = file.path( project.directory("bio"), "data", "cat.rdata"  )
      if (DS=="cat") {
        if (file.exists( fn) ) load( fn)
        return ( cat )
      }
      cat.names =  c("data.source", "id", "spec", "totno", "totmass") 
      if ( "groundfish" %in% p$data.sources ) {
        x = groundfish.db( "set" )  
        x$data.source = "groundfish"
        x$totmass = x$totwgt
				x = x[, cat.names]
        cat = rbind( cat, x )
        rm (x); gc()
      }
      if ( "snowcrab" %in% p$data.sources ) {
        x =  snowcrab.db( DS ="cat.initial" )  
        x$data.source = "snowcrab"
        x$id = paste( x$trip, x$set, sep="." )
        x = x[, cat.names]
				cat = rbind( cat, x  )
        rm (x); gc()
      }
      # cat = cat[ which(is.finite(cat$totno)) , ]
      save( cat, file=fn, compress=T )
      return (fn) 
    }


    if (DS %in% c("det","det.redo") ) {
      # all species caught
      det = NULL # biologicals 
      fn = file.path( project.directory("bio"), "data", "det.rdata"  )
      if (DS=="det") {
        if (file.exists( fn) ) load( fn)
        return ( det )
      }
      det.names =  c("data.source", "id", "spec", "detid", "sex", "mass", "len", "mat" ) 
      if ( "groundfish" %in% p$data.sources ) {
        x = groundfish.db( "det" )  
        x$data.source = "groundfish"
        x$detid = x$fshno
        det = rbind( det, x[, det.names] )
        rm (x); gc()
      }
      if ( "snowcrab" %in% p$data.sources ) {
        x =  snowcrab.db( DS ="det.georeferenced" )  
        x$data.source = "snowcrab"
        x$id = paste( x$trip, x$set, sep="." )
        x$spec = 2526
        x$detid = x$crabno
        x$len = x$cw
        det = rbind( det, x[, det.names] )
        rm (x); gc()
      }
      # det = na.omit( det )
      save( det, file=fn, compress=T )
      return (fn) 
    }


		# --------------------


    if (DS %in% c("cat.fixed","cat.fixed.redo") ) {

			# estimate quantiles for postive and nonero-values, etc..

      cat = NULL # trip/cat loc information
      fn = file.path( project.directory("bio"), "data", "cat.fixed.rdata"  )
      if (DS=="cat.fixed") {
        if (file.exists( fn) ) load( fn)
        return ( cat )
      }
 
			cat = bio.db( DS="cat", p=p)
  
			# these are determined below ...
			spec.todrop =  c(90, 1091, 1092, 1093, 1094, 1095, 1100, 1200, 1224, 1300, 
                       1600, 1701, 4223, 6120,
											 9000, 9001, 9200,  9310, 9400, 9600, 9991, 9992, 9993, 
                       9994, 9995, 9996, 9997, 9998, 9999)
    
			to.drop = which (cat$spec %in% spec.todrop)
			cat = cat[ - to.drop, ]
    
			surveys = sort( unique( cat$data.source ) ) 
      species = sort( unique( cat$spec ) )

			debug = F
			if (debug) {
				sp.names =  lookup.spec2taxa(species) 
				i.strange = which( is.na( sp.names$tx ))
				sp.strange = sp.names$spec[ i.strange]
				print( "Need to check these species code in taxa db:")
				print( sp.strange )
				
				# To drop: c( 90, 1091, 1092, 1093, 1094, 1095, 1100, 1200, 1224,
        # 1300, 1600, 1701, 4223, 6120,
				#							9000, 9001, 9200, 9310, 9400, 9600, 9991, 9992, 9993, 
        #							9994, 9995, 9996, 9997, 9998, 9999)
			
				# 90 Unident fish
				# "Unidentified Species"|1091
				# "Unidentified Species"|1092|
				# Unidentified Species"|1093|
				# Unidentified Species"|1094|
				# "Unidentified Species"|1095|
				# "Eggs Unid."|1100||
				# "Fish Eggs-Unidentified"|1200|
				# "Skate Unid. Eggs"|1224|
				# "Crustacean Eggs"|1300|
				# "Invertebrate Eggs"|1600|
				# "Marine Invertebrates"|1701|
				# "Unid Remains"|9000||"
				# "Unid Fish And Invertebrates"|9001|
				# "Stones And Rocks"|9200||
				# "Foreign Articles"|9400|
				# "Water"|9600|
				# "Unidentified Per Set"|9991 --- 9999|
			
				
				## to keep? -- yes for now 
				# potential Taxa db problems:
				# "Shanny"|645|
				# "White Barracudina"|727||

			}	
		
			# in the following:	quantiles are computed, 
      cat$qn = NA  # default when no data
      for ( s in surveys ) {
        si = which( cat$data.source==s & cat$totno > 0 )
        for (sp in species ){
          spi = which( cat$spec == sp )
          ii = intersect( si, spi )
          if (length( ii) > 0 ) {
						cat$qn[ii] = quantile.estimate( cat$totno[ii]  )  # convert to quantiles, by species and survey
					} 
      }}
	
			cat$qm = NA   # default when no data
      for ( s in surveys ) {
        si = which( cat$data.source==s & cat$totmass > 0 )
        for (sp in species ){
          spi = which( cat$spec == sp )
          ii = intersect( si, spi )
          if (length( ii) > 0 ) {
						cat$qm[ii] = quantile.estimate( cat$totmass[ii]  )  # convert to quantiles, by species and survey	
					}
      }}
		
     # convert from quantile to z-score 
      maxqm = max( cat$qm[ which( cat$qm < 1 ) ] , na.rm=T )   
      maxqn = max( cat$qn[ which( cat$qn < 1 ) ] , na.rm=T )   
      cat$qm[ which(cat$qm==1) ] = maxqm
      cat$qn[ which(cat$qn==1) ] = maxqn
      cat$zm = qnorm( cat$qm )
      cat$zn = qnorm( cat$qn )


			debug = F
			if (debug) {
				
				kmin = tapply( cat$qn, cat$spec, min, na.rm=T)
				kmax = tapply( cat$qn, cat$spec, max, na.rm=T)
			
				# these have minima that are == 1  ! why?
				i = which(kmin==1)
				uu = as.numeric(names(kmin[i]))
				lookup.spec2taxa(uu)

				oo = which( !is.finite( kmin) | !is.finite(kmax))
				strange.spec = as.numeric( names(kmin)[oo] )
				strange.spec.id = lookup.spec2taxa( strange.spec )
				nas = which( is.na( strange.spec.id ) )
				pp = strange.spec[ nas]
				print ("Strange data?")
				print(pp)
			}

			over.write.missing.data = FALSE
			if (over.write.missing.data) {
				
				# over-write na's for n or mass from each other, where possible:
				kxm = which( !is.finite( cat$qm) )
				kxn = which( !is.finite( cat$qn) )
				
				kmn = setdiff( kxm, kxn )
				knm = setdiff( kxn, kxm )
				
				cat$qn[knm] =  cat$qm[knm]
				cat$qm[kmn] =  cat$qn[kmn]

				# remaining missing values take the median value for each species == 0.5
				kxm = which( !is.finite( cat$qm ) )
				cat$qm[kxm] = 0.5
				
				kxn = which( !is.finite( cat$qn ) )
				cat$qn[kxn] = 0.5

			}

			save( cat, file=fn, compress=T )
      return (fn) 
    }


		# --------------------

  }



