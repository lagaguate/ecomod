
  bio.db = function( DS, p=NULL ) {
  
    dir.create( file.path( project.directory("bio"), "data" ), showWarnings=FALSE, recursive=TRUE )

    if (DS %in% c("set.init","set.init.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( project.directory("bio"), "data", "set.init.rdata"  )
      if (DS=="set.init") {
        if (file.exists( fn) ) load( fn)
        return ( set )
      }
      
			set.names =  c("data.source", "id", "chron", "yr", "julian", "lon", "lat", 
                     "z", "t", "sal", "oxyml", "settype", "sa", "cfset") 
      if ( "groundfish" %in% p$data.sources ) {
        # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
        #  4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
        #  6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
        y = groundfish.db( "set.base" )
        y$data.source = "groundfish"
        y$sa = y$sakm2
        y$cfset = 1 / y$sa

        set = rbind( set, y[ ,c("data.source", "id", "chron", "yr", "julian",  "lon", "lat", 
                                "sdepth", "temp", "sal", "oxyml", "settype", "sa", "cfset") ] )
        names(set) = set.names
        set = set[ set$settype %in% c(1,2,5) ,] # remove bad sets
        rm (y); gc()
      }
      if ( "snowcrab" %in% p$data.sources ) {
        y =  snowcrab.db( DS ="set.clean" )  
        y$data.source = "snowcrab"
        y$id = paste( y$trip, y$set, sep="." )
        y$cfset = 1 # assume 100% q
        y$settype = y$towquality # 1=good
        iii = which( y$settype != 1 ) 
        if (length(iii)>0 ) y$settype[iii ] = NA  # should not happen as only good tows have already been selected .. here in case something changes in snow crab data stream
        y$sal = NA
        y$oxyml = NA
        set = rbind( set, y[ , set.names ] )
        rm (y); gc()
      }
      
      save( set, file=fn, compress=T )
      return (fn) 
    }



    # --------------------


    if (DS %in% c("cat.init","cat.init.redo") ) {
      # all species caught
      cat = NULL # trip/cat loc information
      fn = file.path( project.directory("bio"), "data", "cat.init.rdata"  )
      if (DS=="cat.init") {
        if (file.exists( fn) ) load( fn)
        return ( cat )
      }


###  NOTE:: cf == correction factor is a reweighting required to make each totno and totmass comparable for each set and species subsampling

      cat.names =  c("data.source", "id", "spec", "totno", "totmass", "cfcat") 
      if ( "groundfish" %in% p$data.sources ) {
       
        x = groundfish.db( "cat" )   ## not really set but "cat" 
        # totno and totmass are sa, vessel and sub-sampling corrected ::  cf = cfvessel / sakm2 
        # no./km2 ; and  kg/km2
        x$data.source = "groundfish"
        x$totmass = x$totwgt
        x$cfcat = x$cf
				x = x[, cat.names]
        cat = rbind( cat, x )
        rm (x); gc()
      }

      if ( "snowcrab" %in% p$data.sources ) {
        x =  snowcrab.db( DS ="cat.georeferenced" ) # sa corrected ; kg/km2; no./km2 
        x$data.source = "snowcrab"
        x$id = paste( x$trip, x$set, sep="." )
        x$cfcat = 1/x$sa  # no other correction factors as there is no species-based subsampling
        x = x[, cat.names]

        oo = which( !is.finite(x$totno) & x$spec== taxa.specid.correct(2526) )  # snow crab are assumed to be real zeros
        if (length(oo) > 0 ) x$totno[oo] = 0

        oo = which( !is.finite(x$totmass) & x$spec== taxa.specid.correct(2526) )  # snow crab are assumed to be real zeros
        if (length(oo) > 0 ) x$totmass[oo] = 0

				cat = rbind( cat, x  )
        rm (x); gc()
      }
      
      cat$id2 = paste( cat$id, cat$spec, sep="." )

      save( cat, file=fn, compress=T )
      return (fn) 
    }



    # --------------------


    if (DS %in% c("det.init","det.init.redo") ) {
      # all species caught
      det = NULL # biologicals 
      fn = file.path( project.directory("bio"), "data", "det.init.rdata"  )
      if (DS=="det.init") {
        if (file.exists( fn) ) load( fn)
        return ( det )
      }
      
                   
        # sex codes
        #  male = 0 
        #  female = 1
        #  sex.unknown = 2

        # maturity codes
        #  immature = 0
        #  mature = 1 
        #  mat.unknown = 2

      
      det.names =  c("data.source", "id", "spec", "detid", "sex", "mass", "len", "mat", "cfdet" ) 
      if ( "groundfish" %in% p$data.sources ) {
        x = groundfish.db( "det" )  
        x$data.source = "groundfish"
        x$detid = x$fshno
        
        # mass in kg, len in cm

        # convert sex codes to snow crab standard
        # --------- codes ----------------
        # sex: 0=undetermined, 1=male, 2=female,  3=hermaphrodite, 9= not examined
        # mat: 0=observed but undetermined, 1=imm, 2=ripening(1), 3=ripening(2), 4=ripe(mature), 
        #      5=spawning(running), 6=spent, 7=recovering, 8=resting
        # settype: 1=stratified random, 2=regular survey, 3=unrepresentative(net damage), 
        #      4=representative sp recorded(but only part of total catch), 5=comparative fishing experiment, 
        #      6=tagging, 7=mesh/gear studies, 8=explorartory fishing, 9=hydrography
        # --------- codes ----------------

        sx = x$sex
        x$sex = NA
        oo = which( sx %in% c(0, 3, 9) ); if (length(oo)>0) x$sex[oo] = 2 # unknown
        oo = which( sx %in% c(1) ); if (length(oo)>0) x$sex[oo] = 0 # male
        oo = which( sx %in% c(2) ); if (length(oo)>0) x$sex[oo] = 1 # female
        
        # convert maturity to snow crab standard
        mt = x$mat
        x$mat = NA
        oo = which( mt %in% c(0) ); if (length(oo)>0) x$mat[oo] = 2 # unknown
        oo = which( mt %in% c(1) ); if (length(oo)>0) x$mat[oo] = 0  # immature
        oo = which( mt %in% c(2,3,4,5,6,7,8) ); if (length(oo)>0) x$mat[oo] = 1 # mature  -- investment into gonads has begun
         
        det = rbind( det, x[, det.names] )
        rm (x); gc()

      }

      if ( "snowcrab" %in% p$data.sources ) {
          
        x =  snowcrab.db( DS ="det.georeferenced" )  
        x$data.source = "snowcrab"
        x$id = paste( x$trip, x$set, sep="." )
        x$spec = taxa.specid.correct(2526)  # in case there has been an interal alteration in species code for snowcrab
        x$detid = x$crabno
        x$len = x$cw / 10  # convert mm to cm
        x$cfdet = 1/x$sa  ########## <<<<<< ------ NOTE THIS accounts only for SA as there is no subsampling (so far)
        x$sex = as.numeric( as.character( x$sex) )
        x$mat = as.numeric( as.character( x$mat) )
        x$mass = x$mass /1000  # g to kg
        
        det = rbind( det, x[, det.names] )
        rm (x); gc()
      }
       
      det$id2 = paste( det$id, det$spec, sep=".") 

      save( det, file=fn, compress=T )
      return (fn) 
    }


    # -------------


    if (DS %in% c("set.intermediate","set.intermediate.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( project.directory("bio"), "data", "set.intermediate.rdata"  )
      if (DS=="set.intermediate") {
        if (file.exists( fn) ) load( fn)
        return ( set )
      }
 
      set = bio.db( DS="set.init", p=p )
      
      set = set[ which(is.finite(set$lon + set$lat + set$yr ) ) , ]  #  fields are required

      oo =  which( !duplicated(set$id) )
      if (length(oo) > 0 ) set = set[ oo, ] 
   
      set = lonlat2planar( set, proj.type=p$internal.projection )  # plon+plat required for lookups
     
      print( "Interpolating depth and temperature")

      p$interpolation.distances = c( 2, 4, 8, 16, 32, 64 ) # pseudo-log-scale

      set$z = habitat.lookup.simple( set,  p=p, vnames="z", lookuptype="depth", sp.br=p$interpolation.distances   ) 
      set$t = habitat.lookup.simple( set,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 
      
      set$oxysat = compute.oxygen.saturation( t.C=set$t, sal.ppt=set$sal, oxy.ml.l=set$oxyml)

      save( set, file=fn, compress=T )
      return (fn) 
    }


    # --------------------


    if (DS %in% c("det","det.redo") ) {
     
      # error checking, imputation, etc
      
      det = NULL 
      fn = file.path( project.directory("bio"), "data", "det.rdata"  )
      if (DS=="det") {
        if (file.exists( fn) ) load( fn)
        return ( det )
      }

        # sex codes
        #  male = 0 
        #  female = 1
        #  sex.unknown = 2

        # maturity codes
        #  immature = 0
        #  mature = 1 
        #  mat.unknown = 2
        
      det = bio.db( DS="det.init", p=p )
      det = det[ which( is.finite( det$spec)), ]   
      det$sex[ which( !is.finite(det$sex)) ] = 2 # set all uncertain sexes to one code sex code 
      det$mat[ which( !is.finite(det$mat)) ] = 2 # set all uncertain sexes to one code sex code 
     
      # length-weight modelling and estimate parameters as well as residuals
      det = length.weight.regression( x=det )

      # fix mass, length estimates where possible using model parameters
      # try finest match first: by spec:mat, spec:sex, spec
      lwp = length.weight.regression( DS="parameters" )
      
      ims = which( !is.finite( det$mass) )
      sps = sort( unique( det$spec[ ims ] ) )
      mats = sort( unique( det$mat))
      sexes = sort( unique( det$sex))

      for (sp in sps) {
        isp = which( det$spec == sp )
        
        # first try exact matches based upon {spec, mat, sex}  
        for ( mat in mats ) {
        for ( sex in sexes ) {
          u = which( det$mat==mat & det$sex==sex & !is.finite(det$mass ) ) 
          w = intersect( isp, u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & lwp$mat==mat & lwp$sex==sex & lwp$rsq>0.75)
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
        }}
        
        # next try exact matches based upon {spec, mat}  
        for ( mat in mats ) {
          u = which( det$mat==mat & !is.finite(det$mass )  )
          w = intersect( isp, u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & lwp$mat==mat & is.na(lwp$sex  & lwp$rsq>0.75 ) )
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
        }

       # next try exact matches based upon {spec, sex}  
        for ( sex in sexes ) {
          u = which( det$sex==sex & !is.finite(det$mass )  ) 
          w = intersect( isp, u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & lwp$sex==sex & is.na(lwp$mat  & lwp$rsq>0.75 ) )
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
        }

       # next try exact matches based upon {spec} only  
          u = which( is.na(det$mass ))
          w = intersect( isp , u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & is.na(lwp$sex) & is.na(lwp$mat  & lwp$rsq>0.75 ) )
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
      }

      # last go -- try exact matches based upon {spec} only but less contrained by rquared
      for (sp in sps) {
        isp = which( det$spec == sp )
          u = which( is.na(det$mass ))
          w = intersect( isp , u )
          if (length(w) > 0) {
            v = which( lwp$spec==sp & is.na(lwp$sex) & is.na(lwp$mat ) )
            if (length(v)==1) det$mass[w] = 10^( lwp$b0[v] + lwp$b1[v] * log10(det$len[w]) )
          }
      }

      # estimate metabolic rates estimates (requires temperature estimate ) 
      set = bio.db( DS="set.intermediate" ) # kg/km^2, no/km^2
      set = set[ , c("id", "t")]  # temperature is required to estimate MR .. 

      det = merge( det, set, by="id", all.x=T, all.y=F, sort=F )
      detmr = metabolic.rates ( det$mass * 1000, det$t )
      det = cbind( det, detmr )


      save (det, file=fn, compress=TRUE )
      return (fn)
    } 


    # --------------------


    if (DS %in% c("cat", "cat.redo") ) {
      # all species caught
      cat = NULL # biologicals 
      fn = file.path( project.directory("bio"), "data", "cat.rdata"  )
      if (DS=="cat") {
        if (file.exists( fn) ) load( fn)
        return ( cat )
      }
   
      set = bio.db( DS="set.init" ) # kg/km^2, no/km^2
      
      cat = bio.db( DS="cat.init", p=p )
      cat = cat[ which( cat$id %in% unique( set$id) ), ]

      det = bio.db( DS="det" ) # size information, no, cm, kg
      det = det[ which( det$id %in% unique( set$id) ), ]

  
      # NOTE: cat$totno and cat$totmass have already been cf corrected ---> already in per km2
      sm = data.frame( id2=as.character( sort( unique( cat$id2 ) )), stringsAsFactors=FALSE )
 
      # sm = merge( sm, applySum( det[ , c("id2", "mass", "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )
      # test to make sure weights are proper:
      # plot( sm$totwgt, sm$mass )
      
      sm = merge( sm, applySum( det[ , c("id2", "mr", "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )
       
      sm = merge( sm, applyMean( det[ , c("id2", "smr", "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )
      sm = merge( sm, applyMean( det[ , c("id2", "mass", "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )
      sm = merge( sm, applyMean( det[ , c("id2", "len", "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )

  

    	surveys = sort( unique( cat$data.source ) ) 
      species = sort( unique( cat$spec ) )

			
			# in the following:	quantiles are computed, 
      cat$qn = NA  # default when no data
      oo = which( cat$totno == 0 )  # retain as zero values 
      if (length(oo)>0 ) cat$qn[oo] = 0

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
      oo = which( cat$totmass == 0 )  # retain as zero values 
      if (length(oo)>0 ) cat$qm[oo] = 0
      
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
        
      cat$zm = quantile.to.normal( cat$qm )
      cat$zn = quantile.to.normal( cat$qn )


			over.write.missing.data = TRUE 
			if (over.write.missing.data) {
				
				# over-write na's for n or mass from each other, where possible:
				kxm = which( !is.finite( cat$qm) )
				kxn = which( !is.finite( cat$qn) )
				
				kmn = setdiff( kxm, kxn )
				knm = setdiff( kxn, kxm )
				
				if ( length( knm) > 0 ) cat$qn[knm] =  cat$qm[knm]
				if ( length( kmn) > 0 ) cat$qm[kmn] =  cat$qn[kmn]

				# remaining missing values take the median value for each species == 0.5
				kxm = which( !is.finite( cat$qm ) )
				if ( length( kxm) > 0 ) cat$qm[kxm] = 0.5
				
				kxn = which( !is.finite( cat$qn ) )
				if ( length( kxn) > 0 ) cat$qn[kxn] = 0.5

			}
      
      # qn, qm and zn, zm are quantiles and zscores for within each survey and species 
      # create another one for overall relative abundance of each species within each survey (qs, zs) 
      # zn * zs ==> zn(global across all surveys) 

      xtn = as.data.frame( xtabs( totno ~ data.source + spec, cat ), stringsAsFactors =FALSE  )
      xtn$qns = NA

        for (sp in surveys ){
          ii = which( xtn$data.source == sp )
					xtn$qns[ii] = quantile.estimate( xtn$Freq[ii]  )  # convert to quantiles, by species and survey
        }

      xtm = as.data.frame( xtabs( totmass ~ data.source + spec, cat ), stringsAsFactors =FALSE  )
      xtm$qms = NA
        for (sp in surveys ){
          ii = which( xtm$data.source == sp )
					xtm$qms[ii] = quantile.estimate( xtm$Freq[ii]  )  # convert to quantiles, by species and survey
        }


      xtm$Freq = NULL
      xtn$Freq = NULL
      xt = merge (xtn, xtm, by=c("data.source", "spec") )
      cat = merge( cat, xt, by=c("data.source", "spec"), all.x=TRUE, all.y=FALSE )


      cat$zms = quantile.to.normal( cat$qms )
      cat$zns = quantile.to.normal( cat$qns )

      save (cat, file=fn, compress=TRUE )
      return (fn)
    
    } 



    # -------------



    if (DS %in% c("set","set.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( project.directory("bio"), "data", "set.rdata"  )
      if (DS=="set") {
        if (file.exists( fn) ) load( fn)
        return ( set )
      }
 
      set = bio.db( DS="set.intermediate", p=p )
       
      det = bio.db( DS="det" ) # size information, no, cm, kg
      det = det[ which( det$id %in% unique( set$id) ), ]
 
      # NOTE: cat$totno and cat$totmass have already been cf corrected ---> already in per km2
      sm = data.frame( id=as.character( sort( unique( cat$id ) )), stringsAsFactors=FALSE )
   
      sm = merge( sm, applySum( cat[ , c("id", "totno") ] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      sm = merge( sm, applySum( cat[ , c("id", "totwgt") ] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )

      sm = merge( sm, applySum( det[ , c("id", "mr", "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      
      sm = merge( sm, applyMean( det[ , c("id", "smr", "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      sm = merge( sm, applyMean( det[ , c("id", "mass", "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      sm = merge( sm, applyMean( det[ , c("id", "len", "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
  
      save( set, file=fn, compress=T )
      return (fn) 
    }


  }



