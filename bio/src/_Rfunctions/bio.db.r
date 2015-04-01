
  bio.db = function( DS, p=NULL ) {
  
    dir.create( file.path( project.datadirectory("bio"), "data" ), showWarnings=FALSE, recursive=TRUE )

    if (DS %in% c("set.init","set.init.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( project.datadirectory("bio"), "data", "set.init.rdata"  )
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
      
      # set$chron = as.chron( as.numeric(string2chron( paste( paste( set$yr, "Jan", "01", sep="-" ), "12:00:00") )) + set$julian ) # required for time-dependent lookups
      
      save( set, file=fn, compress=T )
      return (fn) 
    }



    # --------------------


    if (DS %in% c("cat.init","cat.init.redo") ) {
      # all species caught
      cat = NULL # trip/cat loc information
      fn = file.path( project.datadirectory("bio"), "data", "cat.init.rdata"  )
      if (DS=="cat.init") {
        if (file.exists( fn) ) load( fn)
        return ( cat )
      }


      ###  NOTE:: cf == correction factor is a reweighting required to make each totno and totmass comparable for each set and species subsampling

      cat.names =  c("data.source", "id", "spec", "spec_bio", "totno", "totmass", "cfcat") 
      if ( "groundfish" %in% p$data.sources ) {
       
        x = groundfish.db( "cat" )   ## not really set but "cat" 
        # totno and totmass are sa, vessel and sub-sampling corrected ::  cf = cfvessel / sakm2 
        # no./km2 ; and  kg/km2
        x$data.source = "groundfish"
        x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec )
        x$totmass = x$totwgt
        x$cfcat = x$cfset * x$cfvessel
				x = x[, cat.names]
        cat = rbind( cat, x )
        rm (x); gc()
      }

      if ( "snowcrab" %in% p$data.sources ) {
        x =  snowcrab.db( DS ="cat.georeferenced" ) # sa corrected ; kg/km2; no./km2 
        x$data.source = "snowcrab"
        x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec )
        x$id = paste( x$trip, x$set, sep="." )
        x$cfcat = 1/x$sa  # no other correction factors as there is no species-based subsampling
        x = x[, cat.names]
         
        iissp = taxonomy.recode( from="spec", to="parsimonious", tolookup=2526 ) # snow crab using groundfish codes

        oo = which( !is.finite(x$totno) & x$spec_bio==iissp  )  # snow crab are assumed to be real zeros
        if (length(oo) > 0 ) x$totno[oo] = 0

        oo = which( !is.finite(x$totmass) & x$spec_bio== iissp )  # snow crab are assumed to be real zeros
        if (length(oo) > 0 ) x$totmass[oo] = 0

				cat = rbind( cat, x  )
        rm (x); gc()
      }
      
      cat$id2 = paste( cat$id, cat$spec_bio, sep="." )

      save( cat, file=fn, compress=T )
      return (fn) 
    }



    # --------------------


    if (DS %in% c("det.init","det.init.redo") ) {
      # all species caught
      det = NULL # biologicals 
      fn = file.path( project.datadirectory("bio"), "data", "det.init.rdata"  )
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

      
      det.names =  c("data.source", "id", "spec", "spec_bio", "detid", "sex", "mass", "len", "mat", "cfdet" ) 
      if ( "groundfish" %in% p$data.sources ) {
        x = groundfish.db( "det" )  
        x$data.source = "groundfish"
        x$detid = x$fshno
             
        x$spec_bio = taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec ) 

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
        x$spec = 2526
        x$spec_bio =  taxonomy.recode( from="spec", to="parsimonious", tolookup=x$spec ) # snow crab using groundfish codes
        x$detid = x$crabno
        x$len = x$cw / 10  # convert mm to cm
        x$cfdet = 1/x$sa  ########## <<<<<< ------ NOTE THIS accounts only for SA as there is no subsampling (so far)
        x$sex = as.numeric( as.character( x$sex) )
        x$mat = as.numeric( as.character( x$mat) )
        x$mass = x$mass /1000  # g to kg
        
        det = rbind( det, x[, det.names] )
        rm (x); gc()
      }
       
      det$id2 = paste( det$id, det$spec_bio, sep=".") 

      save( det, file=fn, compress=T )
      return (fn) 
    }


    # -------------


    if (DS %in% c("set.intermediate","set.intermediate.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( project.datadirectory("bio"), "data", "set.intermediate.rdata"  )
      if (DS=="set.intermediate") {
        if (file.exists( fn) ) load( fn)
        return ( set )
      }
      set = bio.db( DS="set.init", p=p )
      set = set[ which(is.finite(set$lon + set$lat + set$yr ) ) , ]  #  fields are required
      oo =  which( !duplicated(set$id) )
      if (length(oo) > 0 ) set = set[ oo, ] 
      set = lonlat2planar( set, proj.type=p$internal.projection )  # plon+plat required for lookups
      set = habitat.lookup( set, DS="depth", p=p )
      set = habitat.lookup( set, DS="temperature", p=p )
      set$oxysat = compute.oxygen.saturation( t.C=set$t, sal.ppt=set$sal, oxy.ml.l=set$oxyml)
      save( set, file=fn, compress=T )
      return (fn) 
    }


    # --------------------


    if (DS %in% c("det","det.redo") ) {
     
      # error checking, imputation, etc
      
      det = NULL 
      fn = file.path( project.datadirectory("bio"), "data", "det.rdata"  )
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
      det = det[ which( is.finite( det$spec_bio)), ]   
      det$sex[ which( !is.finite(det$sex)) ] = 2 # set all uncertain sexes to one code sex code 
      det$mat[ which( !is.finite(det$mat)) ] = 2 # set all uncertain sexes to one code sex code 
    
      # do this here rather than in the condition subsystem as the models are useful in predicting missing data ...
      # length-weight modelling and estimate parameters as well as residuals
      det$spec0 = det$spec
      det$spec  = det$spec_bio 
      det = length.weight.regression( x=det )
      det$spec = det$spec0
      det$spec0 = NULL

      # fix mass, length estimates where possible using model parameters
      # try finest match first: by spec:mat, spec:sex, spec
      
      lwp = length.weight.regression( DS="parameters" )
      # note: lwp$spec is derived from spec_bio, as above

      ims = which( !is.finite( det$mass) )
      sps = sort( unique( det$spec_bio[ ims ] ) )
      mats = sort( unique( det$mat))
      sexes = sort( unique( det$sex))

      for (sp in sps) {
        isp = which( det$spec_bio == sp )
        
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
        isp = which( det$spec_bio == sp )
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

    
      ## det$cfdet needs to be updated as it was formed without the above re-estimation of missing weights
      
      cat = bio.db( DS="cat.init", p=p )
 
      massTotCat = applySum( det[ ,c("id2", "mass")], newnames=c("id2","massTotdet" ) )  
      
      noTotCat = applySum( det$id2, newnames=c("id2","noTotdet" ) )  

      cat = merge( cat, massTotCat, by="id2", all.x=T, all.y=F, sort=F )  # set-->kg/km^2, det-->km
      cat$massTotdet[ which( !is.finite (cat$massTotdet ))] = 0  ### whenn missing it means no determinations were made
      cat = merge( cat, noTotCat, by="id2", all.x=T, all.y=F, sort=F )    # set-->no/km^2, det-->no
      cat$noTotdet[ which( !is.finite (cat$noTotdet ))] = 0  ### whenn missing it means no determinations were made
      

      cf = data.frame( cbind( cfdetm =  cat$totmass/ cat$massTotdet, cfdetn =  cat$totno/ cat$noTotdet ) )
      cf$rsids = NA
      cf$cfdetm [ which(cf$cfdetm==0) ] = NA
      cf$cfdetn [ which(cf$cfdetn==0) ] = NA
      cfi = which( is.finite( cf$cfdetm + cf$cfdetn ))     

      cfmod = lm( cfdetm ~ cfdetn, data=cf[cfi,] )
      cf$rsids[cfi] = rstandard( cfmod) 
    
      # remove extremes and redo
      cfi = which( is.finite( cf$cfdetm + cf$cfdetn ) & abs(cf$rsids) < 4 )
      cfmod = lm( cfdetm ~ cfdetn, data=cf[cfi,] )
      cf$rsids = NA
      cf$rsids[cfi] = rstandard( cfmod) 
      
      cfi = which( is.finite( cf$cfdetm + cf$cfdetn ) & abs(cf$rsids) < 3 )  #make even more selective
      
      cat$cfdet = NA
      cat$cfdet[cfi] = cat$totmass[cfi] / cat$massTotdet[cfi]   # totwgt already corrected for vessel and tow .. cfdet is the multiplier required to make each det measurement scale properly
   
      oo = which ( !is.finite( cf$cfdetm ) & is.finite(cf$cfdetn) )
      if (length(oo)>0) cat$cfdet[oo] = cf$cfdetn[oo]  
      
      oo = which ( is.finite( cf$cfdetm ) & !is.finite(cf$cfdetn) )
      if (length(oo)>0) cat$cfdet[oo] = cf$cfdetm[oo]  
      
      oo = which ( !is.finite( cat$cfdet ) ) 
      
      for ( ds in unique( cat$data.source[oo] ) ) {
        for ( sp in unique( cat$spec_bio[oo]  )) {
          
          mm = which( cat$data.source==ds & cat$spec_bio==sp & (!is.finite( cat$cfdet ) |  cat$cfdet==0 ) ) 
          nn = which( cat$data.source==ds & cat$spec_bio==sp )
          if ( length(mm)>0  & length(nn)>0  ) {
            cat$cfdet[ mm ] = median( cat$cfdet[nn] , na.rm=TRUE )  
          }
      }}

      cat = cat[, c("id2", "cfdet")]  # a lot of missing values but this is normal as they will not be represented in "det"
      
      det$cfdet = NULL
      det = merge( det, cat, by="id2", all.x=T, all.y=F, sort=F) 
      
      ## remaining NA's with cfdet are mostly due to bad hauls, broken nets etc. 

      save (det, file=fn, compress=TRUE )
      return (fn)
    } 


    # --------------------


    if (DS %in% c("cat", "cat.redo") ) {
      # all species caught
      cat = NULL # biologicals 
      fn = file.path( project.datadirectory("bio"), "data", "cat.rdata"  )
      if (DS=="cat") {
        if (file.exists( fn) ) load( fn)
        return ( cat )
      }
   
      set = bio.db( DS="set.init" ) # kg/km^2, no/km^2
      
      det = bio.db( DS="det" ) # size information, no, cm, kg
      det = det[ which( det$id %in% unique( set$id) ), ]

      cat = bio.db( DS="cat.init", p=p )
      cat = cat[ which( cat$id %in% unique( set$id) ), ]
      cat$cfcat = NULL  # no longer needed as each data point is ~ equivalent
  
      oo = which( duplicated( cat$id2) )
      if (length( oo) > 0 ) cat = cat[ -oo, ]
     

      cm = data.frame( id2=as.character( sort( unique( cat$id2 ) )), stringsAsFactors=FALSE )
      cm = merge( cm, applySum( det[ , c("id2", "mr", "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )
     
      # averages of these variables
      newvars = c( "residual", "mass", "len", "Ea", "A", "Pr.Reaction", "smr"  ) 
      for ( nv in newvars ) {
        cm = merge( cm, 
          applyMean( det[ , c("id2", nv, "cfdet")] ), by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )
      }
      
      cat = merge( cat, cm, by="id2", all.x=TRUE, all.y=FALSE, sort=FALSE )

      # where det measurements not available, estimate mean mass from total weights and numbers  
      oo = which( !is.finite( cat$mass )) 
      if (length(oo) > 0 ) {
        cat$mass[oo] = cat$totmass[oo] / cat$totno[oo]
      }


    	surveys = sort( unique( cat$data.source ) ) 
      species = sort( unique( cat$spec_bio ) )
			
			# in the following:	quantiles are computed, 
      cat$qn = NA  # default when no data
      oo = which( cat$totno == 0 )  # retain as zero values 
      if (length(oo)>0 ) cat$qn[oo] = 0

      for ( s in surveys ) {
        si = which( cat$data.source==s & cat$totno > 0 )
        for (sp in species ){
          spi = which( cat$spec_bio == sp )
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
          spi = which( cat$spec_bio == sp )
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
      
      save (cat, file=fn, compress=TRUE )
      return (fn)
    
    } 



    # -------------



    if (DS %in% c("set","set.redo") ) {
      # survet sets
      set = NULL # trip/set loc information
      fn = file.path( project.datadirectory("bio"), "data", "set.rdata"  )
      if (DS=="set") {
        if (file.exists( fn) ) load( fn)
        return ( set )
      }
 
      set = bio.db( DS="set.intermediate", p=p )
       
      det = bio.db( DS="det" ) # size information, no, cm, kg
      det = det[ which( det$id %in% unique( set$id) ), ]
    
      cat = bio.db( DS="cat", p=p )
      cat = cat[ which( cat$id %in% unique( set$id) ), ]

      # NOTE: cat$totno and cat$totmass have already been cf corrected ---> already in per km2
      sm = data.frame( id=as.character( sort( unique( set$id ) )), stringsAsFactors=FALSE )
  
      # summaries from cat
      sm = merge( sm, applySum( cat[ , c("id", "totno") ] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      sm = merge( sm, applySum( cat[ , c("id", "totmass") ] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )

   

      # summaries from det   
      # --- NOTE det was not always determined and so totals from det mass != totals from cat nor set for all years 

      sm = merge( sm, applySum( det[ , c("id", "mr", "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
     
      # averages of these variables from det
      newvars = c( "residual", "mass", "len", "Ea", "A", "Pr.Reaction", "smr"  ) 
      for ( nv in newvars ) {
        sm = merge( sm, 
          applyMean( det[ , c("id", nv, "cfdet")] ), by="id", all.x=TRUE, all.y=FALSE, sort=FALSE )
      }


      set = merge( set, sm, by ="id", all.x=TRUE, all.y=FALSE, sort=FALSE )

    	surveys = sort( unique( set$data.source ) ) 
			
			# in the following:	quantiles are computed, 
      set$qn = NA  # default when no data
      oo = which( set$totno == 0 )  # retain as zero values 
      if (length(oo)>0 ) set$qn[oo] = 0

      for ( s in surveys ) {
        ii = which( set$data.source==s & set$totno > 0 )
        if (length( ii) > 0 ) {
  				set$qn[ii] = quantile.estimate( set$totno[ii]  )  # convert to quantiles, by survey
				} 
      }
	
			set$qm = NA   # default when no data
      oo = which( set$totmass == 0 )  # retain as zero values 
      if (length(oo)>0 ) set$qm[oo] = 0
      
      for ( s in surveys ) {
        ii = which( set$data.source==s & set$totmass > 0 )
          if (length( ii) > 0 ) {
						set$qm[ii] = quantile.estimate( set$totmass[ii]  )  # convert to quantiles, by survey	
					}
      }
		
     # convert from quantile to z-score 
        
      set$zm = quantile.to.normal( set$qm )
      set$zn = quantile.to.normal( set$qn )


			over.write.missing.data = TRUE 
			if (over.write.missing.data) {
				
				# over-write na's for n or mass from each other, where possible:
				kxm = which( !is.finite( set$qm) )
				kxn = which( !is.finite( set$qn) )
				
				kmn = setdiff( kxm, kxn )
				knm = setdiff( kxn, kxm )
				
				if ( length( knm) > 0 ) set$qn[knm] =  set$qm[knm]
				if ( length( kmn) > 0 ) set$qm[kmn] =  set$qn[kmn]

				# remaining missing values take the median value == 0.5
				kxm = which( !is.finite( set$qm ) )
				if ( length( kxm) > 0 ) set$qm[kxm] = 0.5
				
				kxn = which( !is.finite( set$qn ) )
				if ( length( kxn) > 0 ) set$qn[kxn] = 0.5

			}
      

      save( set, file=fn, compress=T )
      return (fn) 
    }


  }



