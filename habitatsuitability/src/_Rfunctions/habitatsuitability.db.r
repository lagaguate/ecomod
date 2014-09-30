
  habitatsuitability.db = function( DS, p=NULL ) {

    SSSdir = file.path( project.directory("habitatsuitability"), "data", p$studyarea )
    dir.create( SSSdir, showWarnings=FALSE, recursive=TRUE )

		if (DS %in% c("initial.set.redo", "initial.set" ) ) {
	    
      # process an initial bio.db snapshot and add environmental data lookups
      fn.set = file.path( SSSdir, "initial.set.rdata" )
      bs = NULL
			if ( DS=="initial.set" ) {
				if (file.exists( fn.set) ) load( fn.set )
				return ( bs )
			}
		
			bs = bio.db( DS="set",p=p )
      bs$month = floor(bs$julian/365*12) + 1 
      bs = lonlat2planar( bs, proj.type=p$internal.projection )
      bs$plon = grid.internal( bs$plon, p$plons )
      bs$plat = grid.internal( bs$plat, p$plats )
      bs = bs[ which( is.finite( bs$plon + bs$plat) ) , ]

      
      # filter based upon data sources
			if (exists("data.sources", p)) {
				bs = bs[ which( bs$data.source %in% p$data.sources ) , ]
			}
			
      if (exists("season", p)) {
				bs = bs[ filter.season( bs$julian, period=p$season, index=T ) , ]
			}
	 
			# filter area
			if (exists("corners", p)) {
				igood = which( 
          bs$lon >= p$corners$lon[1] & bs$lon <= p$corners$lon[2] &  
          bs$lat >= p$corners$lat[1] & bs$lat <= p$corners$lat[2] )
				if (length(igood) > 0 )  bs = bs[igood, ]
      }

      if (exists("studyarea", p)) {
        bs = bs[ filter.region.polygon( bs, region=p$studyarea ) , ]
      }
 

      # depth
      bs = habitat.lookup( bs, p=p, DS="depth" ) 
       
      if (exists( "depthrange", p) ) {
        to.na = unique( c( which( bs$z < p$depthrange[1] ) , which( bs$z > p$depthrange[2] ) ) )
        if (length( to.na) > 0 ) bs$z[ to.na ] = NA
      }

      bs = bs[ which( is.finite( bs$z ) ), ]
      gc()
 

      # temperature
      bs = habitat.lookup( bs, p=p, DS="temperature" ) 
      if (exists( "temperaturerange", p) ) {
        to.na = unique( c( which( bs$t < p$temperaturerange[1] ) , which( bs$t > p$temperaturerange[2] ) ) )
        if (length( to.na) > 0 ) bs$t[ to.na ] = NA
      }
      bs = bs[ which( is.finite( bs$t ) ), ]
      gc()

   
      # merge in other habitat data
      bs = habitat.lookup(x=bs, DS="all.data", p=p )

      oo = which( is.finite( bs$totno + bs$totwgt + bs$substrate.mean ) )
      bs = bs[oo,]

			save( bs, file=fn.set, compress=T )
			return(fn.set)
    }
		
    
    if ( DS=="initial.cat" ) {
      bs = habitatsuitability.db( "initial.set", p=p )
      bc =NULL
      bc = bio.db( DS="cat",p=p )
      bc$data.source = NULL 
      bc = bc[ which( bc$id %in% unique( bs$id) ) ,]
      return ( bc )
		}

			
    if ( DS=="initial.det" ) {
      bs = habitatsuitability.db( "initial.set", p=p )
      bd =NULL
      bd = bio.db( DS="det",p=p )
      bd$data.source = NULL 
      bd = bd[ which( bd$id %in% unique( bs$id) ) ,]
      return ( bd )
    }


		if (DS %in% c("taxasubset.cat", "taxasubset.cat.redo" ) ) {
					
      fn.cat = file.path( SSSdir, paste("cat", p$speciesofinterest, "rdata", sep=".") )
			bs = NULL
			if ( DS=="taxasubset.cat" ) {
				if (file.exists( fn.cat ) ) load( fn.cat )
				return ( bs )
			}
 
      # filter for species of interest 
      bc = habitatsuitability.db("initial.cat", p=p) # matches the id's in bs
      bc = taxonomy.filter.taxa( bc$spec_bio, taxafilter=p$speciesofinterest )
      bc$hn = bc$totno
      bc$hm = bc$totmass
      bc$totmass = NULL
      bc$totno = NULL
      bc$cf = NULL

      bs = habitatsuitability.db("initial.set", p=p)
      bs$w = 1/bs$cf   ### weighting factor
      bs$cf = NULL
      bs = habitatsuitability.sanitycheck(bs, p ) # sanity checking ---
      
      required.fields = c("id", "yr", "julian", "sa", "lon", "lat") 
			st = na.omit( bs[,required.fields] ) # all are required fields
			bs = bs[ which( bs$id %in% st$id ) ,]
      rm( st); gc()

      ## IDentify real zeros  
			bs = merge( bs, bc, by="id", all.x=T, all.y=F, sort=F )
      vs = c("hn", "hm", "qn", "qm", "zn", "zm" )
      for (v in vs) {
        ii = which( !is.finite( bs[,v] ) )
        if (length( (ii)>0)) bs[ii,v] = 0
      }

      bs$present = 0  # default assumes absence
      npresent = which( bs$qn >= p$habitat.threshold.quantile )
      mpresent = which( bs$qm >= p$habitat.threshold.quantile )
      present = unique(npresent, mpresent )
      bs$present [ present] = 1

      save( bs, file=fn.cat, compress=T )
			return(fn)
		
		}
	

		if (DS %in% c( "taxasubset.det", "taxasubset.det.redo" ) ) {
					
      fn.det = file.path( SSSdir, paste("det", p$speciesofinterest, "rdata", sep=".") )
 			if ( DS=="taxasubset.det" ) {
				if (file.exists( fn.det ) ) load( fn.det )
				return ( bs )
			}

      # filter for species of interest 
      bd = habitatsuitability.db("initial.det", p=p) # matches the id's in bs
      bd = taxonomy.filter.taxa( bd$spec_bio, taxafilte=p$speciesofinterest )
      bd = filter.biologicals ( bd, p$subset ) 


        mass1 = tapply(X=det$mass*det$cf, INDEX=index, FUN=sum, na.rm=T)
        mass1 = data.frame(mass1=as.vector(mass1), id=I(names(mass1)))
        


      bc = crosstabulate.biologicals (bd)

      # the rest tries to mimic the flow above for the 'taxasubset.cat'

      bc$hn = bc$totno
      bc$hm = bc$totmass
      bc$totmass = NULL
      bc$totno = NULL

      bs = habitatsuitability.db("initial.set", p=p)
      bs = habitatsuitability.sanitycheck(bs, p ) # sanity checking ---
      
      required.fields = c("id", "yr", "julian", "sa", "lon", "lat") 
			st = na.omit( bs[,required.fields] ) # all are required fields
			bs = bs[ which( bs$id %in% st$id ) ,]
      rm( st); gc()
 

      ## IDentify real zeros  
			bs = merge( bs, bc, by="id", all.x=T, all.y=F, sort=F )
      vs = c("hn", "hm", "qn", "qm", "zn", "zm" )
      for (v in vs) {
        ii = which( !is.finite( bs[,v] ) )
        if (length( (ii)>0)) bs[ii,v] = 0
      }

      bs$present = 0  # default assumes absence
      npresent = which( bs$qn >= p$habitat.threshold.quantile )
      mpresent = which( bs$qm >= p$habitat.threshold.quantile )
      present = unique(npresent, mpresent )
      bs$present [ present] = 1

      save( bs, file=fn.det, compress=T )
			return(fn)
		
		}
	

  }




