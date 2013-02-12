
  habitat.db = function( ip=NULL, DS="baseline", p=NULL, year=NULL ) {
 
    if (DS %in% c("baseline", "baseline.redo") ) {
      
      # form a basic prediction surface in planar coords for SS habitat for 
      # factors that do not "change" rapidly and 
    
      outdir = file.path( project.directory("habitat"), "data", p$spatial.domain )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.directory("habitat"), "data", "SSE" )
      
      dir.create(outdir, recursive=T, showWarnings=F)
      outfile =  file.path( outdir, "PS.baseline.rdata" )

      if ( DS=="baseline" ) {
        if (file.exists(outfile)) load( outfile )
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          PS = PS[ id, ]
        }
        return (PS)
      }
     
			# depth is the primary constraint 
      Z = bathymetry.db( p=p, DS="baseline" )  # SS to a depth of 500 m  the default used for all planar SS grids
      Z$id = 1:nrow(Z)
    
      dZ = bathymetry.db( p=p, DS="dZ.planar" )  #already log transformed
      ddZ = bathymetry.db( p=p, DS="ddZ.planar" )  # already log transformed
      S =  substrate.db ( p=p, DS="planar")
      S$substrate.mean = log(S$grainsize)
      S$grainsize = NULL
  
      PS = merge( Z, S, by =c("plon", "plat"), all.x=T, all.y=F, sort=F )
      PS = merge( PS, dZ, by  =c("plon", "plat"), all.x=T, all.y=F, sort=F )
      PS = merge( PS, ddZ, by  =c("plon", "plat"), all.x=T, all.y=F, sort=F )
     
      print( "Interpolating missing data with inverse-distance weighted means" )
        vars = setdiff( names(PS), c("plon", "plat") )
        require (gstat)
        for (v in vars) {
          print(v)
          for (dists in p$interpolation.distances) { 
            ii = which ( !is.finite( PS[, v]) )
            if (length(ii)==0) break()
            print( paste("N = ", length(ii), "data points") )
            gs = gstat( id=v, formula=PS[-ii,v]~1, locations=~plon+plat, data=PS[-ii,], 
                nmax=p$interpolation.nmax, maxdist=dists, set=list(idp=.5)) 
            PS[ii,v] = predict( object=gs, newdata=PS[ii,] ) [,3]
        }}
     
      PS = PS[ order( PS$id), ]
      PS$id = NULL

      save (PS, file=outfile, compress=T )
      return( outfile )
    }



    if (DS %in% c("annual", "annual.redo") ) {
      
      outdir =  file.path( project.directory("habitat"), "data", p$spatial.domain )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.directory("habitat"), "data", "SSE" )
      dir.create(outdir, recursive=T, showWarnings=F)
   
      if ( DS=="annual" ) {
        outfile =  file.path( outdir, paste( "PS", year, "rdata", sep= ".") )
        PS = NULL
        if ( file.exists( outfile ) ) load( outfile )
        if ( p$spatial.domain =="snowcrab" ) {
          id = bathymetry.db( DS="lookuptable.sse.snowcrab" )
          PS = PS[ id, ]
        }
   
        return (PS)
      }

      ####### "ip" is the first parameter expected when run in parallel mode .. do not move this one
      if (!is.null(p$env.init)) for( i in  p$env.init ) source (i)
      if (is.null(ip)) ip = 1:length(p$yearstomodel)
      ip = as.numeric(ip)   # indexing variable (year) of the serial or parallel run
    
      for (iy in ip) {
        yr = p$yearstomodel[iy]
        print (yr)
        outfile =  file.path( outdir, paste( "PS", yr, "rdata", sep= ".") )
       
        PS = habitat.db( DS="baseline", p=p )  
        PS$id = 1:nrow(PS)
        
        E = hydro.modelled.db( DS="bottom.statistics.annual", p=p, yr=yr  ) 
        

        PS = merge( PS, E,  by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".annual" ) )
        
        # names(PS)[ which(names(PS)=="tmean.annual") ] = "tmean.annual"
        names(PS)[ which(names(PS)=="tamplitude") ] = "tamp.annual"
        names(PS)[ which(names(PS)=="thalfperiod") ] = "thp.annual"

        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = p
        pm$taxa = p$speciesarea.taxa
        pm$season = p$speciesarea.season
        SAG =  speciesarea.interpolate( p=pm, yr=max(1970,yr) , modtype=pm$speciesarea.modeltype )
        SAG = SAG[ , c("plon", "plat", "C", "Z", "sar.rsq", "Npred" ) ]
        PS = merge( PS, SAG, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".sag" ) ) 
        rm (SAG)

        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = p
        pm$taxa = p$speciescomposition.taxa  
        pm$season = p$speciescomposition.season
        SC = speciescomposition.interpolate( p=pm, yr=max(1970,yr), modtype=pm$speciescomposition.modeltype ) 
        SC = SC[ , c( "plon", "plat", "ca1", "ca2", "pca1", "pca2"  ) ]
        PS = merge( PS, SC, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".sc" ) ) 
        rm(SC)

        # size spectrum stats
        pm = p
        pm$taxa = p$sizespectrum.taxa
        pm$season = p$sizespectrum.season
        SS = sizespectrum.interpolate ( p=pm, yr=max(1970,yr), modtype=pm$sizespectrum.modeltype ) 
        SS = SS[ , c( "plon", "plat", "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon", "nss.evenness", "nss.Hmax" ) ]
        PS = merge( PS, SS, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".ss" ) ) 
        rm(SS)

        # metabolic rates
        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = p
        pm$taxa = p$metabolism.taxa 
        pm$season = p$metabolism.season 
        MR = metabolism.interpolate ( p=pm, yr=max(1970,yr), modtype=pm$metabolism.modeltype ) 
        MR = MR[ , c( "plon", "plat", "meanlen", "meanwgt", "smr", "mr", "totno", "totwgt"  ) ]
        PS = merge( PS, MR, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".mr" ) ) 
        rm(MR)


        print( "Interpolating missing data with inverse-distance weighted means" )
        vars = setdiff( names(PS), c("plon", "plat") )
        require (gstat)
        for (v in vars) {
          print(v)
          for (dists in p$interpolation.distances) { 
            ii = which ( !is.finite( PS[, v]) )
            if (length(ii)==0 | length(ii)==nrow(PS) ) break()
            print( length(ii) )
            gs = gstat( id=v, formula=PS[-ii,v]~1, locations=~plon+plat, data=PS[-ii,], 
                nmax=p$interpolation.nmax, maxdist=dists, set=list(idp=.5)) 
            PS[ii,v] = predict( object=gs, newdata=PS[ii,] ) [,3]
        }}
           
        PS = PS[ order( PS$id ) ,]
        PS$id =NULL
        PS$Y = 1 # required to run "model.matrix"
        if (yr < min( p$yearstomodel ) ) PS$yr = max( p$yearstomodel )  # most recent year as reference

        save (PS, file=outfile, compress=T )
      }
      return( "Complete" )
    }
   
  }


