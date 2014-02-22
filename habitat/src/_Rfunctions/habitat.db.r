
  habitat.db = function( ip=NULL, DS="baseline", p=NULL, year=NULL ) {
 
    if (DS %in% c("baseline", "baseline.redo") ) {
      
      # form a basic prediction surface in planar coords for SS habitat for 
      # factors that do not "change" rapidly and 
    
      outdir = file.path( project.directory("habitat"), "data", p$spatial.domain, "baseline" )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.directory("habitat"), "data", "SSE", "baseline" )
      
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



    if (DS %in% c("complete", "complete.redo") ) {
      
      outdir =  file.path( project.directory("habitat"), "data", p$spatial.domain, "complete" )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.directory("habitat"), "data", "SSE","complete" )
      dir.create(outdir, recursive=T, showWarnings=F)
   
      if ( DS=="complete" ) {
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
      if (!is.null(p$init.files )) for( i in  p$init.files ) source (i)
      if (is.null(ip)) ip = 1:length(p$yearstomodel)
      ip = as.numeric(ip)   # indexing variable (year) of the serial or parallel run
    
      loadlibraries (p$libs)

      for (iy in ip) {
        yr = p$yearstomodel[iy]
        print (yr)
        outfile =  file.path( outdir, paste( "PS", yr, "rdata", sep= ".") )
       
        PS = habitat.db( DS="baseline", p=p )  
        PS$id = 1:nrow(PS)
        
        E = temperature.db( DS="complete", p=p, year=yr  ) 
        E$z = NULL

        PS = merge( PS, E,  by =c("plon", "plat"), all.x=T, all.y=F, sort=F)

        # ---------------------
        # Species-area
        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = p
        pm$taxa = p$speciesarea.taxa
        pm$season = p$speciesarea.season
        pm$data.sources = p$speciesarea.data.sources
        pm$varstomodel = pm$speciesarea.variables

        SAG =  speciesarea.interpolate( DS="all", p=pm, yr=max(1970,yr) , modtype=pm$speciesarea.modeltype  )

        # remove duplicates derived from repeated tows
        oo = which( duplicated (SAG$platplon ) )
        if (length( oo)> 0 ) {
          todrop= NULL
          for (o in oo ) {
            i = which( SAG$platplon == SAG$platplon[o] )
            for (w in pm$speciesarea.variables ) {
              SAG[i[1],w] = mean(SAG[i,w], na.rm=TRUE)
              todrop = c(todrop, i[-1])
            }
          }
          SAG = SAG[ -todrop, ]
        }
        SAG = SAG[ , c("plon", "plat", pm$speciesarea.variables ) ]
        PS = merge( PS, SAG, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".sag" ) ) 
        rm (SAG)


        # ---------------------
        # Species composition
        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = p
        pm$taxa = p$speciescomposition.taxa  
        pm$season = p$speciescomposition.season
        pm$varstomodel= pm$speciescomposition.variables

        SC = speciescomposition.interpolate( DS="all", p=pm, yr=max(1970,yr), modtype=pm$speciescomposition.modeltype ) 

        # remove duplicates derived from repeated tows --- slow ... 
        oo = which( duplicated (SC$platplon ) )
        if (length( oo)> 0 ) {
          todrop= NULL
          for (o in oo ) {
            i = which( SC$platplon == SC$platplon[o] )
            for (w in pm$speciescomposition.variables ) {
              SC[i[1],w] = mean(SC[i,w], na.rm=TRUE)
              todrop = c(todrop, i[-1])
            }
          }
          SC = SC[ -todrop, ]
        }
        SC = SC[ , c("plon", "plat", pm$speciescomposition.variables ) ]
        PS = merge( PS, SC, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".sc" ) ) 
        rm (SC)


        # ---------------------
        # size spectrum stats
        pm = p
        pm$taxa = p$sizespectrum.taxa
        pm$season = p$sizespectrum.season
        pm$varstomodel = pm$sizespectrum.variables

        SS = sizespectrum.interpolate ( DS="all", p=pm, yr=max(1970,yr), modtype=pm$sizespectrum.modeltype ) 
        
        # remove duplicates derived from repeated tows --- slow ... 
        oo = which( duplicated (SS$platplon ) )
        if (length( oo)> 0 ) {
          todrop= NULL
          for (o in oo ) {
            i = which( SS$platplon == SS$platplon[o] )
            for (w in pm$sizespectrum.variables) {
              SS[i[1],w] = mean(SS[i,w], na.rm=TRUE)
              todrop = c(todrop, i[-1])
            }
          }
          SS = SS[ -todrop, ]
        }
        SS = SS[ , c("plon", "plat", pm$sizespectrum.variables ) ]
        PS = merge( PS, SS, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".ss" ) ) 
        rm(SS)


        # --------------- 
        # metabolic rates
        # no biological data prior to 1970 .. fill with data from 1970 until another solution is found
        pm = p
        pm$taxa = p$metabolism.taxa 
        pm$season = p$metabolism.season 
        pm$varstomodel = p$metabolism.variables 

        MR = metabolism.interpolate ( DS="all", p=pm, yr=max(1970,yr), modtype=pm$metabolism.modeltype ) 
    
                
        # remove duplicates derived from repeated tows --- slow ... 
        oo = which( duplicated (MR$platplon ) )
        if (length( oo)> 0 ) {
          todrop= NULL
          for (o in oo ) {
            i = which( MR$platplon == MR$platplon[o] )
            for (w in pm$metabolism.variables) {
              MR[i[1],w] = mean(MR[i,w], na.rm=TRUE)
              todrop = c(todrop, i[-1])
            }
          }
          MR = MR[ -todrop, ]
        }
        MR = MR[ , c("plon", "plat", pm$metabolism.variables ) ]
        PS = merge( PS, MR, by =c("plon", "plat"), all.x=T, all.y=F, sort=F, suffixes=c("", ".mr" ) ) 
        rm(MR)


        vars = setdiff( names(PS), c("plon", "plat") )
        require (gstat)
        for (v in vars) {
          for (dists in p$interpolation.distances) { 
            ii = which ( !is.finite( PS[, v]) )
            if (length(ii)==0 | length(ii)==nrow(PS) ) break()
            print( paste( "Interpolating missing data with inverse-distance weighted means", v, ":", length(ii) ) )
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




    if (DS %in% c("complete_no.biologicals", "complete_no.biologicals.redo") ) {
      
      outdir =  file.path( project.directory("habitat"), "data", p$spatial.domain, "complete_no.biologicals" )
      if ( p$spatial.domain =="snowcrab" ) outdir = file.path( project.directory("habitat"), "data", "SSE",  "complete_no.biologicals"  )
      dir.create(outdir, recursive=T, showWarnings=F)
   
      if ( DS=="complete_no.biologicals" ) {
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
      if (!is.null(p$init.files)) for( i in  p$init.files ) source (i)
      if (is.null(ip)) ip = 1:length(p$yearstomodel)
      ip = as.numeric(ip)   # indexing variable (year) of the serial or parallel run
    
      for (iy in ip) {
        yr = p$yearstomodel[iy]
        print (yr)
        outfile =  file.path( outdir, paste( "PS", yr, "rdata", sep= ".") )
       
        PS = habitat.db( DS="baseline", p=p )  
        PS$id = 1:nrow(PS)
        
        E = temperature.db( DS="complete", p=p, year=yr  ) 
        E$z = NULL

        PS = merge( PS, E,  by =c("plon", "plat"), all.x=T, all.y=F, sort=F)
               vars = setdiff( names(PS), c("plon", "plat") )
        require (gstat)
        for (v in vars) {
          for (dists in p$interpolation.distances) { 
            ii = which ( !is.finite( PS[, v]) )
            if (length(ii)==0 | length(ii)==nrow(PS) ) break()
            print( paste( "Interpolating missing data with inverse-distance weighted means", v, ":", length(ii) ) )
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


