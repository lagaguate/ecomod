

  habitat.interpolate = function( ip=NULL,  p=NULL, DS="saved", vname=NULL, yr=NULL ) {

    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    
    outdir = file.path( p$project.outdir.root, p$spatial.domain, p$season, p$modtype, "interpolations" )
    dir.create( outdir, showWarnings=FALSE, recursive=TRUE )
       
    timemethod = "default"
    if ( all( is.finite(( p$movingdatawindow )) )) timemethod = "windowed"


    if (DS=="all") {
      # glue all variables for 1 year
      hdat = habitat.db( DS="baseline", p=p )  
      for ( vn in  p$varstomodel ) {
        fn = file.path( outdir, paste( "interpolations", vn, yr, "rdata", sep=".") )
        if( file.exists(fn)) {
          load( fn)
          hdat[, vn] = HD
        }
      }
      return ( hdat )
    }

 
    if (DS=="saved") {
      HD = NULL
      fn = file.path( outdir, paste( "interpolations", vname, yr, "rdata", sep=".") )
      if( file.exists(fn)) load( fn)
      return ( HD )
    }
    
    if (is.null(ip)) ip = 1:p$nruns

    # load raw point data to determine safe ranges
    pdat = habitat.db( DS=p$project.name, p=p ) 
    dr = list()
    for ( ww in p$varstomodel ) {
      dr[[ww]] = quantile( pdat[,ww], probs=c(0.005, 0.995), na.rm=TRUE ) # use 95%CI
    }
    rm (pdat); gc()


    P0 = habitat.db( DS="baseline", p=p )  
    P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )  ## TODO:: make this a generic resolution change
    P0 = P0[, c( "platplon", "plon", "plat", "z", "dZ", "ddZ", "substrate.mean" ) ]
    
    if ( timemethod=="default" ){ 
      #if not moving window
      
      for ( iip in ip ) { 
        yr = p$runs[iip,"yrs"]
        print( p$runs[iip,])
        
        td = temperature.db( year=yr, p=p, DS="complete")
        d$platplon = paste( round( td$plat ), round(td$plon), sep="_" )  ## TODO:: make this a generic resolution change
        td = td[ , setdiff(names(td), c( "z", "yr", "plon", "plat") )  ]
        hdat = merge( P0, td, by=c("platplon"), all.x=TRUE, all.y=FALSE )
        rm( td); gc()
        
        hdat$yr = yr # update all other records
        hdat$chron = string2chron( paste( paste( yr, p$habitat.predict.time.julian, sep="-" ), "12:00:00") )  # for time-dependent lookups
        hdat$julian = convert.datecodes(  hdat$chron, "julian" )
        hdat = habitat.lookup( hdat, p=p, DS="temperature" ) 

        for ( ww in p$varstomodel ) {
          mod.cond = habitat.model( p=p, vn=ww, yr=yr )
          if (is.null( mod.cond )) next()
        fn = file.path( outdir, paste( "interpolations", ww, yr, "rdata", sep=".") )
        if(file.exists(fn)) next()
        sol = try( predict( mod.cond, newdata=hdat, type="response", na.action="na.pass") )
        if  ( "try-error" %in% class(sol) ) {
          hdat[,ww] = NA
        } else { 
          hdat[,ww] = sol
        }
        # debug:: require (lattice); levelplot( mr ~ plon+plat, hdat, aspect="iso")
        HD = hdat[,ww]
     
        iu = which(HD > dr[[ww]][2])
        if (length( iu)>0) HD[iu] = dr[[ww]][2]
   
        id = which(HD < dr[[ww]][1])
        if (length( id)>0) HD[id] = dr[[ww]][1]

        fn = file.path( outdir, paste( "interpolations", ww, yr, "rdata", sep=".") )
        save ( HD, file=fn, compress=T )
        print(fn)
      }
      }   
    }


    #moved down to see if speeds up
    
    if ( timemethod=="windowed" ){ 
      for ( iip in ip ) {
        ww = p$runs[iip,"vars"]
        mod.cond = habitat.model( p=p, vn=ww, yr=yr )
        if (is.null( mod.cond )) next()
    
        for(yr in p$yearstomodel) {
          td = temperature.db( year=yr, p=p, DS="complete")
          td$platplon = paste( round( td$plat ), round(td$plon), sep="_" )  ## TODO:: make this a generic resolution change
          td = td[ , setdiff(names(td), c( "z", "yr", "plon", "plat") )  ]
          hdat = merge( P0, td, by=c("platplon"), all.x=TRUE, all.y=FALSE )
          rm( td); gc()

          hdat$yr = yr # update all other records
          hdat$chron = string2chron( paste( paste( yr, p$habitat.predict.time.julian, sep="-" ), "12:00:00") )  # for time-dependent lookups
          hdat$julian = convert.datecodes(  hdat$chron, "julian" )
          hdat = habitat.lookup( hdat, p=p, DS="temperature" ) 
# to here
          # fn = file.path( outdir, paste( "interpolations", ww, yr, "rdata", sep=".") )
          # if(file.exists(fn)) next()
          sol = try( predict( mod.cond, newdata=hdat, type="response", na.action="na.pass") )
          if  ( "try-error" %in% class(sol) ) {
            hdat[,ww] = NA
          } else { 
            hdat[,ww] = sol
          }
          # debug:: require (lattice); levelplot( mr ~ plon+plat, hdat, aspect="iso")
          HD = hdat[,ww]
       
          iu = which(HD > dr[[ww]][2])
          if (length( iu)>0) HD[iu] = dr[[ww]][2]
     
          id = which(HD < dr[[ww]][1])
          if (length( id)>0) HD[id] = dr[[ww]][1]

          fn = file.path( outdir, paste( "interpolations", ww, yr, "rdata", sep=".") )
          save ( HD, file=fn, compress=T )
          print(fn)
        }
      }
    } 
    return( "Completed spatial interpolations" )
  }



