
  condition.interpolate = function( ip=NULL,  p=NULL, DS="saved", modtype=NULL, vname=NULL, yr=NULL ) {
 
    if (exists( "init.files", p)) loadfilelist( p$init.files ) 
    if (exists( "libs", p)) loadlibraries( p$libs ) 
           
    if (DS=="all") {
      # glue all variables for 1 year
      sc = habitat.db( DS="baseline", p=p )  
      ddir = file.path( project.directory("condition"), "data", p$spatial.domain, p$season, modtype )
      for ( vn in  p$varstomodel ) {
        fn = file.path( ddir, paste("condition.annual.gridded", vn, yr, "rdata", sep=".") )
        if( file.exists(fn)) {
          load( fn)
          sc[, vname] = SC
        }
      }
      return ( sc )
    }

 
    if (DS=="saved") {
      sc = NULL
      ddir = file.path( project.directory("condition"), "data", p$spatial.domain, p$season, modtype )
      fn = file.path( ddir, paste("condition.annual.gridded", vname, yr, "rdata", sep=".") )
      if( file.exists(fn)) load( fn)
  
      return ( SC )
    }
    
    if (is.null(ip)) ip = 1:p$nruns

    for ( iip in ip ) {
      yr = p$runs[iip,"yrs"]
      modtype = p$runs[iip,"modtype"]
      print( p$runs[iip,])

      ddir = file.path( project.directory("condition"), "data", p$spatial.domain, p$season, modtype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
    
      my = condition.db( DS="condition", p=p )

      P0 = habitat.db( DS="baseline", p=p )  
      P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )  ## TODO:: make this a generic resolution change
      P0 = P0[, c( "platplon", "plon", "plat", "z", "dZ", "ddZ", "substrate.mean" ) ]
      
      td = temperature.db( year=yr, p=p, DS="complete")
			td$platplon = paste( round( td$plat ), round(td$plon), sep="_" )  ## TODO:: make this a generic resolution change
      td = td[ , setdiff(names(td), c( "z", "yr", "plon", "plat") )  ]

      sc = merge( P0, td, by=c("platplon"), all.x=TRUE, all.y=FALSE )
      rm( P0, td); gc()

      sc$yr = yr # update all other records
      
      sc$chron = string2chron( paste( paste( yr, p$habitat.predict.time.julian, sep="-" ), "12:00:00") )  # for time-dependent lookups
      sc$julian = convert.datecodes(  sc$chron, "julian" )
   
      sc$t = NA
      sc$t = habitat.lookup.simple( sc,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 
 
      for ( ww in p$varstomodel ) {
  
        mod.cond = condition.model( p=p, modeltype=modtype, var=ww, yr=yr )
        if (is.null( mod.cond )) next()
        sol = try( predict( mod.cond, newdata=sc, type="response", na.action="na.pass") )
        if  ( "try-error" %in% class(sol) ) {
          sc[,ww] = NA
        } else { 
          sc[,ww] = sol
        }

        # require (lattice)
        # levelplot( mr ~ plon+plat, sc, aspect="iso")
        SC = sc[,ww]
     
        myr = range( my[,ww], na.rm=T )   
        iu = which(SC > myr[2])
        if (length( iu)>0) SC[iu] = myr[2]
   
        id = which(SC < myr[1])
        if (length( id)>0) SC[id] = myr[1]

        fn = file.path( ddir, paste("condition.annual.gridded", ww, yr, "rdata", sep=".") )
        save ( SC, file=fn, compress=T )
        print(fn)
      }
    } 
    return( "Completed" )
  }



