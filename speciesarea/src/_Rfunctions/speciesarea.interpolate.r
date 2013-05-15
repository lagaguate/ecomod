
  speciesarea.interpolate = function( ip=NULL, p=NULL, DS="saved", modtype=NULL, vname=NULL, yr=NULL ) {
            
    if (DS=="all") {
      # glue all variables for 1 year
      sc = habitat.db( DS="baseline", p=p )  
      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, modtype )
      for ( vn in  p$varstomodel ) {
        fn = file.path( ddir, paste("speciesarea.annual.gridded", vn, yr, "rdata", sep=".") )
        if( file.exists(fn)) {
          load( fn)
          sc[, vname] = SC
        }
      }
      return ( sc )
    }

 
    if (DS=="saved") {
      SC = NULL
      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse=".")  , p$speciesarea.method, modtype )
      fn = file.path( ddir, paste("speciesarea.annual.gridded", vname, yr, "rdata", sep=".") )
      if( file.exists(fn)) load( fn)
      return ( SC )
    }
    
    require(snow)
    require(mgcv)

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
 
    for ( iip in ip ) {
      yr = p$runs[iip,"yrs"]
      modtype = p$runs[iip,"modtype"]

      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain,  p$taxa, p$season, paste(p$data.sources, collapse=".")  , p$speciesarea.method, modtype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
 
      P0 = habitat.db( DS="baseline", p=p )  
      P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )  ## TODO:: make this a generic resolution change
      P0 = P0[, c( "platplon", "plon", "plat", "z", "dZ", "ddZ", "substrate.mean" ) ]
    
      td = temperature.db( year=yr, p=p, DS="complete")
			td$platplon = paste( round( td$plat ), round(td$plon), sep="_" )  ## TODO:: make this a generic resolution change
      td = td[ , setdiff(names(td), c( "z", "yr", "plon", "plat") )  ]

      sc = merge( P0, td, by=c("platplon"), all.x=TRUE, all.y=FALSE )
      rm(td, P0) ; gc()

      sc$yr = yr
      sc$chron = string2chron( paste( paste( yr, p$habitat.predict.time.julian, sep="-" ), "12:00:00") )  # for time-dependent lookups
      sc$julian = convert.datecodes(  sc$chron, "julian" )
      sc$t = NA   
      sc$t = habitat.lookup.simple( sc,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 

      for( ww in p$varstomodel ) {
        mod.sar = speciesarea.model.spatial( p=p, modeltype=modtype, var=ww )
        sc[,ww] = predict( mod.sar, newdata=sc, type="response", na.action="na.pass" ) 
        # require (lattice)
        # levelplot( Z ~ plon+plat, sc, aspect="iso")
        SC = sc[,ww]
        fn = file.path(  ddir, paste("speciesarea.annual.gridded", ww, yr, "rdata", sep=".") )
        save ( SC, file=fn, compress=T )
        print(fn)
      }
    } 
    return( "Completed" )
  }


