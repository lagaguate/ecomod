
  speciesarea.interpolate = function( ip=NULL, p=NULL, DS="saved", modtype=NULL, yr=NULL ) {
            
    if (DS=="saved") {
      sc = NULL
      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain, p$taxa, p$season, paste(p$data.sources, collapse=".")  , p$speciesarea.method, modtype )
      fn = file.path( ddir, paste("speciesarea.annual.gridded", yr, "rdata", sep=".") )
      if( file.exists(fn)) load( fn)
      return ( sc )
    }
    
    require(snow)
    require(mgcv)


    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns

 
    P0 = habitat.db( DS="baseline", p=p )  
    P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )  ## TODO:: make this a generic resolution change
    P0 = P0[, c( "platplon", "plon", "plat", "z", "dZ", "ddZ", "substrate.mean" ) ]

    ks = speciesarea.db( DS="speciesarea.stats.merged", p=p )
    ks = ks[ , c("yr", "platplon", "t", p$varstomodel ) ]
    gc()

    for ( iip in ip ) {

      yr = p$runs[iip,"yrs"]
      modtype = p$runs[iip,"modtype"]

      ddir = file.path( project.directory("speciesarea"), "data", p$spatial.domain,  p$taxa, p$season, paste(p$data.sources, collapse=".")  , p$speciesarea.method, modtype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn = file.path(  ddir, paste("speciesarea.annual.gridded", yr, "rdata", sep=".") )
     
      td = temperature.db( year=yr, p=p, DS="complete")
			td$platplon = paste( round( td$plat ), round(td$plon), sep="_" )  ## TODO:: make this a generic resolution change
      td = td[ , setdiff(names(td), c( "z", "yr", "plon", "plat") )  ]

      PS = merge( P0, td, by=c("platplon"), all.x=TRUE, all.y=FALSE )
 
      sc = merge( PS, ks[ which(ks$yr==yr), ], by="platplon", all.x=TRUE, all.y=FALSE )
    
      sc$yr = yr
      sc$chron = string2chron( paste( paste( yr, p$habitat.predict.time.julian, sep="-" ), "12:00:00") )  # for time-dependent lookups
      sc$julian = convert.datecodes(  sc$chron, "julian" )
      
      sc$t = habitat.lookup.simple( sc,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 

      for( ww in p$varstomodel ) {
        idata = which( is.finite( sc[,ww] ) )
        scrange = range( sc[idata,ww], na.rm=T )
        inodata = which( !is.finite( sc[,ww] ) )
        
        mod.sar = speciesarea.model.spatial( p=p, modeltype=modtype, var=ww )
        sc[inodata,ww] = predict( mod.sar, newdata=sc[inodata,], type="response", na.action="na.pass" ) 
        
        # require (lattice)
        # levelplot( Z ~ plon+plat, sc, aspect="iso")
        
        # no extrapolation .. truncation to data extremes
        ooo = which( sc[,ww] < scrange[1])  
        if (length(ooo) > 0 ) sc[ooo,ww] = scrange[1]
        ppp = which( sc[,ww] > scrange[2])  
        if (length(ppp) > 0 ) sc[ppp,ww] = scrange[2]
      }
      save ( sc, file=fn, compress=T )
    } 
    return( "Completed" )
  }

