
  metabolism.interpolate = function( ip=NULL, p=NULL, DS="saved", modtype=NULL, vname=NULL, yr=NULL ) {
 
    if (DS=="all") {
      # glue all variables for 1 year
      sc = habitat.db( DS="baseline", p=p )  
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modtype )
      for ( vn in  p$varstomodel ) {
        fn = file.path( ddir, paste("metabolism.annual.gridded", vn, yr, "rdata", sep=".") )
        if( file.exists(fn)) {
          load( fn)
          sc[, vname] = SC
        }
      }
      return ( sc )
    }


    if (DS=="saved") {
      SC = NULL
      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modtype )
      fn = file.path( ddir, paste("metabolism.annual.gridded", vname, yr, "rdata", sep=".") )
      if( file.exists(fn)) load( fn)
      return ( SC )
    }
    
    require(chron)
    require(snow)
    require(mgcv)

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
 
    P0 = habitat.db( DS="baseline", p=p )  
    P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )  ## TODO:: make this a generic resolution change
    P0 = P0[, c( "platplon", "plon", "plat", "z", "dZ", "ddZ", "substrate.mean" ) ]

    ks = metabolism.db( DS="metabolism", p=p )
    ks = ks[ , c("yr", "platplon", "t", p$varstomodel ) ]
    gc()


    for ( iip in ip ) {
      yr = p$runs[iip,"yrs"]
      modtype = p$runs[iip,"modtype"]

      ddir = file.path( project.directory("metabolism"), "data", p$spatial.domain, p$taxa, p$season, modtype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
       
      td = temperature.db( year=yr, p=p, DS="complete")
			td$platplon = paste( round( td$plat ), round(td$plon), sep="_" )  ## TODO:: make this a generic resolution change
      td = td[ , setdiff(names(td), c( "z", "yr", "plon", "plat") )  ]

      PS = merge( P0, td, by=c("platplon"), all.x=TRUE, all.y=FALSE )
 
      sc = merge( PS, ks[ which(ks$yr==yr), ], by="platplon", all.x=TRUE, all.y=FALSE )
    
      sc$yr = yr # update all other records
      sc$chron = string2chron( paste( paste( yr, p$habitat.predict.time.julian, sep="-" ), "12:00:00") )  # for time-dependent lookups
      sc$julian = convert.datecodes(  sc$chron, "julian" )
      
      sc$t = habitat.lookup.simple( sc,  p=p, vnames="t", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 
 
      for ( ww in p$varstomodel ) {

        idata = which( is.finite( sc[,ww] ) )
        scrange = range( sc[idata,ww], na.rm=T )
        inodata = which( !is.finite( sc[,ww] ) )
        
        mod.metab = metabolism.model( p=p, modeltype=modtype, var=ww )
        sc[inodata,ww] = predict( mod.metab, newdata=sc[inodata,], type="response", na.action="na.pass" ) 
        
        # require (lattice)
        # levelplot( totwgt ~ plon+plat, sc[which(sc[,ww] >-3 & sc$ca1< 3),], aspect="iso")
        # levelplot( mr ~ plon+plat, sc, aspect="iso")
        # levelplot( totwgt ~ plon+plat, sc, aspect="iso")
        
        # no extrapolation .. truncation to data extremes
        ooo = which( sc[,ww] < scrange[1])  
        if (length(ooo) > 0 ) sc[ooo,ww] = scrange[1]
        ppp = which( sc[,ww] > scrange[2])  
        if (length(ppp) > 0 ) sc[ppp,ww] = scrange[2]
        
        SC = sc[,ww]
        fn = file.path( ddir, paste("metabolism.annual.gridded", ww, yr, "rdata", sep=".") )
        save ( SC, file=fn, compress=T )
        
        print(fn)


      }
    } 
    
    return( "Completed" )

  }



