  
  sizespectrum.interpolate = function( ip=NULL, p=NULL, DS="saved", modtype=NULL, yr=NULL ) {
            
    if (DS=="saved") {
      sc = NULL
      ddir = file.path( project.directory("sizespectrum"), "data", p$spatial.domain, p$taxa, p$season, modtype )
      fn = file.path( ddir, paste("sizespectrum.annual.gridded", yr, "rdata", sep=".") )
      if( file.exists(fn)) load( fn)
      return ( sc )
    }
    
    require(snow)
    require(mgcv)


    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns


    P0 = bathymetry.db( p=p, DS="baseline" )  # prediction surface appropriate to p$spatial.domain, already in ndigits = 2
    P0$platplon = paste( round( P0$plat ), round(P0$plon), sep="_" )

    ks = sizespectrum.db( DS="sizespectrum.stats.filtered", p=p )

    for ( iip in ip ) {

      yr = p$runs[iip,"yr"]
      modtype = p$runs[iip,"modtype"]

      ddir = file.path( project.directory("sizespectrum"), "data", p$spatial.domain,  p$taxa, p$season, modtype )
      dir.create( ddir, showWarnings=FALSE, recursive=TRUE )
      fn = file.path(  ddir, paste("sizespectrum.annual.gridded", yr, "rdata", sep=".") )

      sc = merge( P0, ks[ which(ks$yr==yr), ], by="platplon", all.x=T, all.Y=F, sort= F)
      ii = which(!is.finite( sc$z) )
      if (length(ii)>0) sc$z[ii] = sc$z.set[ii]
      sc$z.set = NULL

      if (nrow(sc) != nrow(P0) ) {
        # some duplicates created by merge, keep first match and remove the rest 
        dups = which( duplicated( sc$platplon ) )
        if (length(dups) > 0 ) {
          sc = merge( P0, sc[-dups,], by="platplon", all.x=T, all.Y=F, sort= F, suffixes=c("",".merge") )
          sc$plon.merge = sc$plat.merge = NULL
        }
      }
    
      # assign fixed prediction times
      sc$yr = yr
      sc$julian = 8/12 * 365  # 1 sept 
      sc$chron = string2chron( paste( paste( yr, "Sep", "01", sep="-" ), "12:00:00") )  # required for time-dependent lookups

      sc$t = habitat.lookup.simple( sc,  p=p, vnames="temp", lookuptype="temperature.weekly", sp.br=p$interpolation.distances ) 

      # reduce dataframe size to reduce memory requirements/improve speed
      sc = sc[ , c("plon","plat", "yr", "julian", "z", "t", p$varstomodel ) ]

      gc()
      
      if (modtype=="complex") { 
        loadfunctions( "habitat")
        sc = habitat.lookup(x=sc, p=p, dist.scale=p$dist.scale, keep.lon.lat=TRUE, datatype="all.data"  )
      }
  
      for( ww in p$varstomodel ) {
        print (ww)
        idata = which( is.finite( sc[,ww] ) )
        scrange = range( sc[idata,ww], na.rm=T )
        inodata = which( !is.finite( sc[,ww] ) )
        
        mod.nss = sizespectrum.model.spatial( p=p, modeltype=modtype, var=ww )
        require(mgcv)
        sc[inodata,ww] = predict( mod.nss, newdata=sc[inodata,], type="response", na.action="na.pass" ) 
        
        # require (lattice)
        # levelplot( totwgt ~ plon+plat, sc[which(sc[,ww] >-3 & sc$ca1< 3),], aspect="iso")
        # levelplot( mr ~ plon+plat, sc, aspect="iso")
        # levelplot( totwgt ~ plon+plat, sc, aspect="iso")
        
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


