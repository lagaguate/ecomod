

  habitat.map = function ( ip=NULL, p ) {

    # dta contains the uninterpolated raw point data 

    if (exists( "init.files", p)) loadfilelist( p$init.files ) 
    if (exists( "libs", p)) loadlibraries( p$libs ) 

    if (is.null(ip)) ip = 1:p$nruns
   
    outdir = file.path( p$project.outdir.root, p$spatial.domain, p$season, p$modtype, "maps" )
    dir.create(path=outdir, recursive=T, showWarnings=F)
    
    pdat = habitat.db( DS=p$project.name, p=p ) 
    dr = list()
    for ( ww in p$varstomodel ) {
      dr[[ww]] = quantile( pdat[,ww], probs=c(0.025, 0.975), na.rm=TRUE ) # use 95%CI
      if ( ww %in% c("mr", "smr") ) {
        # exceptions that require log tranforms go here and below as well)
        dr[[ww]] = log10(dr[[ww]])
      }
    }
    rm (pdat); gc()

      for ( iip in ip ) {
        
        print( p$runs[iip,] )
        
        yr = p$runs[iip,"yrs"]
        ww = p$runs[iip,"vars"]

        hd = habitat.db( DS="baseline", p=p )  
        hi = habitat.interpolate( p=p, yr=yr, vname=ww ) 
        if (is.null(hi)) next()
        hd[,ww] =  hi
    
        if ( ww %in% c("mr", "smr") ) {
          # exceptions that require log tranforms go here and above as well
          hd[,ww] = log10( hd[,ww] )
          annot = paste( capwords(p$project.name), " (log10) : ", capwords(ww), " (", yr, ")", sep="")
        } 
        
        hd = hd[ filter.region.polygon( hd, region=c("4vwx", "5yz" ), planar=T, proj.type=p$internal.projection ) , ]
        annot = paste( capwords(p$project.name), " : ", capwords(ww), " (", yr, ")", sep="")
        datarange = seq( dr[[ww]][1], dr[[ww]][2], length.out=100 )
 
        il = which( hd[,ww] < dr[[ww]][1] )
        if ( length(il) > 0 ) hd[il,ww] = dr[[ww]][1]

        iu = which( hd[,ww] > dr[[ww]][2] )
        if ( length(iu) > 0 ) hd[iu,ww] = dr[[ww]][2]
      
        colourmap = color.code( "blue.black", datarange )
        outfn = paste( "maps", ww, yr, sep=".")

        map( xyz= hd[, c("plon", "plat", ww)], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
            fn=outfn, loc=outdir, at=datarange, col.regions=colourmap, corners=p$corners )
      }

    return("Completed mapping")
  }


