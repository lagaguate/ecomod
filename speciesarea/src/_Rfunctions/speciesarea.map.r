

  speciesarea.map = function( ip=NULL, p, type="annual" ) { 

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns

		require( lattice )
    require (grid)
   
    if ( type=="annual" ) {
          
      for ( iip in ip ) {

        y = p$runs[iip,"yrs"]
        v = p$runs[iip,"vars"]
        modtype = p$runs[iip,"modtype"]

        sag = speciesarea.interpolate( p=p, yr=y, modtype=modtype ) 
        if (is.null(sag)) next()
        sag = sag[ filter.region.polygon( sag, region=c("4vwx", "5yz" ), planar=T, proj.type=p$internal.projection ) , ]
  
        outdir = file.path( project.directory("speciesarea"), "maps", p$spatial.domain, p$taxa, p$season, p$speciesarea.method, modtype, type ) 
        dir.create(path=outdir, recursive=T, showWarnings=F)

        outfn = paste( v, y, sep=".")
        annot = paste("Species-area: ", v, " (", y, ")", sep="")
        
        dr = NULL
        if (v=="C") dr=c(-1, 3)
        if (v=="Npred") dr=c(10, 100)
        if (v=="sar.rsq") dr=c(0.9, 1.0)
        if (v=="Z") dr=c(0.1, 0.5)
        if (v=="T") dr=c(0.1, 0.5)
        if (is.null(dr)) dr = range(sag[,v], na.rm=T)
        
        datarange = seq( dr[1], dr[2], length.out=100 )
        cols = color.code( "blue.black", datarange )
        xyz = sag[, c("plon","plat",v)]
        
        map( xyz=xyz, cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, fn=outfn, loc=outdir, 
            at=datarange, col.regions=cols, corners=p$corners )

      }
    }

  }
  
