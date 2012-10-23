

  sizespectrum.map = function( ip=NULL, p, type="annual" ) { 

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns

		require( lattice )
    require (grid)

    if ( type=="annual" ) {
          
      for ( iip in ip ) {

        y = p$runs[iip,"y"]
        v = p$runs[iip,"v"]
        modtype = p$runs[iip,"modtype"]
	  
        outdir = file.path( project.directory("sizespectrum"), "maps", p$spatial.domain,  p$taxa, p$season, modtype, type ) 
        dir.create(path=outdir, recursive=T, showWarnings=F)

        sag =  sizespectrum.interpolate( p=p, yr=y, modtype=modtype ) 
        if (is.null(sag)) next()
        sag = sag[ filter.region.polygon( sag, region=c("4vwx", "5yz" ), planar=T, proj.type=p$internal.projection ) , ]

        outfn = paste( v, y, sep=".")
        annot = paste("Size spectrum: ", v, " (", y, ")", sep="")
        
        dr = NULL
        if (v=="nss.rsquared") dr=c(0, 1)
        if (v=="nss.df") dr=c(5, 95)
        if (v=="nss.b0") dr=c(-5, 2 )
        if (v=="nss.b1") dr=c(-5, 2 )
        if (v=="nss.shannon") dr=c(2, 6)
        if (v=="nss.evenness") dr=c(-2, 0 )
        if (v=="nss.Hmax") dr=c( -1, -8 )
        if (is.null(dr)) dr = range(sag[,v], na.rm=T)
        
        datarange = seq( dr[1], dr[2], length.out=50 )
        cols = color.code( "blue.black", datarange )
        xyz = sag[, c("plon","plat",v)]
        
        map( xyz=xyz, cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, fn=outfn, loc=outdir, 
            at=datarange, col.regions=cols, corners=p$corners )

      }
    }

  }


