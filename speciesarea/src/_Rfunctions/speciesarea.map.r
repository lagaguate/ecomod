

  speciesarea.map = function( ip=NULL, p, type="annual" ) { 

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns

		require( lattice )
    require (grid)
   
    if ( type=="annual" ) {
           
      my = speciesarea.db( DS="speciesarea.stats.merged", p=p )

      for ( iip in ip ) {

        y = p$runs[iip,"yrs"]
        v = p$runs[iip,"vars"]
        modtype = p$runs[iip,"modtype"]
  
        sag = habitat.db( DS="baseline", p=p )  
        
        sv =  speciesarea.interpolate( p=p, yr=y, modtype=modtype, vname=v )

        if (is.null(sv)) next()
        sag[,v] = sv
        sag = sag[ filter.region.polygon( sag, region=c("4vwx", "5yz" ), planar=T, proj.type=p$internal.projection ) , ]
  
        outdir = file.path( project.directory("speciesarea"), "maps", p$spatial.domain, p$taxa, p$season, p$speciesarea.method, modtype, type ) 
        dir.create(path=outdir, recursive=T, showWarnings=F)

        outfn = paste( v, y, sep=".")
        annot = paste("Species-area: ", v, " (", y, ")", sep="")
                
        dr = quantile( my[,v], probs=c(0.05, 0.95), na.rm=T )

        # if (v=="C") dr=c(-3, 4)
        # if (v=="Npred") dr=c(20, 110)
        # if (v=="sar.rsq") dr=c(0.9, 1.1)
        # if (v=="Z") dr=c(-0.2, 0.9)
        # if (v=="T") dr=c(-0.2, 0.9)
        # if (is.null(dr)) dr = range(sag[,v], na.rm=T)
        
        datarange = seq( dr[1], dr[2], length.out=100 )
        cols = color.code( "blue.black", datarange )
        sc = sag[, c("plon","plat",v)]
             
        il = which( sc[,v] < dr[1] )
        if ( length(il) > 0 ) sc[il,v] = dr[1]

        iu = which( sc[,v] > dr[2] )
        if ( length(iu) > 0 ) sc[iu,v] = dr[2]

        map( xyz=sc, cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, fn=outfn, loc=outdir, 
            at=datarange, col.regions=cols, corners=p$corners )

      }
    }

  }
  
