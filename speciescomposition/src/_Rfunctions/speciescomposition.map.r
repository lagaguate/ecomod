
  speciescomposition.map = function( ip=NULL, p, type="annual" ) { 

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
		
     loadlibraries (p$libs)


    if ( type=="annual" ) {
       
      my = speciescomposition.db( DS="speciescomposition.merged", p=p )
      
      for ( iip in ip ) {
        y = p$runs[iip,"yrs"]
        v = p$runs[iip,"vars"]
        modtype= p$runs[iip,"modtype"]
        
        ddir = file.path( project.directory("speciescomposition"), "maps", p$spatial.domain, modtype, p$taxa, p$season, type  )
        dir.create(path=ddir, recursive=T, showWarnings=F)

        sc = habitat.db( DS="baseline", p=p )  
        sv = speciescomposition.interpolate ( p=p, yr=y, modtype=modtype, vname=v )

        if (is.null(sv)) next()
        sc[,v] = sv

        sc = sc[ filter.region.polygon( sc, region=c("4vwx", "5yz" ), planar=T, proj.type=p$internal.projection ) , ]
        outfn = paste( v, y, sep=".")
        annot = paste("Species composition: ", toupper(v), " (", y, ")", sep="")
        
        # if (v=="ca1") dr=c(-2.5,2.5)
        # if (v=="ca2") dr=c(-2.5,2.5)
        # if (v=="pca1") dr=c(-0.45, 0.45)
        # if (v=="pca2") dr=c(-0.45, 0.45)

        dr = quantile( my[,v], probs=c(0.05, 0.95), na.rm=T )
    
        datarange = seq( dr[1], dr[2], length.out=100 )
      
        # levelplot( ca1 ~ plon+plat, sc, aspect="iso")
        sc = sc[, c("plon","plat",v)]
             
        il = which( sc[,v] < dr[1] )
        if ( length(il) > 0 ) sc[il,v] = dr[1]

        iu = which( sc[,v] > dr[2] )
        if ( length(iu) > 0 ) sc[iu,v] = dr[2]
        
        map( xyz=sc, cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, fn=outfn, loc=ddir, at=datarange , 
          col.regions=color.code( "blue.black", datarange ), corners=p$corners )
      }
    }

  }


