
  condition.map = function( ip=NULL, p, type="annual" ) { 

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
		
    require( lattice )
    require (grid)
    
   
    if ( type=="annual" ) {
        
      my = condition.db( DS="condition", p=p )
 
      for ( iip in ip ) {
        y = p$runs[iip,"yrs"]
        v = p$runs[iip,"vars"]
        modtype= p$runs[iip,"modtype"]

        outdir = file.path( project.directory("condition"), "maps", p$spatial.domain, modtype, p$season, type )
        dir.create(path=outdir, recursive=T, showWarnings=F)

        sc = habitat.db( DS="baseline", p=p )  
 
        sv = condition.interpolate( p=p, yr=y, modtype=modtype, vname=v ) 
        if (is.null(sv)) next()

        sc[,v] =  sv

        sc = sc[ filter.region.polygon( sc, region=c("4vwx", "5yz" ), planar=T, proj.type=p$internal.projection ) , ]
        outfn = paste( v, y, sep=".")
        annot = paste("Condition: ", toupper(v), " (", y, ")", sep="")
        dr = NULL
        
        if (v=="condition_special") dr=c(-3, 3)
           
        dr = quantile( my[,v], probs=c(0.05, 0.95), na.rm=T)

        datarange = seq( dr[1], dr[2], length.out=100 )
        
        il = which( sc[,v] < dr[1] )
        if ( length(il) > 0 ) sc[il,v] = dr[1]

        iu = which( sc[,v] > dr[2] )
        if ( length(iu) > 0 ) sc[iu,v] = dr[2]

        map( xyz= sc[, c("plon","plat",v)], cfa.regions=F, depthcontours=T, 
            pts=NULL, annot=annot, fn=outfn, loc=outdir, at=datarange , 
            col.regions= color.code( "blue.black", datarange ), corners=p$corners )
      }
    }
  }


