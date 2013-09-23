
  metabolism.map = function( ip=NULL, p, type="annual" ) { 

    if (!is.null(p$init.files)) for( i in p$init.files ) source (i)
    if (is.null(ip)) ip = 1:p$nruns
		
    require( lattice )
    require (grid)
    
   
    if ( type=="annual" ) {
      for ( iip in ip ) {
        y = p$runs[iip,"yrs"]
        v = p$runs[iip,"vars"]
        modtype= p$runs[iip,"modtype"]

        outdir = file.path( project.directory("metabolism"), "maps", p$spatial.domain, modtype, p$taxa, p$season, type )
        dir.create(path=outdir, recursive=T, showWarnings=F)
        
        sc = habitat.db( DS="baseline", p=p )  
               
        sv = metabolism.interpolate( p=p, yr=y, modtype=modtype, vname=v )
        if (is.null(sv)) next()

        sc[,v] =  sv

        sc = sc[ filter.region.polygon( sc, region=c("4vwx", "5yz" ), planar=T, proj.type=p$internal.projection ) , ]
        outfn = paste( v, y, sep=".")
        annot = paste("Metabolism: ", toupper(v), " (", y, ")", sep="")
        dr = NULL
        
     #   if (v=="mr") dr=c(3, 90)
     #   if (v=="mrA") dr=c(3, 100)
     #   if (v=="smr") dr=c(0.003, 0.0065)
     #   if (v=="smrA")   dr=c(0.0025, 0.0065)
     #   if (v=="totwgt") dr=c( 100, 10^5)
     #   if (v=="totno")    dr=c( 750, 10^6)
     #   if (v=="meanwgt")    dr=c( 0.01, 2.5)
     #   if (v=="meanlen")    dr=c( 1.0, 50)
        
        debug = T; if (debug) dr = quantile(sc[,v], probs=c(0.05, 0.95), na.rm=T)

        if ( v %in% c("mr", "smr") ) {
          sc[,v] = log10( sc[,v] )
          dr = log10(dr)
          annot = paste("Metabolism; log10: ", toupper(v), " (", y, ")", sep="")
        } 

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


