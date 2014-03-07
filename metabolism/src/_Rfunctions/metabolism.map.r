
  metabolism.map = function( ip=NULL, p, type="annual" ) { 

    if (exists( "init.files", p)) loadfilelist( p$init.files ) 
    if (exists( "libs", p)) loadlibraries( p$libs ) 
    if (is.null(ip)) ip = 1:p$nruns

    if ( type=="annual" ) {

      my = metabolism.db( DS="metabolism.merged", p=p )
 
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
        
        #         if (v=="mr") dr=c( 10^0.5, 10^5.5  )
        #         if (v=="smr") dr=c(10^-2.50, 10^-2.10 )
        #         if (v=="A") dr=c(0.0020, 0.0060 )
        #         if (v=="Ea")   dr=c( -1400, -200 )
        #         if (v=="len") dr=c( 15, 60 )
        #         if (v=="mass")    dr=c( 0.1, 1.2 )
        #         if (v=="Pr.Reaction") dr = c(1, 2 ) 
        #         if (v=="qm") dr=c( 0.1, 0.9 )
        #         if (v=="qn") dr=c( 0.1, 0.9 )
        #         if (v=="zm") dr=c( 0.25, 0.70 )
        #         if (v=="zn") dr=c( 0.25, 0.70 )
                    
        dr = quantile(my[,v], probs=c(0.05, 0.95), na.rm=T)

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


