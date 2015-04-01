 
  predict.discretised.habitat = function(ip=NULL, DS="", p=NULL, yr=NULL ){
 
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    if (is.null(ip)) ip = 1:p$nruns

    dir.create( file.path( project.datadirectory("habitatsuitability"), p$speciesofinterest ), recursive=T, showWarnings=F )
        
    if (DS %in% c( "model.pa", "model.pa.redo") ) {
      fn.model.pa = file.path( project.datadirectory("habitatsuitability"), p$speciesofinterest, "model.pa.rdata" )
      if (DS=="model.pa") {
        Q = NULL
        load(fn.model.pa)
        return(Q)
      }
      
      W = habitatsuitability.data.extract(p)

      W$plon = jitter(W$plon)
      W$plat = jitter(W$plat)

      AB = which( is.finite( W$presence + W$substrate.mean + W$tmean ) )
      
      if (length( p$optimizer) > 1) p$optimizer = p$optimizer[1]
      o = c( "outer", p$optimizer ) 
      if (p$optimizer=="perf") o=p$optimizer

      Q = gam( p$gam.model.pa, data=W[AB,], na.action="na.omit", family=binomial("logit"), optimizer=o  )
      save ( Q, file=fn.model.pa, compress=T )
      
      debug = F
      if (debug) {
        AIC (Q) # = 81590
        summary(Q)
        Qp = predict( Q, W[AB.], type="response")
        cor(Qp,W$presence[AB])^2 # = 0.41
        plot(Q, all.terms=T, rug=T, jit=T, seWithMean=T, pers=F , scale=0)
        vis.gam( Qa, view=c("plon", "plat"), plot.type="contour", n.grid=200, too.far=2 ) 
      }
    }
 
    
    if (DS %in% c( "model.ra", "model.ra.redo") ) {
      
      dir.create( file.path( project.datadirectory("habitatsuitability"), p$speciesofinterest ), recursive=T, showWarnings=F )
      fn.model.ra = file.path( project.datadirectory("habitatsuitability"), p$speciesofinterest, "model.ra.rdata" )
      
      if (DS=="model.ra") {
        Q = NULL
        load(fn.model.ra)
        return(Q)
      }
    
      W = habitatsuitability.data.extract(p)

      W$plon = jitter(W$plon)
      W$plat = jitter(W$plat)
      PA = which(  W$presence==1 &  is.finite( W$substrate.mean + W$tmean )  )
      W = W[PA,]

      if (length( p$optimizer) > 1) p$optimizer = p$optimizer[1]
      o = c( "outer", p$optimizer ) 
      if (p$optimizer=="perf") o=p$optimizer

      Q = gam( p$gam.model.ra, data=W, na.action="na.omit", family=gaussian(), optimizer=o )
      save ( Q, file=fn.model.ra, compress=T )
      
      debug = F
      if (debug) {
        AIC (Q) # = 37851.8
        summary(Q)
        require(boot)
        Qp = predict( Q, W, type="response")
        cor(Qp,W$presence[PA])^2 # = 0.41
        plot(Qp, all.terms=T, rug=T, jit=T, seWithMean=T, trans=inv.logit, pers=F , scale=0)
        vis.gam( Qp, view=c("plon", "plat"), plot.type="contour", n.grid=200, too.far=2, trans=inv.logit ) 
      }

    }
    

    if (DS %in% c( "sim", "sim.redo", "timeseries", "timeseries.redo", "timeseries.all" ) ) {

      gdir = file.path( project.datadirectory("habitatsuitability"), p$speciesofinterest, "grids" )
      tdir = file.path( project.datadirectory("habitatsuitability"), p$speciesofinterest, "timeseries" )

      dir.create( gdir, recursive=T, showWarnings=F )
      dir.create( tdir, recursive=T, showWarnings=F )
   
      if ( DS == "sim") {
        fn.ps = file.path( gdir, paste( "PS", yr, "rdata", sep=".")) 
        PS = NULL
        if (file.exists(fn.ps)) load( fn.ps)
        return( PS )
      }
    
      if ( DS == "timeseries") {
        fn.ts = file.path( tdir, paste( "TS", yr, "rdata", sep=".")) 
        TS = NULL
        if (file.exists(fn.ts)) load( fn.ts )
        return( TS )
      }


      if ( DS == "timeseries.all") {
        out = NULL
        for (yr in p$yearstomodel ) {
          fn.ts = file.path( tdir, paste( "TS", yr, "rdata", sep=".")) 
          TS = NULL
          if (file.exists(fn.ts)) load( fn.ts )
          if ( !is.null( (TS) )) out = rbind(out, TS )
        }
        return( out )
      }

      
      W = habitatsuitability.data.extract(p)
      qs = c(0.05, 0.95)   ### <<<< is this too small a range? >>  c(0.01, 0.99) 
      rm(W); gc()

      for ( iip in 1:p$nruns ) {
        
        y = p$runs[iip, "y"]
        
        fn.ts = file.path( tdir, paste( "TS", y, "rdata", sep=".")) 
        fn.ps = file.path( gdir, paste( "PS", y, "rdata", sep="."))
  
        PS = habitat.db ( DS="complete", year=y, p=p )
        PS = PS[ filter.region.polygon( PS, region=p$studyarea, planar=T, proj.type=p$internal.projection ) , ]
        PS$month = p$prediction.month
        # PS$sa = 1
        # PS = PS[ sample( nrow(PS), size=5000 ) ,]
        Qp = predict.discretised.habitat( DS="model.pa", p=p )
        debug = F
        if (debug) {
          pred = predict( Qp, newdata=PS, type="response" )  #
          x11()
          levelplot( pred ~ plon + plat, data=PS, aspect="iso" )
        }
        Hsim = gam.simulation( M=Qp, X=PS, nsims=p$nsims ) #~8min
        PS$habitat.mean = apply( Hsim, 1, mean, na.rm=T )
        PS$habitat.sd = apply( Hsim, 1, sd, na.rm=T )
        rm( Qp); gc()
        
        iHabitat = which( 
          PS$habitat.mean > p$habitat.threshold.quantile  & 
          PS$habitat.mean - 2 * PS$habitat.sd > 0 
        )  

        Qa = predict.discretised.habitat( DS="model.ra", p=p )
        Asim = gam.simulation( M=Qa, X=PS, nsims=p$nsims ) #~8min
        
        PS$abundance.mean = apply( Asim, 1, mean, na.rm=T ) 
        PS$abundance.sd =  apply( Asim, 1, sd, na.rm=T )
        rm( Qa); gc()
        
        save( PS, file=fn.ps, compress=T)
 
       # Do not extrapolate beyond data: trim to XX% quantiles to be a little more conservative
        Asim[ which( Asim >  qs[2] ) ] = qs[2]
        Asim[ which( Asim <  qs[1] ) ] = qs[1]   

        Asim = Asim * Hsim  # Asim now becomes weighted by Pr of habitat
	
        Hsim.sa = colSums( Hsim ) # Pr weighted sum of habitat 
        totalsurfacearea = mean ( Hsim.sa ) * (p$pres*p$pres)
        totalsurfacearea.sd = sd( Hsim.sa ) * (p$pres*p$pres)

        iHabitat = which( PS$habitat.mean > p$habitat.threshold.quantile  & (PS$habitat.mean - 2 * PS$habitat.sd) > 0 )
						
        if ( length( iHabitat ) > 10 ) {
        
          hhsum = colSums( Hsim[ iHabitat, ] )
          sa.estim = mean( hhsum ) * (p$pres*p$pres)
          sa.estim.sd = sd( hhsum ) * (p$pres*p$pres)
          
          aa.sums = apply( Asim[ iHabitat,  ] , 2, sum ) # abundance weighted by Pr
          V.mean = mean( aa.sums )
          V.sd = sd( aa.sums )
          ci = quantile( aa.sums, probs=c(0.005, 0.5, 0.995), na.rm=T,names=F )
        
        } else {

          sa.estim = NA
          sa.estim.sd = NA
          
          V.mean = NA
          V.sd = NA
          ci = rep( NA, 3 )
        }

        bb.sums = apply( Asim[ iHabitat,  ] , 2, sum ) # abundance weighted by Pr
        W.mean = mean( bb.sums )
        W.sd = sd( bb.sums )
        W.ci = quantile( bb.sums, probs=c(0.005, 0.5, 0.995), na.rm=T,names=F )
 
        L = data.frame( yr=y, total=V.mean, total.sd=V.sd, median=ci[2], lbound=ci[1], ubound=ci[3],
              ss=W.mean, ss.sd=W.sd, ss.median=W.ci[2], ss.lbound=W.ci[1], ss.ubound=W.ci[3],
              sa.total=totalsurfacearea, sa.total.sd=totalsurfacearea.sd,
              sa.estimation=sa.estim, sa.estimation.sd=sa.estim.sd,
              datestamp=as.character( Sys.time() ) )
          
        
        save( L , file=fn.ts, compress=T)
        print ( L )
        print( "... Completed successfully" )  


      }

    }

    if (DS =="map.habitat" ) {

      map.loc = file.path( project.datadirectory("habitatsuitability"), p$speciesofinterest, "maps" )
      dir.create (map.loc,  recursive=T, showWarnings=F )
      planar.corners = data.frame(rbind( cbind( plon=p$corners$plon, plat=p$corners$plat ))) 
      
      for ( iip in 1:p$nruns ) {
        y = p$runs[iip,"y"]
        PS = predict.discretised.habitat ( DS="sim", yr=y, p=p )
 
        # map habitat
        datarange = seq( 0, 1, length.out=150)
          cols = color.code( "seis", datarange )
          outfn = paste( "prediction.habitat.mean", y, sep=".")
          map( xyz=PS[,c("plon", "plat", "habitat.mean")], cfa.regions=F, depthcontours=T, pts=NULL, annot=paste( y ), 
            annot.cex=p$annot.cex, corners=planar.corners, fn=outfn, loc=map.loc, at=datarange, 
            col.regions=cols, rez=c(p$pres,p$pres) )
        
          
        # map abundance
          # empirical ranges
          if ( p$speciesofinterest == "wolffish" )  { 
            ER = c( -1.784, 2.584 ) 
          } else if ( p$speciesofinterest == "white.hake" )  { 
            ER = c( -1.784, 2.584 ) 
          } else if ( p$speciesofinterest == "thornyskate" ) { 
            ER = c( -1.784, 2.584 ) 
          } else if ( p$speciesofinterest == "american.plaice" ) { 
            ER = c( -1.784, 2.584 ) 
          } else if ( p$speciesofinterest == "cod" )  { 
            ER = c( -1.784, 2.584 ) 
          } else if ( p$speciesofinterest == "redfish" )  { 
            ER = c( -1.784, 2.584 ) 
          } else { 
            ER = quantile( PS$abundance.mean, probs=c(0.005, 0.995 ), na.rm=T) 
          }

          datarange = seq( ER[1], ER[2], length.out=150)
          cols = color.code( "seis", datarange )
          outfn = paste( "prediction.abundance.mean", y, sep=".")
          map( xyz=PS[ , c("plon", "plat", "abundance.mean")], cfa.regions=F, depthcontours=T, pts=NULL, annot= paste( y ), 
            annot.cex=p$annot.cex, corners=planar.corners, fn=outfn, loc=map.loc, at=datarange, 
            col.regions=cols, rez=c(p$pres,p$pres) )
       

          # map the abundance in good habitat -- "estimation area"
          PS$estim = PS$habitat.mean * PS$abundance.mean
           # empirical ranges
          if ( p$speciesofinterest == "wolffish" )  { 
            ER = c( -0.05, 0.5 ) 
          } else if ( p$speciesofinterest == "white.hake" )  { 
            ER = c( -0.05, 0.5 ) 
          } else if ( p$speciesofinterest == "thornyskate" ) { 
            ER = c( -0.05, 0.5 ) 
          } else if ( p$speciesofinterest == "american.plaice" ) { 
            ER = c( -0.05, 0.5 ) 
          } else if ( p$speciesofinterest == "cod" )  { 
            ER = c( -0.05, 0.5 ) 
          } else if ( p$speciesofinterest == "redfish" )  { 
            ER = c( -0.05, 0.5 ) 
          } else { 
            ER = quantile( PS$estim, probs=c(0.005, 0.995 ), na.rm=T)
          }

          datarange = seq( ER[1], ER[2], length.out=150)
          cols = color.code( "seis", datarange )
          outfn = paste( "estimation.abundance.mean", y, sep=".")
          map( xyz=PS[ , c("plon", "plat", "estim")], cfa.regions=F, depthcontours=T, pts=NULL, annot= paste( y ), 
            annot.cex=p$annot.cex, corners=planar.corners, fn=outfn, loc=map.loc, at=datarange, 
            col.regions=cols, rez=c(p$pres,p$pres) )
     
				gc()
      }
    }


  }

  
