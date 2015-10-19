
  spacetime.plot = function( p, obj, RES=NULL, MESH=NULL, SPDE=NULL, vname=NULL, idat=NULL, nxout=100, nyout=100 ) {
    #\\ Simple diagnostic plots for spacetime statistics and predictions
    #\\   spacetime.plot( p=p, odb="mesh" ) :: mesh, range, nugget, partial.sill, kappa, intercept, etc
    #\\   RES is the data result from an inla call
    #\\   MESH is the mesh data object
    #\\   vname is the name of the random spatial field in the model formula 
    #\\   idat are indices for random field at data locations ( idat <- inla.stack.index( DATA, 'data')$data )
    #\\   nxout, nyout are the number of cells in x and y direct for interpolated output

    gg =  grep("bigmemory", obj)
    if ( length(gg)>0 ) {
      pp = grep("predictions", obj )
      if ( length(pp) > 0 ) {
        p = spacetime.db( p=p, DS="bigmemory.inla.filenames" )
 
        pps  =  expand.grid( plons=p$plons, plats=p$plats)
        P = attach.big.matrix( p$descriptorfile.P , path=p$tmp.datadir ) 
        
        cl = 2 # default is mean value 
        if ( grep("n", obj) ) cl=1
        if ( grep("mean", obj) ) cl=2
        if ( grep("sd", obj) ) cl=3
        require(lattice)
        levelplot( log( P[,2] ) ~plons+plats, pps , 
            col.regions=rev(sequential_hcl(100)), scale=list(draw=FALSE) , aspect="iso" )
      }
    }

    if ("mesh" %in% obj) {
      x11(); 
      plot(MESH, asp=1 ) # visualise mesh
    }
     

    if ("range" %in% obj) {
      x11(); 
      oo = inla.spde2.result(RES, vname, SPDE, do.transf=TRUE)
      im = oo$marginals.range.nominal[[1]]
      # or iRange=sqrt(8)/exp(oo$summary.log.kappa$mean)  = exp( oo$summary.log.range.nominal[ "mean" ] )  
      iRange = c( mode=inla.mmarginal( im ), inla.zmarginal( im, silent=TRUE ), as.data.frame(inla.hpdmarginal( 0.95, im )) )
      plot( im, type='l', xlab='range nominal', ylab='Density')
      abline(v=iRange$mode, lty="dotted"  ) 
    }


    if ("nugget" %in% obj) {
      x11()
      ## Non-spatial ("observation") error ('nugget variance')
      iprec = grep ( "Precision.*observ.*", names(RES$marginals.hyperpar), ignore.case=TRUE )
      im = inla.tmarginal( function(x) {1/x}, RES$marginals.hyperpar[[ iprec ]] )
      iNugget =  c( mode=inla.mmarginal( im ), inla.zmarginal( im, silent=TRUE ), as.data.frame(inla.hpdmarginal( 0.95, im ) ) )
      plot.default( im, xlab="Non-spatial observation error ('nugget variance')", type="l", ylab="Density" )
      abline( v=iNugget$mode, lty="dotted" )
    }


    if ("partial.sill" %in% obj) {
      x11(); 
      # "Spatial variance/error ('partial sill variance')"
      oo = inla.spde2.result(RES, vname, SPDE, do.transf=TRUE)
      im = oo$marginals.variance.nominal[[1]]
      iVar =  c( mode=inla.mmarginal( im ), inla.zmarginal( im, silent=TRUE ), as.data.frame(inla.hpdmarginal( 0.95, im )) )
      plot( im, type='l', xlab="Spatial error ('partial sill variance')", ylab='Density')
      abline(v=iVar$mode, lty="dotted" )
    }
  
     gg =  grep("fixed", obj)
     if ( length(gg)>0) { 
      x11()
      vn = gsub( "^fixed.", "", obj )
      idx = grep ( vn, names(RES$marginals.fixed), ignore.case=TRUE )
      im = inla.tmarginal( function(x) {1/x}, RES$marginals.fixed[[ idx ]] )
      iFixed =  c( mode=inla.mmarginal( im ), inla.zmarginal( im, silent=TRUE ), as.data.frame(inla.hpdmarginal( 0.95, im ) ) )
      plot.default( im, xlab=obj, type="l", ylab="Density" )
      abline( v=iFixed$mode, lty="dotted" )
    }


    if ("kappa" %in% obj) {
      x11(); 
      oo = inla.spde2.result(RES, vname, SPDE, do.transf=TRUE)
      im = oo$marginals.kappa[[1]]
      iKappa = c( mode=inla.mmarginal( im ), inla.zmarginal( im, silent=TRUE ), as.data.frame(inla.hpdmarginal( 0.95, im ) ) )
      plot( im, type='l', xlab=expression(kappa), ylab='Density')
      abline(v=iKappa$mode, lty="dotted"  )
    }


    if ("tau" %in% obj) {
      x11(); 
      oo = inla.spde2.result(RES, vname, SPDE, do.transf=TRUE)
      im = oo$marginals.tau[[1]]
      iTau = c( mode=inla.mmarginal( im ), inla.zmarginal( im, silent=TRUE ), as.data.frame(inla.hpdmarginal( 0.95, im ) ) )
      plot( im, type='l', xlab=expression(tau), ylab='Density')
      abline(v=iTau$mode, lty="dotted"  )
    }


    if ("map.prediction" %in% obj) {
      x11()
      require(lattice)
      loadfunctions( "utility" )

      xrange = range( MESH$loc[,1], na.rm=TRUE)
      yrange = range( MESH$loc[,2], na.rm=TRUE)

      pG = inla.mesh.projector( MESH, xlim=xrange, ylim=yrange, dims=c(nxout, nyout) )
      posterior.samples = inla.posterior.sample( n=1000, RES)
 
      rnm = rownames(posterior.samples[[1]]$latent )
      posterior = sapply( posterior.samples, p$spacetime.posterior.extract, rnm=rnm )
      posterior = p$spacetime.invlink( posterior )   # return to original scale
   
      out_mean = inla.mesh.project( pG, field=apply( posterior, 1, mean, na.rm=TRUE )  )  # mean
      out_sd   = inla.mesh.project( pG, field=apply( posterior, 1, sd  , na.rm=TRUE )  )  # mean

      datarange = range( out_mean, na.rm=TRUE )
      dr = seq( datarange[1], datarange[2], length.out=150)
      lp = levelplot( out_mean ~ x+y, pG, aspect="iso", main="Posterior mean", at=dr, col.regions=color.code( "seis", dr) ,
        contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      print(lp)


      datarange = range( out_sd, na.rm=TRUE )
      dr = seq( datarange[1], datarange[2], length.out=150)
      lp = levelplot( out_sd ~ x+y, pG, aspect="iso", main="Posterior SD", at=dr, col.regions=color.code( "seis", dr) ,
        contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      print(lp)

    }

  }

