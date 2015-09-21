
  spacetime.plot = function( p, obj, RES=NULL, MESH=NULL, SPDE=NULL, vname=NULL, idat=NULL, nxout=100, nyout=100 ) {
    #\\ Simple diagnostic plots for spacetime statistics and predictions
    #\\   spacetime.plot( p=p, odb="mesh" ) :: mesh, range, nugget, partial.sill, kappa, intercept, etc
    #\\   RES is the data result from an inla call
    #\\   MESH is the mesh data object
    #\\   vname is the name of the random spatial field in the model formula 
    #\\   idat are indices for random field at data locations ( idat <- inla.stack.index( DATA, 'data')$data )
    #\\   nxout, nyout are the number of cells in x and y direct for interpolated output

    oo =  grep("bigmemory", obj)
    if ( length(oo)>0 ) {
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
      vn = "marginals.range.nominal"
      oo = inla.spde2.result(RES, vname, SPDE, do.transf=TRUE)
      iRange = exp( oo$summary.log.range.nominal[ "mean" ] ) # or iRange=sqrt(8)/exp(oo$summary.log.kappa$mean) 
      # random field parameters on user scale
      plot(oo[[vn]][[1]], type='l', xlab='range nominal', ylab='Density')
      abline(v=iRange$mean, lty="dotted"  ) 
    }


    if ("nugget" %in% obj) {
      x11()
      vn = "Precision for the Gaussian observations"  ## i.e, the "nugget" or observation error
      v = RES$marginals.hyperpar[[vn]]
      iNuggetmarginals = inla.tmarginal( function(x) {1/x}, RES$marginals.hyperpar[[vn]] )
      iNugmean = inla.emarginal( function(x) {x}, iNuggetmarginals )
      plot.default( iNuggetmarginals, xlab="Non-spatial observation error ('nugget variance')", type="l", ylab="Density" )
      abline( v=iNugmean, lty="dotted" )
    }

    if ("partial.sill" %in% obj) {
      x11(); 
      vn = "marginals.variance.nominal"  # spatially stuctured variance .. ~ psill
      oo = inla.spde2.result(RES, vname, SPDE, do.transf=TRUE)
      plot( oo[[vn]][[1]], type='l', xlab="Spatial error ('partial sill variance')", ylab='Density')
      iVar = exp( oo$summary.log.variance.nominal["mean"] ) # spatial variance (~ psill)
      abline(v=iVar$mean, lty="dotted" )
    }
   
    if ("intercept" %in% obj) {
      x11()
      vn = "b0"
      v=RES$marginals.fixed[[vn]]
      v = v[order(v[,1]),]
      plot( v, type="l", xlab="b0 -- intercept", ylab="Density" )
    }


    if ("kappa" %in% obj) {
      x11(); 
      vn = "marginals.kappa"
      oo = inla.spde2.result(RES, vname, SPDE, do.transf=TRUE)
      plot(oo[[vn]][[1]], type='l', xlab=expression(kappa), ylab='Density')
      iKappa = exp( oo$summary.log.kappa["mean"]  )
      abline(v=iKappa$mean, lty="dotted"  )
    }


    if ("map.prediction" %in% obj) {
      x11()
      require(lattice)
      loadfunctions( "utility" )

      # correlation between the the posterior mean and the response by
      # cor.predict = cor( z, RES$summary.linear.predictor$mean[idat])

      delta = mean( RES$summary.linear.predictor$mean[idat]) -  mean(RES$summary.random$i$mean) 
      pG = inla.mesh.projector( MESH, xlim=xrange, ylim=yrange, dims=c(nxout, nyout) )
      out = inla.mesh.project( pG, RES$summary.random$i$mean ) # mean
      outdf = as.data.frame.table( out)
      preds$z = outdf[,3] + delta
      datarange = range( preds$z, na.rm=TRUE )
      dr = seq( datarange[1], datarange[2], length.out=150)
      lp = levelplot( z~plon+plat, preds, aspect="iso", main="Posterior mean", at=dr, col.regions=color.code( "seis", dr) ,
        contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      print(lp)

      pG = inla.mesh.projector( MESH, xlim=xrange, ylim=yrange, dims=c(nxout, nyout) )
      out = inla.mesh.project( pG, RES$summary.random$i$sd ) # SD
      outdf = as.data.frame.table( out)
      preds$z = outdf[,3] 
      datarange = range( preds$z, na.rm=TRUE )
      dr = seq( datarange[1], datarange[2], length.out=150)
      lp = levelplot( z~plon+plat, preds, aspect="iso", main="Posterior SD", at=dr, col.regions=color.code( "seis", dr) ,
        contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      print(lp)

    }

  }

