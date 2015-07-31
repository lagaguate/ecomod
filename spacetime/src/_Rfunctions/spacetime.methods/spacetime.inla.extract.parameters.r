
spacetime.inla.extract.parameters = function( 
  R, S0, vname="i", Z=NULL,  
  extract = c("mean", "sd", "mode", "0.5quant", "0.025quant", "0.975quant"),
  plotdata="none" ) {
  
  # note Z is required if spatial prediction is to be made
  
  require(INLA)

  # field parameters on user scale
  oo = inla.spde2.result(R, vname,  S0, do.transf=TRUE)
  
  iRange = exp( oo$summary.log.range.nominal[ extract] ) # or iRange=sqrt(8)/exp(oo$summary.log.kappa$mean) 
  iVar = exp( oo$summary.log.variance.nominal[extract] ) # spatial variance (~ psill)
  iKappa = exp( oo$summary.log.kappa[extract]  )
  iTau = exp(oo$summary.log.tau[extract ] )

  iNuggetmarginals = inla.tmarginal( function(x) {1/x}, R$marginals.hyperpar[["Precision for the Gaussian observations"]] )
  iNugmode = inla.mmarginal( iNuggetmarginals ) 
  iNugmean = inla.emarginal( function(x) {x}, iNuggetmarginals ) 
  iNugSD =  sqrt( inla.emarginal( function(x) {x^2}, iNuggetmarginals ) - iNugmean^2)
  iNugQuants = inla.qmarginal( c(0.5, 0.025, 0.975), iNuggetmarginals )

  iNugget = data.frame(cbind( iNugmean, iNugSD, iNugmode, iNugQuants[1], iNugQuants[2], iNugQuants[3] ) )
  names(iNugget) = extract

  inla.summary = as.matrix( rbind( iKappa, iTau, iRange, iVar, iNugget ) )
  rownames( inla.summary) = c( "kappa", "tau", "range", "spatial error", "observation error" )
  colnames( inla.summary) = c( "mean", "sd", "mode", "median", "q025", "q975" )

  if ("mesh" %in% plotdata) {
    x11(); 
    plot(M0, asp=1 ) # visualise mesh
  }
   if ("range" %in% plotdata) {
    x11(); 
    vn = "marginals.range.nominal"
    plot(oo[[vn]][[1]], type='l', xlab='range nominal', ylab='Density')
    abline(v=iRange$mean, lty="dotted"  ) 
  }


  if ("nugget" %in% plotdata) {
    x11()
    vn = "Precision for the Gaussian observations"  ## i.e, the "nugget" or observation error
    v = R$marginals.hyperpar[[vn]]
    # v = v[order(v[,1]),]
    plot.default( iNuggetmarginals, xlab="Non-spatial observation error ('nugget variance')", type="l", ylab="Density" )
    abline( v=iNugget$mean, lty="dotted" )
  }

  if ("partial.sill" %in% plotdata) {
    x11(); 
    vn = "marginals.variance.nominal"  # spatially stuctured variance .. ~ psill
    plot( oo[[vn]][[1]], type='l', xlab="Spatial error ('partial sill variance')", ylab='Density')
    abline(v=iVar$mean, lty="dotted" )
  }
 
  if ("intercept" %in% plotdata) {
    x11()
    vn = "b0"
    v=R$marginals.fixed[[vn]]
    v =  v[order(v[,1]),]
    plot( v, type="l", xlab="b0 -- intercept", ylab="Density" )
  }



  if ("kappa" %in% plotdata) {
    x11(); 
    vn = "marginals.kappa"
    plot(oo[[vn]][[1]], type='l', xlab=expression(kappa), ylab='Density')
    abline(v=iKappa$mean, lty="dotted"  )
  }


  if ("map.prediction" %in% plotdata) {
    x11()
    require(lattice)
    loadfunctions( "utility" )

    # indices for random field at data locations
    idat <- inla.stack.index( Z, 'data')$data
    # correlation between the the posterior mean and the response by
    # cor.predict = cor( z, R$summary.linear.predictor$mean[idat])

    delta = mean( R$summary.linear.predictor$mean[idat]) -  mean(R$summary.random$i$mean) 
    pG = inla.mesh.projector( M0, xlim=xrange, ylim=yrange, dims=c(nxout, nyout) )
    out = inla.mesh.project( pG, R$summary.random$i$mean ) # mean
    outdf = as.data.frame.table( out)
    preds$z = outdf[,3] + delta
    datarange = range( preds$z, na.rm=TRUE )
    dr = seq( datarange[1], datarange[2], length.out=150)
    lp = levelplot( z~plon+plat, preds, aspect="iso", main="Posterior mean", at=dr, col.regions=color.code( "seis", dr) ,
      contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
    print(lp)

    pG = inla.mesh.projector( M0, xlim=xrange, ylim=yrange, dims=c(nxout, nyout) )
    out = inla.mesh.project( pG, R$summary.random$i$sd ) # SD
    outdf = as.data.frame.table( out)
    preds$z = outdf[,3] 
    datarange = range( preds$z, na.rm=TRUE )
    dr = seq( datarange[1], datarange[2], length.out=150)
    lp = levelplot( z~plon+plat, preds, aspect="iso", main="Posterior SD", at=dr, col.regions=color.code( "seis", dr) ,
      contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
    print(lp)

  }

  return (inla.summary)
}

