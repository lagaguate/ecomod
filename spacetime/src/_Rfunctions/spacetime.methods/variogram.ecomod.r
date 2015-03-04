
variogram.ecomod = function( xyz, crs="+proj=utm +zone=20 +ellps=WGS84", plot=FALSE, edge=c(1/5, 1), return.inla=FALSE ) {
  
  # estimate empirical variograms and then model them using a number of different approaches
  # returns empirical variogram and parameter estimates, and optionally the models themselves
  # expect xyz = c(lon, lat, variable)

  require(sp)
  require(gstat)
  require(INLA)
  require(lattice)
  
  out = NULL

 if ( "test" %in% xyz ) {
    # just for debugging / testing ...
    data(meuse)
    xyz = meuse[, c("x", "y", "elev")]
    crs="+proj=utm +zone=20 +ellps=WGS84"
  }

   
  if ( !grepl( "planar", crs )) { 
    # i.e. if not already planar coords, then  assume it is in lon-lat .. requires some planar coord system
    nm = names(xyz) 
    xyz = try( lonlat2planar( xyz, proj.type=crs ), silent=TRUE )
    xyz = xyz[, c("plon", "plat", nm[3])]
  } 
  
  names(xyz) =  c("plon", "plat", "z" )
  
  drange = sqrt(diff(range(xyz$plon))^2 + diff(range(xyz$plat))^2)
  
  xrange = range( xyz$plon, na.rm=TRUE )
  yrange = range( xyz$plat, na.rm=TRUE )
  zrange = range( xyz$z, na.rm=TRUE )
  
  nxout = 100
  nyout = 100
  nzout = 100

  xx = seq( xrange[1], xrange[2], length.out=nxout )
  yy = seq( yrange[1], yrange[2], length.out=nyout )
  zz = seq( zrange[1], zrange[2], length.out=nzout )
  preds = expand.grid( plon=xx, plat=yy )

  # first pass -- use gstat to obtain faster estimates of variogram parameters to speed up inla
  vEm = variogram( z~1, locations=~plon+plat, data=xyz ) # empirical variogram
  vMod0 = vgm(psill=0.5, model="Mat", range=drange/10, nugget=0.5, kappa=2 ) # starting model parameters
  vFitgs =  fit.variogram( vEm, vMod0 ) ## gstat's kappa is the Bessel function's "nu" smoothness parameter
    
  vRange = vFitgs$range[2] # 95% of total variance 
  vTot   = vFitgs$psill[1] + vFitgs$psill[2] 
  vPsill = vFitgs$psill[2]  
  vNugget = vFitgs$psill[1]   


  if (plot) {
    x11()
    plot( vEm, model=vFitgs, main="gstat Variogram" )
    g <- gstat(id = "elev", formula = z~1, locations = ~plon+plat, data = xyz )
    g = gstat(g, id="elev", model=vFitgs) 
    gpredres <- predict( g, preds )
    x11()
    lp = levelplot( elev.pred ~ plon+plat, gpredres, aspect = "iso", at=zz, col.regions=color.code( "seis", zz),
      contour=FALSE, labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE)  )
    plot(lp)
  }

# now inla
  inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors

  locs0  = as.matrix( xyz[,1:2] )
  z = xyz[,3]  # must live outside xyz for inla
  xyz$z = NULL
  xyz$b0 = 1  # intercept for inla
  
  M0.domain = inla.nonconvex.hull( locs0 )
  M0 = inla.mesh.2d (
    loc=locs0, # locations of data points
    boundary = M0.domain,
    max.edge = edge * vRange
  )
 
  kappa0 = sqrt(8) / vRange
  tau0 = 1/ ( sqrt(4*pi) * kappa0 * vPsill )

  S0 = inla.spde2.matern( M0, alpha=2, 
    B.tau = cbind(log(tau0), -1,1 ),     # parameter basis functions
    B.kappa = cbind( log(kappa0), 0, 1 ), # parameter basis functions
    theta.prior.mean = c(0,  0),    # theta1 controls variance .. vague; theta2 controls range   .. means 0
    theta.prior.prec = c(0.1, 1)  #  precisions are vague for theta1;  for range .. theta2 prec 1 ==> 95% prior prob that range is smaller than domain size
  ) 

  i <- inla.spde.make.index('i', n.spde=S0$n.spde )  

  # projection matrix A to translate from mesh nodes to data nodes
  A = inla.spde.make.A( mesh=M0, loc=locs0 )

  # data stack for occurence (PA)
  Z = inla.stack( 
      tag="data",
      data=list( z=z ) ,
      A=list(A, 1 ),
      effects=list( i=i, xyz )  # b0 is the intercept
  )

  R <- inla(  z ~ 0 + b0+ f( i, model=S0, diagonal=1e-2), 
      data=inla.stack.data(Z), 
      control.compute=list(dic=TRUE),
      control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
      control.predictor=list(A=inla.stack.A(Z), compute=TRUE) , 
      control.inla=list(strategy="laplace", npoints=21, stencil=7 ) ,
      verbose = FALSE
  )

  # field parameters on user scale
  oo = inla.spde2.result(R, 'i', S0, do.transf=TRUE)
  
  extract =  c("mean", "sd", "mode", "0.5quant", "0.025quant", "0.975quant")
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

  # indices for random field at data locations
  idat <- inla.stack.index( Z, 'data')$data
  # correlation between the the posterior mean and the response by
  cor.predict = cor( z, R$summary.linear.predictor$mean[idat])

  if (plot) {
    x11(); 
    plot(M0, asp=1 ) # visualise mesh
    
    x11()
    vn = "b0"
    v=R$marginals.fixed[[vn]]
    v =  v[order(v[,1]),]
    plot( v, type="l", xlab="b0 -- intercept", ylab="Density" )
    
    x11()
    vn = "Precision for the Gaussian observations"  ## i.e, the "nugget" or observation error
    v = R$marginals.hyperpar[[vn]]
    # v = v[order(v[,1]),]
    plot.default( iNuggetmarginals, xlab="Non-spatial observation error ('nugget variance')", type="l", ylab="Density" )
    abline( v=iNugget$mean, lty="dotted" )

    x11(); 
    vn = "marginals.variance.nominal"  # spatially stuctured variance .. ~ psill
    plot( oo[[vn]][[1]], type='l', xlab="Spatial error ('partial sill variance')", ylab='Density')
    abline(v=iVar$mean, lty="dotted" )
    
    x11(); 
    vn = "marginals.kappa"
    plot(oo[[vn]][[1]], type='l', xlab=expression(kappa), ylab='Density')
    abline(v=iKappa$mean, lty="dotted"  )
    
    x11(); 
    vn = "marginals.range.nominal"
    plot(oo[[vn]][[1]], type='l', xlab='range nominal', ylab='Density')
    abline(v=iRange$mean, lty="dotted"  ) 
    
    x11()
    require(lattice)
    loadfunctions( "utility" )

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
    
  inla.summary = rbind( iKappa, iTau, iRange, iVar, iNugget )
  rownames( inla.summary) = c( "kappa", "tau", "range", "spatial error", "observation error" )
  colnames( inla.summary) = c( "mean", "sd", "mode", "median", "q025", "q975" )
                              
  out = list( vario.empirical=vEm, vario.model=vFitgs, vario.range=vRange, vario.psill=vPsill, vario.nugget=vNugget,
              kappa0=kappa0, tau0=tau0, inla.summary=inla.summary, correl=cor.predict         
  ) 

  if (return.inla) {
    out$inla = R
    out$mesh = M0 
  }
  return(out)
}


