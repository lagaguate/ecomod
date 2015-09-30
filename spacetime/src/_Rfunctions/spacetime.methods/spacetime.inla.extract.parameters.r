
spacetime.inla.extract.parameters = function(  RES, SPDE, vname, extract = c("mean", "sd", "mode", "0.5quant", "0.025quant", "0.975quant") ) {
  #// extract summary statistics from a spatial (SPDE) analysis 
  #//   RES is the results from an inla call 
  #//   SPDE is the SPDE object describing the spatial covariance
  #//   vname is the name used in the model formula to describe the spatial random effect
  #//   extract contains the names of the fields to retain
  
  require(INLA)

  # random field parameters on user scale
  oo = inla.spde2.result(RES, vname, SPDE, do.transf=TRUE)
  
  iRange = exp( oo$summary.log.range.nominal[ extract] ) # or iRange=sqrt(8)/exp(oo$summary.log.kappa$mean) 
  iVar = exp( oo$summary.log.variance.nominal[extract] ) # spatial variance (~ psill)
  iKappa = exp( oo$summary.log.kappa[extract]  )
  iTau = exp(oo$summary.log.tau[extract ] )

  iNuggetmarginals = inla.tmarginal( function(x) {1/x}, RES$marginals.hyperpar[["Precision for the Gaussian observations"]] )
  iNugmode = inla.mmarginal( iNuggetmarginals ) 
  iNugmean = inla.emarginal( function(x) {x}, iNuggetmarginals )
  iNugSD =  sqrt( inla.emarginal( function(x) {x^2}, iNuggetmarginals ) - iNugmean^2)
  iNugQuants = inla.qmarginal( c(0.5, 0.025, 0.975), iNuggetmarginals )

  iNugget = data.frame(cbind( iNugmean, iNugSD, iNugmode, iNugQuants[1], iNugQuants[2], iNugQuants[3] ) )
  names(iNugget) = extract

  inla.summary = as.matrix( rbind( iKappa, iTau, iRange, iVar, iNugget ) )
  rownames( inla.summary) = c( "kappa", "tau", "range", "spatial error", "observation error" )
  colnames( inla.summary) = c( "mean", "sd", "mode", "median", "q025", "q975" )

  return (inla.summary)
}

