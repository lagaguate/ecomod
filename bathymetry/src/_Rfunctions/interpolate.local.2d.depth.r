
interpolate.local.2d.depth = function( p ) {

  p$backingfile.P = "predictions.bigmatrix.tmp"
  p$descriptorfile.P = "predictions.bigmatrix.desc"

  p$backingfile.S = "statistics.bigmatrix.tmp"
  p$descriptorfile.S = "statstics.bigmatrix.desc"
 
  # predictions storage matrix (discretized) 
  P = filebacked.big.matrix( nrow=p$nplon * p$nplat, ncol=3, type="double", init=0, dimnames=NULL, separated=FALSE, 
    backingpath=p$tmp.datadir, backingfile=p$backingfile.P, descriptorfile=p$descriptorfile.P ) 


  # statistics storage matrix ( aggregation window, AW )
  sbbox = list( plats = seq( p$corners$plat[1], p$corners$plat[2], by=p$dist.mwin ), 
                plons = seq( p$corners$plon[1], p$corners$plon[2], by=p$dist.mwin )
  )
  AW = expand.grid( sbbox$plons, sbbox$plats )
  attr( AW , "out.attrs") = NULL
  names( AW ) = c("plon", "plat")
  statsvars = c("range", "range.sd", "spatial.error", "observation.error") 
  nstats = length( statsvars ) 
  
  S = filebacked.big.matrix( nrow=nrow(AW), ncol=nstats, type="double", init=0, dimnames=NULL, separated=FALSE, 
    backingpath=p$tmp.datadir, backingfile=p$backingfile.S, descriptorfile=p$descriptorfile.S ) 


  # ------------------------------
  # DEFINE interpolation model and data 
  # run above model in parallel (or serial) mode
  p = make.list( list( jj=sample.int( nrow(AW) ) ), Y=p ) # random order helps use all cpus 
  p = parallel.run( interpolate.depth.local.inla.core, p=p, AW=AW )
 
  # p = interpolate.depth.local.inla.core( p=p, AW=AW )
  
  S = attach.big.matrix(p$descriptorfile.S)  # statistical outputs
  P = attach.big.matrix(p$descriptorfile.P)  # predictions
  
  # tidy up cases where there are no data:
  nd = which( P[,1]==0 )
  if (length(nd)>0) P[ nd,2 ] = NA # no data .. no mean
  means = P[,2] 
  
  variance = P[,3] / (P[,1]-1) # sample variance 
  nd = which( P[,1] <= 1 )
  if (length(nd)>0) variance[nd] = NA

  ### need to define domain
  # inside = inout( pG$lattice$loc, full.domain$loc ) 
  # means[ !inside ] = NA
  # variance[ !inside ] = NA
   
  plotdata=FALSE
      if (plotdata) { 
        means.grid = matrix( data=means, nrow=length(p$plons), ncol=length(p$plats) )
        lv = levelplot( ( means.grid) , xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )
        print(lv)
      }

  predictions = list( 
    bbox = list( plons=p$plons, plats=p$plats ),
    m = matrix( data=means, nrow=p$nplons, ncol=p$nplats ) ,
    v = matrix( data=variance, nrow=p$nplons, ncol=p$nplats )
  )
  save( predictions, file=p$outfilename.P, compress=TRUE )
  rm( predictions, P ) ; gc()


  snr = length(sbbox$plons)
  snc = length(sbbox$plats)
  statistics = list(
    bbox = sbbox,
    range = matrix( data=S[,1], nrow=snr, ncol=snc ) ,
    range.sd = matrix( data=S[,2], nrow=snr, ncol=snc ) ,
    var.spatial = matrix( data=S[,3], nrow=snr, ncol=snc ) ,
    var.observation = matrix( data=S[,4], nrow=snr, ncol=snc )
  )  

  save( statistics,  file=p$outfilename.S, compress=TRUE )
  rm( statistics, S ) ; gc()

  return(p)
 
  ### ----------------------------------------
  ### end .. following are for debugging, etc.

  if (0) {
      (RES$summary.hyperpar)
      # random field parameters on user scale
      oo = inla.spde2.result(RES, 'i', S0, do.transf=TRUE)
      plot(oo$marginals.variance.nominal[[1]], type='l', xlab=expression(sigma[x]), ylab='Density')
      plot(oo$marginals.kappa[[1]], type='l', xlab=expression(kappa), ylab='Density')
      plot(oo$marginals.range.nominal[[1]], type='l', xlab='range nominal', ylab='Density')

      # correlation between the the posterior mean and the response by
      # indices for random field at data locations
      indat = W[ j, varstokeep]
      idat <- inla.stack.index( DATA, 'bathymetry')$data
      cor( indat$z, RES$summary.linear.predictor$mean[idat]) ## 0.9991

      levelplot( ( means.grid) , xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )
      levelplot( ( vars.grid) , xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )
  }

}
 

