
interpolate.local.2d.depth = function( p ) {

  # set up (temporary) data file locations
  p$tmp.datadir = project.datadirectory( p$project.name, "tmp" )
  dir.create( p$tmp.datadir, recursive=TRUE, showWarnings=FALSE )
  
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
  interpolate.depth.local.inla.core = function( ip=NULL, p, AW, plotdata=FALSE ) {

    # ip is the first parameter passed in the parallel mode
    if (exists( "init.files", p)) LoadFiles( p$init.files ) 
    if (exists( "libs", p)) RLibrary( p$libs ) 
    if (is.null(ip)) {
      if( exists( "nruns", p ) ) {
        ip = 1:p$nruns
      } else { 
        ip = 1:nrow(AW)
      }
    }
  
    # data file definitions
    W = attach.big.matrix(p$descriptorfile.W)  # input data
    P = attach.big.matrix(p$descriptorfile.P)  # predictions
    S = attach.big.matrix(p$descriptorfile.S)  # statistical outputs

    for ( dd in ip ) {
      focal = AW[dd,] 
      dists = sqrt( (focal$plon - W$plon )**2 + (focal$plat - W$plat )**2 ) 
      j = which( dists <= p$dist.max )
      if (length(j) < 30 ) next()
      
      varstokeep = c("plon", "plat", "z" )  ## and covariates here
      indat = NULL
      indat = W[ j, varstokeep]
      indat$b0 = 1  # intercepts used later
      indat$ii = 1  # id for iid process
       
      # compute stats
      locs  = as.matrix( indat[,c("plon", "plat")] )
      domain = inla.nonconvex.hull( locs, convex=p$dist.max/4 )
      MESH = inla.mesh.2d (
        loc=locs , # locations of data points
        boundary = domain, 
        offset = p$pres* c(2, 4 ),  # how much to extend inside and outside of boundary
        max.edge= p$pres* c(1, 5 ),  # max size of a triange (in, out)
        # min.angle = c(20),   # min angle (in, out)
        cutoff= p$pres * c(0.25, 10)  # min distance allowed  /.... use 8 or less for production 
      )
      
      if (plotdata) { plot(MESH, asp=1 ) ; lines(domain$loc, col="red", lwd=2) }

      S0 = inla.spde2.matern( MESH, alpha=p$inla.alpha ) # alpha=2 is exponential correlation function
      i <- inla.spde.make.index('i', n.spde=S0$n.spde )  # indices of SPDE 

      # projection matrix A to translate from mesh nodes to data nodes
      A = inla.spde.make.A( mesh=MESH, loc=locs )

      # data stack 
      DATA = inla.stack( tag="bathymetry", data=list( depth=(indat$z) ), A=list(A,1), effects=list( i=i, indat ))
      rm (dists, j, indat, domain, A ) ; gc()

      RES <- try( inla(
          depth ~ -1 + b0 + f(ii, model="iid")  + f( i, model=S0 ),
          data=inla.stack.data(DATA), 
          control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
          # control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
          control.predictor=list(A=inla.stack.A(DATA), compute=TRUE)
      ), silent=TRUE )

      if ("try-error" %in% class(RES)) next()

      # ----------------
      # predict upon grid
      # prediction (of the latent field) for each time and visualize it 
      
      # .. first create projector from mesh to output
      pG = inla.mesh.projector( MESH, xlim=p$corners$plon, ylim=p$corners$plat, dims=c( p$nplons, p$nplats)  )
      # inside = inout( pG$lattice$loc, domain$loc ) 
      
      xmean <- inla.mesh.project( pG, RES$summary.random$i$mean )
      xmean = as.vector(xmean) 
      ee = which( xmean < zrange[1] | xmean > zrange[2] )
      if (length(ee) > 0 ) xmean[ee ] = NA # do not permit extrapolation
      uu = which(is.finite(xmean))
     
      # merge mean, variance estimates of predictions with those from other locations via the
      # "online" ( incremental ) method of mean and variance estimation (after Knuth ; see wikipedia algorithm for variance )
      # order is important !
      P[uu,1] = P[uu,1]+ 1 # n
      delta = xmean[uu] - P[uu,2] # difference
      P[uu,2] = P[uu,2] + delta/P[uu,1] # update mean 
      P[uu,3] = P[uu,3] + delta*(xmean[uu] - P[uu,2]) # update "M2"
   
      inla.summary = spacetime.inla.extract.parameters( RES, S0, vname="i" )

      # update statistics
      S[dd,1] = inla.summary["range", "mode"]
      S[dd,2] = inla.summary["range", "sd"]
      S[dd,3] = inla.summary["spatial error", "mode"]
      S[dd,4] = inla.summary["observation error", "mode"]

      rm(S0, RES, delta, uu, xmean, ee, pG ); gc()

      if (plotdata) { 
        oo = P
        nd = which( oo[,1]==0 )
        if (length(nd)>0) oo[ nd,2 ] = NA # no data .. no mean
        means.grid = matrix( data=oo[,2], nrow=length(p$plons), ncol=length(p$plats) )
        lv = levelplot( ( means.grid) , xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )
        print(lv)
      }
    }
  } ## end core interpolation function definition
  
  # run above model in parallel (or serial) mode
  p = make.list( list( loc=sample.int( nrow(AW) ) ) ) # random order helps use all cpus 
  p = run.parallel( p$interpolation.function( p=p, AW=AW ) )
   
  S = attach.big.matrix(p$descriptorfile.S)  # statistical outputs
  P = attach.big.matrix(p$descriptorfile.P)  # predictions
  
  # tidy up casese where there are no data:
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
 

