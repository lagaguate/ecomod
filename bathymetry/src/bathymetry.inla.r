
 
## ----- Adaptive estimation method :
# require(geosphere)
# processing bathymetry data with RINLA

  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "lattice", "parallel", "INLA", "geosphere", "sp", "raster" )
	p = spatial.parameters( type="canada.east", p=p )
  
  p$dist.max = 25 # length scale of local analysis

  # choose 
  debug.method = TRUE
  if ( ! debug.method ) {
    W = bathymetry.db ( p, DS="z.lonlat.rawdata" ) # larger
    W = lonlat2planar( W, proj.type=p$internal.projection )
  } else {
    W = bathymetry.db ( p, DS="baseline" )  # smaller -- very deep removed and planar coords
  }


  W = W[ which(is.finite( W$z ) & is.finite( W$plon) & is.finite(W$plat) ), c("plon", "plat", "z") ]
  dups = which( duplicated( W) )
  length(dups)
  if (length(dups) > 0 ) W = W[-dups, ]
  gc()
  ndata = nrow( W )

  # first pass -- use gstat to obtain faster and approximate global 
  # estimates of variogram parameters to speed up inla
  #  vEm = variogram( z ~ 1, locations=~plon+plat, data=W, cutoff=250 ) # empirical variogram .. suggest 100 to 125 as first plateau

  nr = length(p$plons) * length(p$plats)
  nc = 3  # keep count, mean, sd(via m2) 

  res = data.frame( id=1:ndata) 
  res$range = NA
  res$range.sd = NA
  res$spatial.error = NA
  res$observation.error = NA

  for ( dd in 1:ndata ) {
    print(dd)
    
    # select data
    focal = W[dd,] 
    dists = sqrt( (focal$plon - W$plon )**2 + (focal$plat - W$plat )**2 ) 
    j = which( dists <= p$dist.max )
    if (length(j) == 0 ) next()
    
    varstokeep = c("plon", "plat", "z" )  ## and covariates here
    indat = NULL
    indat = W[ j, varstokeep]
    indat$b0 = 1  # intercepts used later
    indat$ii = 1  # id for iid process
   
    # compute stats
    locs0  = as.matrix( indat[,c("plon", "plat")] )
         
    M0.domain = inla.nonconvex.hull( locs0, convex=p$dist.max/4 )

    M0 = inla.mesh.2d (
      loc=locs0 , # locations of data points
      boundary = M0.domain, 
      offset = p$pres* c(2.5, 5 ),  # how much to extend inside and outside of boundary
      max.edge= p$pres* c(5, 10 ),  # max size of a triange (in, out)
      # min.angle = c(20),   # min angle (in, out)
      cutoff= p$pres * c(1, 10)  # min distance allowed  /.... use 8 or less for production 
    )
    
    plotdata = FALSE
    if (plotdata) {
      plot(M0, asp=1 ) # visualise mesh
      lines(M0.domain$loc, col="red", lwd=2)
    }

    S0 = inla.spde2.matern( M0, alpha=2 ) # alpha=2 is exponential correlation function

    # indices of SPDE 
    i <- inla.spde.make.index('i', n.spde=S0$n.spde )  

    # projection matrix A to translate from mesh nodes to data nodes
    A = inla.spde.make.A( mesh=M0, loc=locs0 )

    # data stack 
    Z = inla.stack( 
        tag="bathymetry",
        data=list( depth=( indat$z ) ) ,
        A=list(A,1),
        effects=list( i=i, indat ) # in case of covariates add to "indat" 
    )

    rm (indat, M0.domain ) ; gc()

    R <- try( inla(
        depth ~ -1 + b0 + f(ii, model="iid")  + f( i, model=S0 ),
        data=inla.stack.data(Z), 
         control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
        control.predictor=list(A=inla.stack.A(Z), compute=TRUE)
    ), silent=TRUE )

    if ("try-error" %in% class(R)) next()

    if (0) {
      (R$summary.hyperpar)
      # random field parameters on user scale
      oo = inla.spde2.result(R, 'i', S0, do.transf=TRUE)
      plot(oo$marginals.variance.nominal[[1]], type='l', xlab=expression(sigma[x]), ylab='Density')
      plot(oo$marginals.kappa[[1]], type='l', xlab=expression(kappa), ylab='Density')
      plot(oo$marginals.range.nominal[[1]], type='l', xlab='range nominal', ylab='Density')

      # correlation between the the posterior mean and the response by
      # indices for random field at data locations
      indat = W[ j, varstokeep]
      idat <- inla.stack.index( Z, 'bathymetry')$data
      cor( indat$z, R$summary.linear.predictor$mean[idat]) ## 0.9991
    }


  # ----------------
  # predict upon grid
  # prediction (of the latent field) for each time and visualize it 
  # .. first create projector from mesh to output

    pG = inla.mesh.projector( M0, xlim=p$corners$plon, ylim=p$corners$plat, 
                             dims=c( length(p$plons), length(p$plats))  )
    # inside = inout( pG$lattice$loc, M0.domain$loc ) 
    
    xmean <- inla.mesh.project( pG, R$summary.random$i$mean)

    xmean = as.vector(xmean) 
    uu = which(is.finite(xmean))
   
    # "online" ( incremental ) method of variance estimation .. 
    # attributed to Knuth (see wikipedia algorithm for algorithm variance )
    # order is important !
    oMean[uu,1] = oMean[uu,1]+ 1 # n
    delta = xmean[uu] - oMean[uu,2] # difference
    oMean[uu,2] = oMean[uu,2] + delta/oMean[uu,1] # update mean 
    oMean[uu,3] = oMean[uu,3] + delta*(xmean[uu] - oMean[uu,2]) # update "M2"
 
    inla.summary = spacetime.inla.extract.parameters( R, S0, vname="i" )

    res$range[dd] = inla.summary["range", "mode"]
    res$range.sd[dd] = inla.summary["range", "sd"]
    res$spatial.error[dd] = inla.summary["spatial error", "mode"]
    res$observation.error[dd] = inla.summary["observation error", "mode"]


  } ## end for loop


  # no data:
  nd = which( oMean[,1]==0 )
  if (length(nd)>0) oMean[ nd,2 ] = NA # no data .. no mean
  means = oMean[,2] 
  
  variance = oMean[,3] / (oMean[,1]-1) # sample variance 
  nd = which( oMean[,1] <= 1 )
  if (length(nd)>0) variance[nd] = NA

### need to define domain
  # inside = inout( pG$lattice$loc, full.domain$loc ) 
  # means[ !inside ] = NA
  # variance[ !inside ] = NA

  means.grid = matrix( data=means, nrow=length(p$plons), ncol=length(p$plats) )
  vars.grid = matrix( data=variance, nrow=length(p$plons), ncol=length(p$plats) )
 
  levelplot( log( means.grid) , xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )
  
  levelplot( log( variance.grid) , xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )



  fn = project.datadirectory( "bathymetry", "interpolated", paste( "bathymetry", "inla", "rdata")  )
  save( oMean[], file=fn, compress=TRUE )







