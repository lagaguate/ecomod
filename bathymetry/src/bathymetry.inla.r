
 
## ----- Adaptive estimation method (test) :
# processing bathymetry data with RINLA  

  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "lattice", "parallel", "INLA", "geosphere", "sp", "raster",  "bigmemory" )
  
 
  p$project.name = "bathymetry"
  p$project.root = project.datadirectory( p$project.name )

  p = spatial.parameters( type="canada.east.highres", p=p ) ## highres = 1/4 km discretization
  
  p$n.min = 100
  p$dist.max = 20 # length scale (km) of local analysis .. for acceptance into the local analysis/model
  p$dist.mwin = p$dist.max/4 # size of the moving window (km) that aggregates the ** statistics **
  p$dist.pred =  p$dist.max-5 # size of the moving window (km) where **predictions** are retained
 
  p$inla.mesh.offset = c(2, 5) # km
  p$inla.mesh.max.edge = c(1, 5) # km
  p$inla.mesh.cutoff = c(1, 5 ) # km 

  p$inla.alpha = 2 # bessel function curviness
  p$inla.nsamples = 500 # posterior similation

  p$predict.in.one.go = FALSE # use false, one go is very slow and a resource expensive method
  p$predict.type = "response"  # same scale as observations 
  # p$predict.type = "latent.spatial.field" # random field centered to zero
    
  # initialize bigmemory data objects
  p = bathymetry.db( p=p, DS="bigmemory.inla", reload.rawdata=FALSE )

  # do not use all CPU's as INLA itself is partially run in parallel
  # RAM reqiurements are a function of data density and mesh density ..
  p$clusters = c( rep( "hyperion", 3 ), rep( "nyx", 3 ), rep ("tartarus", 3 ), rep("kaos", 3 ) )  

  # p$clusters = "localhost"  # if serial run, send a single cluster host
  p = make.list( list( jj=sample.int( p$nS ) ), Y=p ) # random order helps use all cpus 

  p = parallel.run( bathymetry.interpolate.inla, p=p )  
 
  P = load( p$outfilename.P )
  S = load( p$outfilename.S )
  # ------------------------------
  # extract stats etc.
  S = attach.big.matrix(p$descriptorfile.S, path=p$tmp.datadir)  # statistical outputs
  P = attach.big.matrix(p$descriptorfile.P, path=p$tmp.datadir)  # predictions
  
  # tidy up cases where there are no data:
  means = P[,2] 
  nd = which( P[,1]==0 )
  if (length(nd)>0) means[nd] = NA # no data .. no mean
  
  variance = P[,3] 
  nd = which( P[,1] <= 1 )
  if (length(nd)>0) variance[nd] = NA

  ### need to define domain
  # inside = inout( pG$lattice$loc, full.domain$loc ) 
  # means[ !inside ] = NA
  # variance[ !inside ] = NA
   
  plotdata=FALSE
      if (plotdata) { 
        means.grid = matrix( data=means, nrow=length(p$plons), ncol=length(p$plats),byrow=TRUE )
        lv = levelplot( ( means.grid)~plons+plats, p , xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )
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

  todelete = file.path( p$tmp.datadir,
    c( p$backingfile.P, p$descriptorfile.P, 
       p$backingfile.S, p$descriptorfile.S, 
       p$backingfile.W, p$descriptorfile.W 
  ) 
  
  # for (fn in todelete ) file.remove(fn) 

 
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



