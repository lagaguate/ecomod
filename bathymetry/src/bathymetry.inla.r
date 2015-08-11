
 
## ----- Adaptive estimation method (test) :
# processing bathymetry data with RINLA  

  p=list()
  p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "rgdal", "lattice", "parallel", "INLA", "geosphere", "sp", "raster",  "bigmemory", "colorspace" )
  
 
  p$project.name = "bathymetry"
  p$project.root = project.datadirectory( p$project.name )

  rndseed=101 
  # set.seed(rndseed )

  p = spatial.parameters( type="canada.east.highres", p=p ) ## highres = 0.5 km discretization
  
  p$dist.max = 25 # length scale (km) of local analysis .. for acceptance into the local analysis/model
  p$dist.mwin = 5 # size of the moving window (km) that aggregates the ** statistics **
  p$dist.pred =  15 # size of the moving window (km) where **predictions** are retained
 
  ## this changes with resolution: at p$pres=0.25 and a p$dist.max=25: the max count expected is 40000
  n.filled = ( (p$dist.max*2+1) / p$pres )^2 
  p$n.min = 100
  p$n.max = 10000 # numerical time/memory constraint

  p$inla.mesh.offset   = p$pres * c( 8, 16 ) # km
  p$inla.mesh.max.edge = p$pres * c( 8, 16 ) # km
  p$inla.mesh.cutoff   = p$pres * c( 3, 16 ) # km 

  p$inla.alpha = 2 # bessel function curviness
  p$inla.nsamples = 2000 # posterior similations 
  p$expected.range = 30 # km or higher .. (with dependent var on log scale)
  p$expected.sigma = 0.1  # spatial standard deviation (partial sill) .. on log scale

  p$Yoffset = 1000 ## data range is from -383 to 5467 m .. shift all to positive valued as this will operate on the logs

  p$predict.in.one.go = FALSE # use false, one go is very slow and a resource expensive method
  p$predict.type = "response"  # same scale as observations 
  # p$predict.type = "latent.spatial.field" # random field centered to zero
    
  # initialize bigmemory data objects
  p$reload.rawdata=FALSE 
  p$reset.outputfiles=FALSE
  #p$reset.outputfiles=TRUE
  p = bathymetry.db( p=p, DS="bigmemory.inla" )

  # do not use all CPU's as INLA itself is partially run in parallel
  # RAM reqiurements are a function of data density and mesh density ..
  # p$clusters = c( rep( "hyperion", 1 ), rep( "nyx", 1 ), rep ("tartarus", 1), rep("kaos", 1 ) )  
  p$clusters = c( "hyperion", "nyx", "tartarus", "kaos" )  

  # p$clusters = "localhost"  # if serial run, send a single cluster host
  
  S = attach.big.matrix(p$descriptorfile.S, path=p$tmp.datadir)  # statistical outputs
  todo = which( !is.finite( S[,3] ))
  p = make.list( list( jj=sample( todo ) ), Y=p ) # random order helps use all cpus 

  p = parallel.run( bathymetry.interpolate.inla, p=p )  
 
  P = load( p$outfilename.P )etc.
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
        coordsp = expand.grid( plons=p$plons, plats=p$plats )
        lv = levelplot( log(  means ) ~ plons+plats, coordsp , xlab='', ylab='', col.regions=rev(sequential_hcl(100)), scale=list(draw=FALSE), aspect="iso" )
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
      idat <- inla.stack.index( DATA, 'obs')$data
      cor( indat$z, RES$summary.linear.predictor$mean[idat]) ## 0.9991

      levelplot( ( means.grid) , xlab='', ylab='', col.regions=rev(sequential_hcl(50)), scale=list(draw=FALSE) , aspect="iso" )
      levelplot( ( vars.grid) , xlab='', ylab='', col.regions=rev(sequential_hcl(50)), scale=list(draw=FALSE) , aspect="iso" )

  }



