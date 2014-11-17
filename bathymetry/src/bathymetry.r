# -------------------------------------------------------------------------------------
# Bathymetry data
  
  
  p=list()
  p$init.files = loadfunctions( c( "spatialmethods", "utility", "parallel", "bathymetry" ) )
  p$libs = RLibrary( "chron", "rgdal", "lattice", "parallel" )
 
  p$isobaths = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 250, 300, 350, 400, 450, 500, 600, 800, 1000 )
	

	if ( bathymetry.rawdata.redo ) { 
		# glue all data sources (spherical coords) 
    # ... right now this is about 17 GB in size when expanded .... SLOW .... 
    # and it takes about 52+ GB RAM (due to addition of Greenlaw's DEM )
    # run on servers only unless your machine can handle it
		p = spatial.parameters( type="canada.east", p=p )
		bathymetry.db ( p, DS="z.lonlat.rawdata.redo", additional.data=c("snowcrab", "groundfish") )
	}

	# begin interpolations using GMT 
 
  for ( j in c( "canada.east", "SSE" ) ) {
		p = spatial.parameters( type=j, p=p )
		bathymetry.db ( p, DS="prepare.intermediate.files.for.dZ.ddZ" )  # uses GMT...
		bathymetry.db ( p, DS="Z.redo" )
		bathymetry.db ( p, DS="dZ.redo" )
		bathymetry.db ( p, DS="ddZ.redo" )
    bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
    bathymetry.db ( p, DS="complete.redo" ) # glue all together 
		p = make.list( list( depths=p$isobaths ), Y=p )
    p$clusters = rep( "localhost", 2 )  # too many clusters will overload the system ... data files are large ~(11GB RAM required to block) and can be deleted in the temporary drives 
    parallel.run( isobath.db,  p=p, DS="redo" ) 	
	
    # isobath.db( p=p, depths=depths, DS="redo" ) 
	}


 
  # "snowcrab" subsets do exist but are simple subsets of SSE 
  # so only the lookuptable below is all that is important as far as bathymetry is concerned
  # both share the same initial domains + resolutions
 	p = spatial.parameters( type="snowcrab", p=p )
  bathymetry.db ( p, DS="baseline.redo" ) # additional filtering of areas and or depth to reduce file size
  bathymetry.db( DS="lookuptable.sse.snowcrab.redo" ) 
 



## a few maps:
 
  p = spatial.parameters( type="SSE" )
  x = bathymetry.db ( p, DS="baseline" )
  
	snowcrab.area=F
	if (snowcrab.area) {
		# this is used below
		sc = intersect( 
				which( x$plon< 990 & x$plon > 220  & x$plat< 5270 & x$plat > 4675 ) ,
				filter.region.polygon( x[, c("plon", "plat") ], "cfaall", planar=T) 
		)
		x = x[sc,]
	}
	
	x$z =log( x$z )
  
  dr = quantile( x$z, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "depth"
  annot = "ln ( Depth; m )"
  map( xyz=x[,c("plon", "plat", "z")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry", "maps"), at=datarange , col.regions=cols )
  

  
  x = bathymetry.db ( p, DS="dZ.planar" )
	if (snowcrab.area) x = x[sc,]
  dr = quantile( x$dZ, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "slope"
  annot = "ln ( Slope; m/m )"
  map( xyz=x[ ,c("plon", "plat", "dZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry","maps"), at=datarange , col.regions=cols )

  
 
  x = bathymetry.db ( p, DS="ddZ.planar" )
	if (snowcrab.area) x = x[sc,]
  dr = quantile( x$ddZ, probs=c(0.005, 0.995))
  datarange = seq(dr[1], dr[2], length.out=100)
  cols = color.code( "blue.black", datarange )
  outfn = "curvature"
  annot = "ln ( Curvature; m/m/m )"
  map( xyz=x[,c("plon", "plat", "ddZ")], cfa.regions=F, depthcontours=T, pts=NULL, annot=annot, 
    fn=outfn, loc=project.directory("bathymetry", "maps"), at=datarange , col.regions=cols )



# geostats for area of interest
  p=list()
  p$init.files = loadfunctions( c( "spatialmethods", "utility",  "bathymetry", "polygons" ) )
  p$libs = RLibrary( "chron", "rgdal", "lattice", "INLA"  )
  p = spatial.parameters( p=p, type="SSE" ) #  type="canada.east"  can be completed later (after assessment) when time permits if required
  bt = bathymetry.db ( p, DS="baseline" )
  # scotianshelf = locator() 
  scotianshelf = read.table( polygon.ecomod( "scotia.fundy.with.buffer.dat"  ) ) 
  names( scotianshelf) = c("lon", "lat")
  scotianshelf = lonlat2planar( scotianshelf, proj.type=p$internal.projection )

  plot( bt$plat ~ bt$plon, pch="." )
  lines( scotianshelf[,c("plon","plat")] )

  a = which( point.in.polygon( bt$plon, bt$plat, scotianshelf$plon, scotianshelf$plat ) != 0 )
  bt = bt[a,]


  inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors
  
  # boundary domain
  locs0  = as.matrix( bt[,c("plon", "plat")] )
  M0.domain = inla.nonconvex.hull( locs0, convex=10, resolution=200 )

  cutoff = 10 
  max.edge = c(5, 80)
  offset = c(10, 80)
  
  debug=T
  if (debug) {
    cutoff=15
    max.edge = c(15, 120)
    offset = c(15, 120)
  }


  M0 = inla.mesh.2d (
    loc=locs0, # locations of data points
    boundary=M0.domain, 
    offset=offset,  # how much to extend in the c(inner, outer) domains
    max.edge=max.edge,  # max size of a triange (in, out)
    min.angle=c(22),   # min angle (in, out)
    cutoff=cutoff # min distance allowed  /.... use 8 or less for production 
  )
  
  plot(M0, asp=1 ) # visualise mesh

  # SPDE components
  # matern representation using mesh M
  #   spatial scale parameter kappa(u) 
  #     variance rescaling parameter tau(u)
  #     (kappa^2(u)-Delta)^(alpha/2) (tau(u) x(u)) = W(u)         
  # 
  # hyperparamters for matern2d:
  #   theta1[log prec], prior=loggamma, param=c(1, 5e-5), initial=4 
  #   theta2[log range], prior=loggamma, param=c(1, 0.01), initial=2 
  #   hyper.spde2 = list( theta1=list(param=c(1,0.001), theta2=list(param=c(1,0.001))) )
  # BUT... hyper expects only one as follows! why?
  hyper.spde2 = list( theta=list(param=c(1,0.001))) 
                     
  S0 = inla.spde2.matern( M0, alpha=2 ) # alpha=2 is exponential correlation function

  # indices of SPDE 
  i <- inla.spde.make.index('i', n.spde=S0$n.spde )  

  # projection matrix A to translate from mesh nodes to data nodes
  A = inla.spde.make.A( mesh=M0, loc=locs0 )

  depth=bt$z
  bt$b0 = 1  # intercepts
  varstokeep = c("plon", "plat", "b0" )
  bt = bt[,varstokeep]

  gc()

  # data stack 
  Z = inla.stack( 
      tag="bathymetry",
      data=list( depth=depth ) ,
      A=list(A,1),
      effects=list( i=i, bt ) # in case of caovariates add to bt 
  )
  gc()


# 63 GB RAM .. at 5km resolution
  R <- inla(
      depth ~ 0 + b0 + f( i, model=S0, hyper=hyper.spde2 ) ,
      data=inla.stack.data(Z), 
      # family='gaussian',  # log transf by default .. (?)
#      control.compute=list(dic=TRUE, mlik=TRUE, openmp.strategy='huge'),
#      control.compute=list(dic=TRUE, mlik=TRUE),
             # control.compute = list(cpo=TRUE, pit=TRUE ),  # cpo=conditional predictive ordinate .. leave one out measures of fit to id extreme values (p(y_i|y_{-i}) .. ie. posterior value; # PIT=probability Integral Transforms Pr( y_i {new} <= y_i | y_{-i} ) .. ie on pr scale
              
      #   quantiles=NULL,
   #      control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
      control.predictor=list(A=inla.stack.A(Z), compute=TRUE),
   #   control.inla=list(strategy='gaussian'),
   #   control.inla=list( h=0.002, restart=3, stupid.search=FALSE, stencil=7), 
#     control.inla = list( h=0.002, strategy="laplace", npoints=21, stencil=7 , strategy='gaussian'),  # more points for tails (default is 9)
    #  control.inla=list( stencil=7), 
   #   num.threads=ncpu,
      working.directory = "~/tmp/bathy",
      verbose=TRUE
  )

  fn = "~/tmp/bathy/depths.rdata"
  save(R, file=fn, compress=TRUE )
  # load(fn)

    (R$summary.hyperpar)
    

    # random field parameters on user scale
    oo = inla.spde2.result(R, 'i', S0, do.transf=TRUE)
  
    plot(oo$marginals.variance.nominal[[1]], type='l', xlab=expression(sigma[x]), ylab='Density')

    plot(oo$marginals.kappa[[1]], type='l', xlab=expression(kappa), ylab='Density')

    plot(oo$marginals.range.nominal[[1]], type='l', xlab='range nominal', ylab='Density')
   
    
    # indices for random field at data locations
    idat <- inla.stack.index( Z, 'bathymetry')$data

    # correlation between the the posterior mean and the response by
    cor( depth, R$summary.linear.predictor$mean[idat])
0.994


    # ----------------
    # prediction (of the latent field) for each time and visualize it .. first create projector from mesh to output

      pG = inla.mesh.projector( M0, xlim=p$corners$plon, ylim=p$corners$plat, dims=c(p$nplons, p$nplats)  )
      inside = inout( pG$lattice$loc, M0.domain$loc ) 
      
      xmean <- list()
      xmean <- inla.mesh.project( pG, R$summary.random$i$mean)
 xmean[xmean>800] = NA
 xmean[xmean<=0] = NA
      levelplot( xmean, xlab='', ylab='', col.regions=topo.colors(200), scale=list(draw=FALSE), aspect="iso" )




Call:
c("inla(formula = depth ~ 0 + b0 + f(i, model = S0, hyper = hyper.spde2), ",  "    data = inla.stack.data(Z), verbose = TRUE, control.predictor = list(A = inla.stack.A(Z), ",  "        compute = TRUE))")

Time used:
 Pre-processing    Running inla Post-processing           Total 
         1.3975       2622.3302         44.6624       2668.3901 

Fixed effects:
     mean      sd 0.025quant 0.5quant 0.975quant   mode kld
b0 1.6162 30.8061   -58.8665   1.6153    62.0484 1.6162   0

Random effects:
Name	  Model
 i   SPDE2 model 

Model hyperparameters:
                                           mean     sd 0.025quant 0.5quant 0.975quant    mode
Precision for the Gaussian observations  0.0052 0.0000     0.0052   0.0052     0.0052  0.0052
Theta1 for i                            -3.1044 0.0029    -3.1112  -3.1039    -3.0998 -3.1027
Theta2 for i                            -7.6021 0.0162    -7.6244  -7.6056    -7.5636 -7.6154

Expected number of effective parameters(std dev): 9662.27(0.5087)
Number of equivalent replicates : 26.91 

Marginal Likelihood:  -1081255.43 
Posterior marginals for linear predictor and fitted values computed


