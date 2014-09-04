
  # test to see utility in using inla's SPDE approach towards 
  # spatio-temporal estimation of historical temperature data
  

  # ----------------
  # Prep OSD, snow crab and groundfish temperature profiles    
  # this one has to be done manually .. no longer mainted by anyone ..
  # temperature.profiles = hydro.db( DS="osd.rawdata.refresh" )  
  
  # from choijae; Jc#00390 :: (http://www.mar.dfo-mpo.gc.ca/science/ocean/database/data_query.html) 
  # depths: 500,500, "complete profile"   .. raw data  for the SS (region: jc.ss")
  # must download manually to this directory and run gzip



    p = list()
    p$libs = RLibrary( c( "chron", "gstat", "sp", "parallel", "mgcv", "bigmemory" ) )
    p$init.files = loadfunctions( c( "spatialmethods", "parallel", "utility", "bathymetry", "temperature" ) ) 
		

    RLibrary( "INLA" )
    inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors


 
    p$tyears = c(1970:2013)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    newyear = c( 2013)

    p$wtimes = 1:52 
    p$nw = length(p$wtimes)
    p$ny = length(p$tyears)

    p = spatial.parameters( p=p, type="SSE" ) #  type="canada.east"  can be completed later (after assessment) when time permits if required
    
    t0 = hydro.db( p=p, DS="bottom.gridded.all"  )

    t0 = t0[   filter.region.polygon( t0, region="isobath1000m" ) , ]
    t0 = t0[ - filter.region.polygon( t0, region="isobath1000m" ) , ]


    t0 = t0[ which( t0$yr %in% p$tyears ), ]

    t0$yrindex = as.integer( t0$yr - min(t0$yr) + 1 )
    
    locs0  = as.matrix( t0[,c("plon", "plat")] )
 
    nData0 = nrow(locs0) 


  # boundary domain
  M0.domain = inla.nonconvex.hull( locs0, convex=10, resolution=120 )

  M0 = inla.mesh.2d (
      loc=locs0, # locations of data points
      boundary=M0.domain, 
      offset=c( 10, 100 ),  # how much to extend in the c(inner, outer) domains
      max.edge=c( 10, 100 ),  # max size of a triange (in, out)
      min.angle=c(21),   # min angle (in, out)
      cutoff=10 # min distance allowed
  )       
  plot(M0, asp=1 ) # visualise mesh
  


  t0$b0 = rep(1, nData0)  # intercepts

  # SPDE components
  # matern representation using mesh M
  S0 = inla.spde2.matern( M0, alpha=2 ) # alpha=2 is exponential correlation function

  # indices of SPDE 
  # only time and mesh dependence ,, not location! 
  i <- inla.spde.make.index('i', n.spde=S0$n.spde, n.group=p$ny )  

  # projection matrix A to translate from mesh nodes to data nodes
  A = inla.spde.make.A( mesh=M0, loc=locs0, n.group=p$ny, group=t0$yrindex )


  # data stack for occurence (PA)
  Z = inla.stack( 
      tag="tdata",
      data=list( tC=t0$t ) ,
      A=list(A,1),
      effects=list( i=i, t0 ) 
  )
  
  theta.ar1 = list( theta1=list(param=c(1, 0.1) ), rho=list( param=c( 0.9, 1/0.1 ) ) )  # N( mean=0.9, var=0.1) 

  R <- inla(
      tC ~ 0 + b0 
             + f(i, model=S0, group=i.group, control.group=list(model='ar1', hyper=theta.ar1 ), diagonal=1e-3) 
             + f(weekno, model='seasonal', season.length=52, diagonal=1e-3 ) , 
      family='gaussian',  # log transf by default .. (?)
      data=inla.stack.data(Z), 
   #   control.compute=list(dic=TRUE),
      control.predictor=list(A=inla.stack.A(Z), compute=TRUE),
      verbose=TRUE
  )




    # prediction of the random field: two ways .. with analysis or after analysis
    # (aside: mesh points are already predicted )

    newlocations = rbind(  c( 330, 4790), c(788, 5080) )
    Anew = inla.spde.make.A ( mesh=M, loc=newlocations )  # sparse matrix of the projection coefficients
    

    # A) simple projection, post-analysis  
    Anew %*% R$summary.random$i$mean

    # B) or using inla functionality    
    inla.mesh.project( inla.mesh.projector( M, loc=newlocations), R$summary.random$i$mean )

    # C) or using inla in a global analysis
     
      # simple model so this is not needed but to be consistent:
      i <- inla.spde.make.index('i', n.spde=S$n.spde )  
      # i = 1:S$n.spde  # alternatively this
    
    Znew = inla.stack( 
      data = list( B=NA),
      A = list( Anew ),
      effects = list( i=i, m=rep(1, nrow(newlocations) ) ),
      tag="spatialpredictions"
    )
    
    Z = inla.stack( Z, Znew )
   
    # refit the same model


    ipredictions = inla.stack.index( Z, tag="spatialpredictions")
    R$summary.linear.predctor[ ipredictions$data ,]

   
    # Prediction onto a grid using projections (method A/B) is faster/computationaly more efficient .. method of choice
    pG = inla.mesh.projector( M, xlim=p$corners$plon, ylim=p$corners$plat, dims=c(p$nplons, p$nplats) )
    # pG = inla.mesh.projector( M, dims=c(p$nplons, p$nplats) )
    
    Pmean = inla.mesh.project( pG, R$summary.random$i$mean )  # posterior mean
    Psd = inla.mesh.project( pG, R$summary.random$i$s )       # posterior SD
    image(log(Pmean))

    
    # Prediction of the response variable (B) .. ie. imputation
    Zimp = inla.stack( 
      data=list(B=NA), 
      A=list( Anew, 1 ), # 1 is for the intercept
      effects=list( i=1:S$n.spde, m=rep(1, nrow(newlocations) ) ), # 1 is for the intercept
      tag="prediction.response"
    )

    Z = inla.stack( Z, Zimp )
    R = inla(  
      B ~ 0 + m + f(i, model=S), 
      data=inla.stack.data(Z), 
      control.predictor=list( A=inla.stack.A(Z), compute = TRUE ) ## new addition here to compute linear predictions but this is CPU expensive
    )

    iimputations = inla.stack.index( Z, tag="prediction.response")
    R$summary.fitted.values[ iimputations$data ,]
    oo = R$marginals.fitted.values[ iimputations$data ]
    inla.hpdmarginal( 0.95, oo[[2]] ) 
    inla.zmarginal( oo[[2]])


    # or more directly:
    R$summary.fix[1,1] + Anew %*% R$summary.random$i$mean  # for the intercept
    sqrt( 1^2 + R$summary.fix[1,2]^2 + drop( Anew %*% R$summary.random$i$sd^2 ) ) # for SE 


