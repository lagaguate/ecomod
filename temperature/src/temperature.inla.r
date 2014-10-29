
  # test timeseries methods and spatial, spatio-temporal methods on ocean temperature data
    RLibrary( "INLA" )
    inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors

    # -------------------
    # 1. example model fit of a simple AR1 data series with inla
    n = 200
    ti =  1:n
    y = arima.sim(n=n, model=list(ar=0.9))
    y[ 75:80] = NA

    dat = data.frame( ti=ti, y=y) 
    r = inla( y ~ 0 + f( ti, model="ar1" ), 
      data = dat,  
      control.family = list(initial = 10, fixed=TRUE) ## no observational noise
    )

    summary(r)
    plot(r)



    # -------------------
    # 2. now temperature data timeseries

    p = list()
    p$libs = RLibrary( c( "chron", "gstat", "sp", "parallel", "mgcv", "bigmemory", "INLA" ) )
    p$init.files = loadfunctions( c( "spatialmethods", "parallel", "utility", "bathymetry", "temperature", "polygons" ) ) 

    p$tyears = c(1970:2013)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    p$wtimes = 1:52 
    p = spatial.parameters( p=p, type="SSE" ) #  type="canada.east"  can be completed later (after assessment) when time permits if required
    
    inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors
    
    t0 = hydro.db( p=p, DS="bottom.gridded.all"  )
    t0 = t0[ which( t0$yr %in% p$tyears ), ]
  # t0 = t0[ filter.region.polygon( t0, region="isobath1000m" ) , ]


    fp = data.frame( lon= -63, lat= 45 )
    dl = 0.5
    subarea = which( t0$lon > (fp$lon-dl) & t0$lon < (fp$lon+dl) & t0$lat < (fp$lat+dl) &  t0$lat > (fp$lat-dl) )
    t0 = t0[ subarea ,]
    
    t0$yrindex = t0$yr - min(t0$yr) + 1
    t0$pryr = (t0$weekno-1)/52
    t0$ti = t0$yr + t0$pryr
    t0$logZ = log(t0$z)
    t0$tC = t0$t
    t0$b0 = 1  # intercepts

    R <- inla(
        tC ~ 0 + b0
          + f( z, model='rw1')
          + f( yr, model='ar1', param=c(1,0.0001)) 
          + f( pryr, model='ar1', cyclic=TRUE, param=c(1,0.0001) ),  
        family='gaussian',  
        data=t0, 
        control.compute=list(dic=TRUE),
        control.predictor=list( compute=TRUE),
        verbose=TRUE
    )
    summary(R)
    plot(R)
    # plot( t ~ ti, t0 )

    round( R$summary.fixed)

Fixed effects:
     mean     sd 0.025quant 0.5quant 0.975quant   mode   kld
b0 3.6505 2.9344       -2.1   3.6705     9.2672 3.6834 1e-04

Random effects:
Name	  Model
 z   IID model 
yr   AR1 model 
pryr   AR1 model 

Model hyperparameters:
                                        mean   sd     0.025quant 0.5quant 0.975quant mode  
Precision for the Gaussian observations 0.3308 0.0363 0.2617     0.3305   0.4037     0.3314
Precision for z                         0.5211 0.1061 0.3567     0.5054   0.7699     0.4730
Precision for yr                        0.5285 0.1594 0.2723     0.5107   0.8919     0.4774
Rho for yr                              0.5352 0.1511 0.2112     0.5457   0.7941     0.5649
Precision for pryr                      0.1365 0.0669 0.0376     0.1271   0.2914     0.0981
Rho for pryr                            0.9212 0.0393 0.8309     0.9266   0.9795     0.9440

Expected number of effective parameters(std dev): 410.20(53.28)
Number of equivalent replicates : 2.606 

Deviance Information Criterion: 4617.06
Effective number of parameters: 412.15

Marginal Likelihood:  -2485.71 
Posterior marginals for linear predictor and fitted values computed


    p$gam.optimizer = "nlm"
    p$tsmethod = "seasonal.basic"
    p$tsmethod = "seasonal.smoothed"
    p$tsmethod = "harmonics.1"
  

    # choose model formula for GAM-based models
    mf = switch( p$tsmethod ,
      annual = ' t ~ s(yr) ',
      seasonal.basic = ' t ~ s(yr) + s(weekno, bs="cc") ', 
      seasonal.smoothed = ' t ~ s(yr, weekno) + s(yr) + s(weekno, bs="cc")  ', 
      harmonics.1 = ' t ~ s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w)  ', 
      harmonics.2 = ' t ~ s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w) + s(yr, cos.w2) + s(yr, sin.w2) + s(cos.w2) + s( sin.w2 ) ' , 
      harmonics.3 = ' t ~ s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w) + s(yr, cos.w2) + s(yr, sin.w2) + s(cos.w2) + s( sin.w2 ) + s(yr, cos.w3) + s(yr, sin.w3)  + s(cos.w3) + s( sin.w3 ) '
    )
    mf = formula(mf)

   t0$w = 1 / (( t0$lon - fp$lon)**2 + (t0$lat - fp$lat)**2 )# weight data in space: inverse distance squared
   t0$w[ which( t0$w < 1e-3 ) ] = 1e-3
   # t0 = t0[,c("t", "w", "yr", "weekno" )]
   # data transformations and creation of new variables where required for raw data 
   
   x = t0
   
   # default output grid
   OP = expand.grid( weekno=p$wtimes, yr=p$tyears )
   OP$fit = NA  # these will be filled in with predicted fits and se's
   OP$se  = NA


    OP$tiyr = OP$yr + OP$weekno/52 
    x$tiyr =  x$yr + x$weekno/52
   
   if ( p$tsmethod %in% c( "harmonics.1", "harmonics.2", "harmonics.3"  ) ) {
      x$cos.w  = cos( x$tiyr )
      x$sin.w  = sin( x$tiyr )
      OP$cos.w  = cos( OP$tiyr )
      OP$sin.w  = sin( OP$tiyr )
      # compute additional harmonics only if required (to try to speed things up a bit)
      if ( p$tsmethod %in% c( "harmonics.2", "harmonics.3"  ) ) {
        x$cos.w2 = cos( 2*x$tiyr )
        x$sin.w2 = sin( 2*x$tiyr )
        OP$cos.w2 = cos( 2*OP$tiyr )
        OP$sin.w2 = sin( 2*OP$tiyr )
      }
      if ( p$tsmethod %in% c( "harmonics.3"  ) ) {
        x$cos.w3 = cos( 3*x$tiyr )
        x$sin.w3 = sin( 3*x$tiyr )
        OP$cos.w3 = cos( 3*OP$tiyr )
        OP$sin.w3 = sin( 3*OP$tiyr )
      }
    }
    
    # estimate model parameters
    tsmodel = NULL 
    tsmodel = switch( p$gam.optimizer ,
      bam = try( bam( mf, data=x, weights=w ) ) ,
      bfgs = try( gam( mf, data=x, weights=w, optimizer=c("outer","bfgs")  ) ) ,
      perf = try( gam( mf, data=x, weights=w, optimizer=c("perf")  ) ) ,
      newton = try( gam( mf, data=x, weights=w, optimizer=c("outer","newton")  ) ) ,
      nlm = try( gam( mf, data=x, weights=w, optimizer=c("outer","nlm")  ) ) 
    )
   
    summary(tsmodel)
    AIC(tsmodel)

    if ( ! "try-error" %in% class(tsmodel) ) {
      out = try( predict( tsmodel, newdata=OP, type="response", se.fit=T ) ) 
      OP$fit = out$fit
      OP$se = out$se
      x11()
      plot( OP$fit ~ OP$tiyr,  type="l", main=p$tsmethod, sub=paste( "AIC", round(AIC(tsmodel))) )
    }
    
    # other methods
    OP$time = OP$yr + OP$weekno / 52
    OP = OP[ order( OP$time ) ,]

    # STL method
    vn = "fit"
    OPts = ts( OP[,vn] , start=c( min(p$tyears), 1), frequency=52 )
    plot.default(  OPts, type="l" )

    OPstl = stl( OPts, s.window=4) # seasonal decomposition using loess 
    plot(OPstl)

    # StructTS method
    OPstr  = StructTS( OPts, type = "BSM") ### much slower ... used only as a daignostic tool checking stability
    OPstrsm = tsSmooth( OPstr )
    plot( OPstrsm )
 
    spectrum( OPts )
    kn = kernel( "modified.daniell", c(2,1))

    vn = "fit"
    opop = kernapply( as.vector(OP[,vn] ), kn)
    plot(OP[,vn], type="l")
    lines(opop, type="l", col="orange")

    ppp = kernapply( OPts, kn)
    plot.default(  OPts, type="l", col="green" )
    lines( ppp, col = "red")  

    ooo = spectrum( OPts, kn, plot=FALSE )
    plot( ooo, plot.type = "marginal") # the default type
    plot( ooo, plot.type = "coherency")
    plot( ooo, plot.type = "phase")


    require(forecast)
    Arima( OPts )
  


  # -------------------
  # 3. spatial model only on temperature
 
    p = list()
    p$libs = RLibrary( c( "chron", "gstat", "sp", "parallel", "mgcv", "bigmemory", "INLA" ) )
    p$init.files = loadfunctions( c( "spatialmethods", "parallel", "utility", "bathymetry", "temperature", "polygons" ) ) 

    p$tyears = c(2012)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    p$wtimes = 1:52 
    p = spatial.parameters( p=p, type="SSE" ) #  type="canada.east"  can be completed later (after assessment) when time permits if required
    
    inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors
    
    t0 = hydro.db( p=p, DS="bottom.gridded.all"  )
    t0 = t0[ which( t0$yr %in% p$tyears ), ]
    
    t0$plon = round( t0$plon)
    t0$plat = round( t0$plat)

  # boundary domain
  locs0  = as.matrix( t0[,c("plon", "plat")] )
  
  M0.domain = inla.nonconvex.hull( locs0, convex=10, resolution=120 )

  M0 = inla.mesh.2d (
      loc=locs0, # locations of data points
      boundary=M0.domain, 
      offset=c( 10, 100 ),  # how much to extend in the c(inner, outer) domains
      max.edge=c( 10, 100 ),  # max size of a triange (in, out)
      min.angle=c(21),   # min angle (in, out)
      cutoff=10 # min distance allowed
  )       
  
  
# plot(M0, asp=1 ) # visualise mesh
    t0$yrindex = t0$yr - min(t0$yr) + 1
    t0$pryr = (t0$weekno-1)/52
    t0$ti = t0$yr + t0$pryr
    t0$logZ = log(t0$z)
    t0$b0 = 1  # intercepts

  # SPDE components
  # matern representation using mesh M
  S0 = inla.spde2.matern( M0, alpha=2 ) # alpha=2 is exponential correlation function

  # indices of SPDE 
  # only time and mesh dependence ,, not location! 
  i <- inla.spde.make.index('i', n.spde=S0$n.spde )  

  # projection matrix A to translate from mesh nodes to data nodes
  A = inla.spde.make.A( mesh=M0, loc=locs0 )

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
             + f(i, model=S0) 
             + f(pryr, model='ar1', cyclic=TRUE ) , 
      family='gaussian',  # log transf by default .. (?)
      data=inla.stack.data(Z), 
   #   control.compute=list(dic=TRUE),
      control.predictor=list(A=inla.stack.A(Z), compute=TRUE),
      verbose=TRUE
  )

    # prediction of the random field: two ways .. with analysis or after analysis
    # (aside: mesh points are already predicted )

    newlocations = rbind(  c( 330, 4790), c(788, 5080) )
    Anew = inla.spde.make.A ( mesh=M0, loc=newlocations )  # sparse matrix of the projection coefficients

    # A) simple projection, post-analysis  
    Anew %*% R$summary.random$i$mean

    # B) or using inla functionality    
    inla.mesh.project( inla.mesh.projector( M0, loc=newlocations), R$summary.random$i$mean )

    # C) or using inla in a global analysis
     
      # simple model so this is not needed but to be consistent:
      i <- inla.spde.make.index('i', n.spde=S$n.spde )  
      # i = 1:S$n.spde  # alternatively this
    
    Znew = inla.stack( 
      data = list( B=NA),
      A = list( Anew,1 ),
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



  # -------------------
  # 4. spatial-temporal model on temperature
 
    p = list()
    p$libs = RLibrary( c( "chron", "gstat", "sp", "parallel", "mgcv", "bigmemory", "INLA" ) )
    p$init.files = loadfunctions( c( "spatialmethods", "parallel", "utility", "bathymetry", "temperature", "polygons" ) ) 

    p$tyears = c(1990:2013)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
    p$wtimes = 1:52 
    p = spatial.parameters( p=p, type="SSE" ) #  type="canada.east"  can be completed later (after assessment) when time permits if required
    
    inla.setOption(scale.model.default = TRUE)  # better numerical performance of IGMRF models and less dependnence upon hyperpriors
    
    t0 = hydro.db( p=p, DS="bottom.gridded.all"  )

    # scotianshelf = locator() 
    scotianshelf = read.table( polygon.ecomod( "scotia.fundy.with.buffer.dat"  ) ) 
    names( scotianshelf) = c("lon", "lat")

    plot( t0$lat ~ t0$lon, pch="." )
    lines( scotianshelf )

    a = which( point.in.polygon( t0$lon, t0$lat, scotianshelf$lon, scotianshelf$lat ) != 0 )

    t0 = t0[a,]

   #  t0$plon = round( t0$plon)
   #  t0$plat = round( t0$plat)
    
    t0 = t0[ which( t0$yr %in% p$tyears ), ]
    t0$yrindex = t0$yr - min(t0$yr) + 1
    t0$pryr = (t0$weekno-1)/52
    t0$ti = t0$yr + t0$pryr
    t0$logZ = log(t0$z)
    t0$b0 = 1  # intercepts

  # boundary domain
  locs0  = as.matrix( t0[,c("plon", "plat")] )
  M0.domain = inla.nonconvex.hull( locs0, convex=10, resolution=120 )

  M0 = inla.mesh.2d (
      loc=locs0, # locations of data points
      boundary=M0.domain, 
      offset=c( 5, 100 ),  # how much to extend in the c(inner, outer) domains
      max.edge=c( 5, 100 ),  # max size of a triange (in, out)
      min.angle=c(21),   # min angle (in, out)
      cutoff=10 # min distance allowed
  )       
  
  # plot(M0, asp=1 ) # visualise mesh

  # SPDE components
  # matern representation using mesh M
  S0 = inla.spde2.matern( M0, alpha=2 ) # alpha=2 is exponential correlation function

  # indices of SPDE 
  # only time and mesh dependence ,, not location! 
  ny = length(unique(t0$yr))
  i <- inla.spde.make.index('i', n.spde=S0$n.spde, n.group=ny )  

  # projection matrix A to translate from mesh nodes to data nodes
  A = inla.spde.make.A( mesh=M0, loc=locs0, n.group=ny, group=t0$yrindex )


  # data stack for occurence (PA)
  Z = inla.stack( 
      tag="tdata",
      data=list( tC=t0$t ) ,
      A=list(A,1),
      effects=list( i=i, t0 ) 
  )
  
  # rho hyperparmeters seem to specify mean and variance instead of mean precision (according to spde tutorial) .. need to check this .. assume precision for now ...
  hyper.year   = list( theta1=list(param=c(1, 0.01) ), rho=list( param=c( 0.9, 0.01 ) ) )  # N( mean=0.9, var=0.1)  
  hyper.season = list( theta1=list(param=c(1, 0.01) ), rho=list( param=c( 0.8, 0.01 ) ) )  # N( mean=0.9, var=0.1) 


# 11 years takes ~ 1 hr
  R <- inla(
      tC ~ 0 + b0 
             + f( i, model=S0, diagonal=1e-3,
                 group=i.group, control.group=list(model='ar1', hyper=hyper.year) ) 
             + f( pryr, model='ar1', cyclic=TRUE, hyper=hyper.season, diagonal=1e-3) , 
      family='gaussian',  # log transf by default .. (?)
      data=inla.stack.data(Z), 
      control.compute=list(dic=TRUE, mlik=TRUE),
   #   quantiles=NULL,
      control.results=list(return.marginals.random=FALSE, return.marginals.predictor=FALSE ),
      control.predictor=list(A=inla.stack.A(Z), compute=TRUE),
   #   control.inla=list(strategy='gaussian'),
      control.inla=list( h=0.01), #h is step size for hessian
      verbose=TRUE
  )


  save(R, file="~/tmp/R.spatio.temporal.1990.2013.rdata", compress=TRUE )


summary(R)
R$summary.hyperpar


# seasonal effect
plot(R$summary.random$pryr$mean)
lines(R$summary.random$pryr$mean + R$summary.random$pryr$sd*2)
lines(R$summary.random$pryr$mean - R$summary.random$pryr$sd*2)
round(res$summary.hy[3,], 5)  # temporal autocorrelation

# annual effect 



#correlation between data and predictions
idat <- inla.stack.index( Z, 'tdata')$data 
cor( t0$t, R$summary.linear.predictor$mean[idat], use="pairwise.complete.obs" ) # 89% 
plot( t0$t, R$summary.linear.predictor$mean[idat] )

    
pG = inla.mesh.projector( M0, xlim=(p$corners$plon), ylim=(p$corners$plat), dims=c(p$nplons, p$nplats) )
out = inla.mesh.project( pG, R$summary.random$i$mean[ i$i.group==1 ] ) # first 
levelplot( out, aspect="iso" )

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
    pG = inla.mesh.projector( M0, xlim=p$corners$plon, ylim=p$corners$plat, dims=c(p$nplons, p$nplats) )
    # pG = inla.mesh.projector( M0, dims=c(p$nplons, p$nplats) )
    
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


