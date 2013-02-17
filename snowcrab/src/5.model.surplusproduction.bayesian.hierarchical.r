
 
  require(rjags)
  rjags::load.module("dic")
  rjags::load.module("glm")

  
	loadfunctions( "bayesian" )
	loadfunctions( "snowcrab", functionname="initialise.local.environment.r") 


  ###  all data follow this sequence: c("cfanorth", "cfasouth", "cfa4x")
  
  # curve( dnorm(x, mean=1, sd=1*0.5), from=0.1, to=4  )
  # curve( dlnorm(x, meanlog=log(1), sdlog=1),  from=0.1, to=100  )
  # curve( dbeta(x, 10, 10),  from=0.01, to=1  )
  # curve( dgamma(x, shape=0.001, rate=0.001),  from=0.01, to=2  )

  
  redo.data=F
  if (redo.data) { 
    biomass.summary.db("complete.redo", p=p)
  }
  
  res = biomass.summary.db(p=p)
     
  sb = list( 
    b0x = c(4/5, 4/5, 2/5),  # prior: mean value possible in  N,S,4X 
    q0x = c(1, 1, 1),  # prior 
    r0x = c(1, 1, 1),  # hyper prior 
    K0x = c(5, 50, 1 ),  # max carrying capacity estimate 
    IOA = as.matrix(res$B), # observed index of abundance
    IOAcv = as.matrix(res$B.sd ), # observed index of log abundance SD estimates
    IREC = as.matrix(res$R), # observed index of abundance
    IRECcv = as.matrix(res$R.sd ), # observed index of log abundance SD estimates
    CAT = as.matrix(res$L) , # catches  , assume 20% handling mortality and illegal landings
    p.g = 2,  # dgamma parameter = 2 is an exponential family with mean = p.g * p.h; p.h=mean/p.g
    cv = 0.4, #default 1/cv  -- muliplier for dgamma on inverse scale
    logsdmax = 1,  # upper bound of sd of lognormal distributions 1== assume < 1 order of magnitude variation and modal 
    er = 0.2,  # target exploitation rate
    U = ncol( res$B),  # number of regions
    N = nrow( res$B) , # no years with data  
    M = 3, # no years for projections  
    ty = 7,  # index of the transition year (2004) between spring and fall surveys 
    cfa4x = 3, # index of cfa4x
    epsIOA = 1e-2,  # eps for IOA 
    eps = 1e-4  # small non-zero number
  )
 
# set poor data to NA's and maximize associated CV's
  
  cfa.north =  1 # column index
  cfa.north.bad.data = which( as.numeric(rownames(sb$IOA)) <= 1997 )
  sb$IOA[ cfa.north.bad.data, cfa.north ] = NA 
  sb$IOAcv[ cfa.north.bad.data, cfa.north ] = mean( sb$IOAcv[,cfa.north ] ,na.rm=T )
 
  cfa.south =  2 # column index
  cfa.south.bad.data = which( as.numeric(rownames(sb$IOA)) <= 1998 )
  sb$IOA[ cfa.south.bad.data, cfa.south ] = NA 
  sb$IOAcv[ cfa.south.bad.data, cfa.south ] = mean( sb$IOAcv[,cfa.south ] ,na.rm=T )
 
  cfa.nodata =   which( as.numeric(rownames(sb$IOA)) <= 2003 )
  sb$IOA[ cfa.nodata , sb$cfa4x ] = NA 
  sb$IOAcv[ cfa.nodata , sb$cfa4x ] = mean( sb$IOAcv[,sb$cfa4x] ,na.rm=T )
 
  # recruitment index has no process model right now ... use min value
  sb$IREC[ cfa.nodata , sb$cfa4x ] = min( sb$IREC[,sb$cfa4x] ,na.rm=T )
  sb$IRECcv[ cfa.nodata , sb$cfa4x ] = mean( sb$IRECcv[,sb$cfa4x] ,na.rm=T )

  sb$IREC = trunc( scale( sb$IREC, center=T, scale=T ) / 3 * 10000 ) / 10000
 


  # MCMC/Gibbs parameters
  n.adapt = 10000 # burn-in
  n.iter = 6000 
  n.chains = 3
  n.thin = 500
  n.iter.final = n.iter * n.thin
  fnres = file.path( project.directory("snowcrab"), "R", paste( "surplus.prod.mcmc", p$current.assessment.year,"rdata", sep=".") )
 

  debug =F
  if (debug) {
    n.adapt = 1000 # burn-in
    n.iter = 3000 
    n.chains = 3
    n.thin = 5
    n.iter.final = n.iter 
    fnres = file.path( project.directory("snowcrab"), "R", "surplus.prod.mcmc.debug.rdata" )
  }


  # -------------------
  ##  simple surplus production with observation and process error
  # m = jags.model( file=file.path( project.directory("snowcrab"), "src", "bugs", "biomassdynamic_2010.bugs" ),
  #   data=sb, n.chains=n.chains, n.adapt=n.adapt )
  m = jags.model( file=fishery.model.jags ( DS="biomass.dynamic.candidate", yr=2013 ), data=sb, n.chains=n.chains, n.adapt=n.adapt )  
 
  m = jags.model( file=fishery.model.jags ( DS="biomass.dynamic" ), data=sb, n.chains=n.chains, n.adapt=n.adapt )  
  
  tomonitor =  c("B", "MSY", "BMSY", "FMSY", "Fcrash", "F", "P",    
                
                 "qs", "sd.qs", 
                 "r", "r.sd", "r.mu", "cvr", "sd.r", "rb1", "rb2", "r.q.sd", "r.q.mu", 
                 "r.sd.hyp", "r.mu.hyp","r.q.mu.hyp", "r.sd.hyp", "cv.r", 
                 "q", "q.sd", "q.mu", "cvq", "sd.q", "qb1", "qb2", "q.mu.hyp","cv.q", 
                 "pdbeta", "cvp", 
                 "uabeta", "ua",

                 "b0", "cvb0", "sd.b0", "sd.p", "b0.mu", "b0.sd",
                 "biomass", "cvb", "sd.pq", "cv.p", 
                 "K", "cvk", "sd.K", "K.mu", "K.sd", "K.mu.hyp","cv.K", 
                 "cvc", "rem", "REM", "rem.sd",
                 "ua", "uab1", "uab2", 
                 "cvo", "sd.o", "cbb1", "cbb2",   "sd.o.mu", "sd.o.sd","cv.o", 
                 
                 "TAC",  "C", "catch", "sd.C" )
  tomonitor = intersect( variable.names (m), tomonitor )
  coef(m)
  
  dic.samples(m, n.iter=n.iter ) # pDIC
  
  y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior
  figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseries.png" ) ) 
  figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.png" ) ) 
   
  graphics.off() ; x11()
  layout( matrix(c(1,2,3), 3, 1 )); par(mar = c(5, 4, 0, 2))
  for( i in 1:3) hist(y$cv.r[i,,], "fd")
           


 
Mean deviance:  29.2 
penalty 5386 
Penalized deviance: 5415 

# convergence testing -- by 1000 to 1500 convergence observed by Gelman shrink factor diagnostic
    y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter, thin=n.thin )
    gelman.plot(y[["r"]])
    gelman.plot(y[["K"]])
    gelman.plot(y[["q"]])
    gelman.plot(y[["sd.r"]])
    gelman.plot(y[["sd.K"]]) 
    gelman.plot(y[["sd.o"]])
    gelman.plot(y[["sd.p"]])
    geweke.plot(y[["r"]])


  # update if not yet converged 
  #  update(m, n.iter=n.iter ) # above seems enough for convergence but a few more to be sure


  # autocorrelation thinning
    y = coda.samples(m, variable.names=c("K", "r", "q"), n.iter=n.iter, thin=n.thin) # sample from posterior
    autocorr.plot(y)
    # plot(y, ask=T)
    # autocorr(y, lags = c(0, 1, 5, 10, 50), relative=TRUE)


  # final sampling from the posteriors
  #  y = jags.samples(m, variable.names=tomonitor, n.iter=200000, thin=100) # sample from posterior
    y = jags.samples(m, variable.names=tomonitor, n.iter=n.iter.final, thin=n.thin) # sample from posterior
		fnres =  file.path( project.directory("snowcrab"), "R", "surplus.prod.mcmc.2012_final.rdata" )
    # fnres =  file.path( project.directory("snowcrab"), "R", "surplus.prod.mcmc.2012a.rdata" )
    save(y, file=fnres, compress=T)
    # load( fnres )


  # Figures
    graphics.off()
    
    dir.output = file.path( dirname(p$ofname), "figures", "bugs")   
    dir.create( dir.output, recursive=T, showWarnings=F )

    # frequency density of key parameters
    figure.bugs( "K", y=y, sb=sb, fn=file.path(dir.output, "K.density.png" ) ) 
    figure.bugs( "r", y=y, sb=sb, fn=file.path(dir.output, "r.density.png" ) ) 
    figure.bugs( "q", y=y, sb=sb, fn=file.path(dir.output, "q.density.png" ) ) 
    figure.bugs( "FMSY", y=y, sb=sb, fn=file.path(dir.output, "FMSY.density.png" ) ) 
  
    # timeseries
    figure.bugs( type="timeseries", vname="biomass", y=y, sb=sb, fn=file.path(dir.output, "biomass.timeseries.png" ) ) 
    figure.bugs( type="timeseries", vname="fishingmortality", y=y, sb=sb, fn=file.path(dir.output, "fishingmortality.timeseries.png" ) ) 
      
    # Harvest control rules
    figure.bugs( type="hcr", vname="default", y=y, sb=sb, fn=file.path(dir.output, "hcr.default.png" ) ) 
    figure.bugs( type="hcr", vname="simple", y=y, sb=sb, fn=file.path(dir.output, "hcr.simple.png" ) ) 
     
    # diagnostics
    figure.bugs( type="diagnostic.production", y=y, sb=sb, fn=file.path(dir.output, "diagnostic.production.png" ) ) 
    figure.bugs( type="diagnostic.errors", y=y, sb=sb, fn=file.path(dir.output, "diagnostic.errors.png" ) ) 
    figure.bugs( type="diagnostic.phase", y=y, sb=sb, fn=file.path(dir.output, "diagnostic.phase.png" ) ) 


    ndata = sb$N

    # densities of biomass estimates for the current year
      for (i in 1:3) plot(density(y$B[ndata,i,,] ), main="")
      qs = apply( y$B[ndata,,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
      qs

      # densities of biomass estimates for the previous year
      for (i in 1:3) plot(density(y$B[ndata-1,i,,] ), main="")
      qs = apply( y$B[ndata-1,,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
      qs

      # densities of F in assessment year
      for (i in 1:3) plot(density( y$F[ndata,i,,] ), xlim=c(0.05, 0.5), main="")
      qs = apply( y$F[ndata,,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
      qs
      qs = apply( y$F[ndata,,,], 1, mean )

      # densities of F in previous year
      for (i in 1:3) plot(density( y$F[ndata-1,i,,] ), main="")
      qs = apply( y$F[ndata-1,,,], 1, quantile, probs=c(0.025, 0.5, 0.975) )
      qs

      # F for table ---
      summary(y$F, median)


    debug = F
    if (debug) {
      
      graphics.off()
      
      x11()
      layout( matrix(c(1,2,3), 3, 1 ))
      par(mar = c(5, 4, 0, 2))

      for( i in 1:3) hist(y$r[i,,], "fd")
      for( i in 1:3) hist(y$sd.q[i,,], "fd")
      for( i in 1:3) hist(y$sd.K[i,,], "fd")
      for( i in 1:3) hist(y$sd.b0[i,,], "fd")
      for( i in 1:3) hist(y$sd.pq[i,,], "fd")
      
      for( i in 1:3) hist(y$sd.r[i,,,], "fd")
      for( i in 1:3) hist(y$sd.p[i,,,], "fd")
      for( i in 1:3) hist(y$sd.o[i,,,], "fd")
      for( i in 1:3) hist(y$sd.rem[i,,,], "fd")
       
      for( i in 1:3) hist(y$b0[i,,], "fd")
   
    }


