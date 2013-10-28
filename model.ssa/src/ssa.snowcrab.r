




############## Model Parameters 
# Basic logistic with spatial processes  
# Using: logistic model as base
# dX/dt = rX(1-X/K)
# Solution via SSA -- Gillespie Alogrithm:  direct computation of everything


# NOTE:: using .Internal or .Primitive is not good syntax but 
#        this gives a major perfance boost upto 40%


  # set.seed(1)

  # require(ff)  # try using disk to share data across systems
  

  p = list()

  p$init = loadfunctions( c( "model.ssa", "model.pde", "common", "snowcrab" )  )
  p$lib = loadlibraries( c("parallel" ))
  
  
  # details of output storage locations
  p$outdir = project.directory( "model.ssa", "data" )
  p$runname = "test1"
  dir.create( file.path( p$outdir, p$runname), recursive=TRUE ) 
  p$outfileprefix =  file.path( p$outdir, p$runname, "out")  

  p$fnroot = tempfile( "ssa", tmpdir=p$outdir )
  p$outfile = paste( tempfile( "ssa", tmpdir=p$outdir ), ".res.rdata", sep="" )


  
  p = ssa.model.definition( p, ptype = "default.logistic" ) 


  ## read in PDE -related parameterizations
  p$spatial.domain = "snowcrab"  # spatial extent and data structure 
  p = model.pde.define.spatial.domain(p)
  
  # calc here to avoid repeating calculations in the simulation loop
  p$nr_1 = p$nr-1
  p$nc_1 = p$nc-1

  
  # pde related params
  p$eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
  p$atol = 1e-9  # atol -- absolute error tolerance for lsoda
  p$rtol = 1e-9  # rtol -- relative error tolerance for lsoda


  # ideally only one process should be picked at a time ... 
  # but as the sampling from the propensities is the most time-expensive part of this method
  # a slightly larger number of picks are made in advance and then upadted ..
  p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe



  p$nX = 10^6 # multiplier to convert to unit individuals


  # rows are easting (x);  columns are northing (y) --- in R 
  # ... each cell has dimensions of 1 X 1 km ^2

  # rate of increase per day .. for snow crab r~1 per capita per year ===> therefore per day = 1/365
  # p$modeltimes = c( 0, 5, 100, 200, 400, 800, 1600 )  # times at which output is desired
  p$modeltimeoutput = c( 0, 5, 10, 20, 21, 22, 23, 24, 25, 40, 50 )  # times at which output is desired

  p$parmeterizations = c( 
    "reaction.K.snowcrab.mature", 
    "reaction.r.constant", 
    # "diffusion.random.normal",
    "diffusion.second.order.central",
    # "diffusion.first.order.upwind",
    # "advection.random.normal",
    #"advection.random.normal",
    ""
  ) 

  p$y = 2010
  
 
  # model parameters
  p$b = matrix( nrow=p$nr, ncol=p$nc, data=3/365 ) # birth rate
  p$d = matrix( nrow=p$nr, ncol=p$nc, data=2/365 ) # death rate
  
  p$r = p$b - p$d   ### not used in SSA but must match above

  # p$K = matrix( nrow=p$nr, ncol=p$nc, data=rnorm( p$nrc, mean=p$K, sd=p$K/10) )
  p$K = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" ) 
  # p$K[ p$inothabitat ] = p$eps


  iifin = which (!is.finite( p$K) ) 
  if (length(iifin)>0) p$K[iifin] = p$eps


  # abundance::
  # X = ff( initdata = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" ),
  #  dim=c( p$nr, p$nc), filename = paste( p$fnroot, ".X.ff.tmp", sep="" ) , overwrite=TRUE, finalizer="delete" )

  X = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" ) 


  # in the stochastic form:: using a birth-death Master Equation approach 
  # birth = b
  # death = d
  # carrying capacity = K
  # r = b-d >0 
 
  
  # diffusion coef d=D/h^2 ; h = 1 km; per year (range from 1.8 to 43  ) ... using 10 here 
  # ... see b ulk estimation in model.lattice/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r
  p$dr=matrix( ncol=p$nc, nrow=p$nr, data=10)
  p$dc=matrix( ncol=p$nc, nrow=p$nr, data=10) 
  p$Da = matrix( ncol=p$nc, nrow=p$nr, data=10 ) 
  # p$Da = matrix( ncol=p$nc, nrow=p$nr, data=rnorm( p$nrc, mean=p$Da, sd=p$Da/10 ) )
  
  
  
  # model run dimensions and times
  p$n.times = 365  # number of censuses  
  p$t.end =   365   # in model time .. days
  p$t.censusinterval = p$t.end / p$n.times
  p$modeltimeoutput = seq( 0, p$t.end, length=p$n.times )  # times at which output is desired .. used by pde

  
  # state variable
  p <- within( p, {
    # initiate state space with some random noise and a core area in the center of the system
    X = array( 0, dim=c( nr, nc ) ) 
    rwind = floor(nr/10*4.5):floor(nr/10*5.5)
    cwind = floor(nc/10*4.5):floor(nc/10*5.5)
    X[ rwind, cwind ] = round( K[] * 0.8 )
  }) 


  # propensities
  p <- within( p, {
    # initiate P the propensities 
    P = array( RE( X[], b, d, K, dr, dc ), dim=c( nr, nc, np ) )
    P.total = sum( P[] )
    nP = length( P[] )
  }) 


  # single run
  p = ssa.engine.approximation.snowcrab( p )  # same as  ssa.engine.approximation right now ... but if additional changes such as fishing etc ... then it should be done in the snowcrab version
  
  p$runtype = "snowcrab" --- use this in ssa.parallel to choose alternate engines 
  # multiple runs in parallel
  ssa.parallel.run ( DS="run", p=p  ) # run the simulation in parallel
  ssa.parallel.run ( DS="post.process", p=p  ) # postprocess the simulations gathering a few statistics

  # load some of the run results
  X = ssa.parallel.run ( DS="load", p=p, run=1 )  # to load run 1 
  X = ssa.parallel.run ( DS="load", p=p, run="median" ) # etc. .. "mean", "var", "min", "max" ... add as needed.
  
  # delete raw simulation files 
  ssa.parallel.run ( DS="delete.individual.runs", p=p  ) 
 





  ### ---------------------------------------
  ### Compare with a PDE version of the model 


  require (deSolve)
  require (lattice)

  A = array( 0, dim=c(p$nr, p$nc ) ) 
  debug = TRUE
  if (debug) {
    rwind = floor(p$nr/10*4.5):floor(p$nr/10*5.5)
    cwind = floor(p$nc/10*4.5):floor(p$nc/10*5.5)
    A = array( 0, dim=c(p$nr, p$nc ) ) 
    A[ rwind, cwind ] = round( p$K * 0.8 )
    # X[,] = round( runif(p$nrc) * K )
  }
 
  
  p$parmeterizations = c( "reaction", "diffusion.second.order.central") 

  
  res <- ode.2D(  times=p$modeltimeoutput, y=as.vector(A), parms=p, dimens=c(p$nr, p$nc),
    func=single.species.2D.logistic, 
    method="lsodes", lrw=1e8,  
    atol=p$atol 
  )
 

  image.default( matrix(res[365,2:10001], nrow=100), col=heat.colors(100) )

  diagnostics(res)
  
  plot(p$modeltimeoutput, apply(res, 1, sum))
  
  image(res)
  hist( res[1,] )

  select <- c(1, 4, 10, 20, 50, 100, 200, 500 )
  image(res, xlab = "x", ylab = "y", mtext = "Test", subset = select, mfrow = c(2,4), legend =  TRUE)
 



  # adding discontinuities -- e.g. fishing
    
  p$perturbation = "fishing.random"
  p$fishing.event = c( 21  )

  res <- ode.2D( times=p$modeltimeoutput, y=as.vector(A), parms=p, 
      dimens=c(p$nr, p$nc), method=rkMethod("rk45ck"), 
      func=single.species.2D.logistic,    
      events = list(func=perturbation.event, time=p$fishing.event ), 
      atol=p$atol 
  )
  




