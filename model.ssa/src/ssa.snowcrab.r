




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
  

  # ----------------------------
  # Model definitions
  p = ssa.model.definition( p, ptype = "default.logistic" ) 



  # ----------------------------
  # System size definitions
  # pde related params already define the snow crab data 
  # rows are easting (x);  columns are northing (y) --- in R 
  # ... each cell has dimensions of 1 X 1 km ^2
  p$spatial.domain = "snowcrab"  # spatial extent and data structure 
  p = model.pde.define.spatial.domain(p)
  
  

  # ----------------------------
  # Additional parameters and some calculations here to avoid repeating within the simulation loop
  p$nX = 10^6 # multiplier to convert to unit individuals
  p$eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
  p$atol = 1e-9  # atol -- absolute error tolerance for lsoda
  p$rtol = 1e-9  # rtol -- relative error tolerance for lsoda
  p$nr_1 = p$nr-1
  p$nc_1 = p$nc-1
  p$rn = 0  # if using single runs ... otherwise, with multiple runs, the run numebers are generated automatically 



 

  # ----------------------------
  # Time dimensions and constraints
  p <- within( p, { 
    n.times = 20 # 365  # number of censuses  
    t.end =   2 # 365   # in model time .. days
    t.censusinterval = t.end / n.times
    modeltimeoutput = seq( 0, t.end, length=n.times )  # times at which output is desired .. used by pde
    #modeltimeoutput = c( 0, 5, 10, 20, 21, 22, 23, 24, 25, 40, 50 )  # times at which output is desired
  })





  # ----------------------------
  # Biological rates parameterizations
  # rate of increase per day .. for snow crab r~1 per capita per year ===> therefore per day = 1/365
  # in the stochastic form:: using a birth-death Master Equation approach 
  # birth = b
  # death = d
  # carrying capacity = K
  # r = b-d >0 
 
  
  # diffusion coef d=D/h^2 ; h = 1 km; per year (range from 1.8 to 43  ) ... using 10 here 
  # ... see b ulk estimation in model.lattice/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r

  p$y = 2010  # currently picking a single year for data streams ... must make this more general
  
  p <- within(p, {
    parmeterizations = c( 
        "reaction.K.snowcrab.mature", 
        "reaction.r.constant", 
        # "diffusion.random.normal",
        "diffusion.second.order.central",
        # "diffusion.first.order.upwind",
        # "advection.random.normal",
        #"advection.random.normal",
        "")
    b = matrix( nrow=nr, ncol=nc, data=3/365 ) # birth rate
    d = matrix( nrow=nr, ncol=nc, data=2/365 ) # death rate
    r = b-d # not used in SSA but must match above for PDE
    K = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" )
    # K = matrix( nrow=p$nr, ncol=p$nc, data=rnorm( p$nrc, mean=p$K, sd=p$K/10) )
    # K[ inothabitat ] = eps
    DaR = matrix( ncol=nc, nrow=nr, data=10)
    DaC = matrix( ncol=nc, nrow=nr, data=10) 
    Da = matrix( ncol=nc, nrow=nr, data=10 ) 
    # Da = matrix( ncol=nc, nrow=nr, data=rnorm( nrc, mean=Da, sd=Da/10 ) )
  })

  iifin = which (!is.finite( p$K) ) 
  if (length(iifin)>0) p$K[iifin] = p$eps


  
  # state variable 
  p <- within( p, {
    # initiate state space with some random noise and a core area in the center of the system
    X = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" )
 
    # add some random noise and a core area in the center of the system
    debug = FALSE
    if( debug) { 
      rwind = floor(nr/10*4.5):floor(nr/10*5.5)
      cwind = floor(nc/10*4.5):floor(nc/10*5.5)
      X[ rwind, cwind ] = round( X[ rwind, cwind ] * runif( length(X[ rwind, cwind ]) ) )
    }  
  }) 


  # propensities
  p <- within( p, {
    # initiate P the propensities 
    P = array( RE( X[], b, d, K, DaR, DaC ), dim=c( nr, nc, np ) )
    P.total = sum( P[] )
    nP = length( P[] )
  }) 


  if (ssa.method == "exact" ) {
    p$runname = "snowcrab.exact"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    p = ssa.engine.exact( p )  # same as  ssa.engine.approximation right now ... but if additional changes such as fishing etc ... then it should be done in the snowcrab version
    # takes about 500 MB per run
  }


  if (ssa.method == "approximation" ) {
    p$runname = "snowcrab.approximation"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    p$nsimultaneous.picks =  round( p$nrc * 0.05 ) # 0.1% update simultaneously should be safe
    p = ssa.engine.approximation.snowcrab( p )  # same as  ssa.engine.approximation right now ... but if additional changes such as fishing etc ... then it should be done in the snowcrab version
    # takes about 500 MB per run
  }



  if (ssa.method == "approximation.parallel" ) {
    # use parallel mode to run multiple simulations is the most efficient use of resources 
    # wrapper is "ssa.parallel" (below)
    p$runname = "snowcrab.approximation.parallel"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    
    p$libs = loadlibraries(  "snow" , "rlecuyer" )
   
    p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    p$cluster = rep( "localhost", 5 )
    p$cluster.message.system = "SOCK" 
    #p$cluster.message.system = "PSOCK" 

    
    # choose and make a copy of the core ssa engine 
    # p$ssa.engine = ssa.engine.exact
    # p$ssa.engine = ssa.engine.approximation
    # p$ssa.engine = ssa.engine.approximation.snowcrab
    p$ssa.engine = ssa.engine.approximation 
 
    p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe
    p$nruns = 6
 
    ssa.parallel.run ( DS="run", p=p  ) # run the simulation in parallel
    ssa.parallel.run ( DS="post.process", p=p  ) # postprocess the simulations gathering a few statistics

    # load some of the run results
    X = ssa.parallel.run ( DS="load", p=p, run=1 )  # to load run 1 (to debug) 
    X = ssa.parallel.run ( DS="load", p=p, run="median" ) # etc. .. "mean", "var", "min", "max" ... add as needed.
  
    # delete raw simulation files 
    ssa.parallel.run ( DS="delete.individual.runs", p=p  ) 
  }




  ### ---------------------------------------
  ### Compare with a PDE version of the model 


  require (deSolve)
  require (lattice)

  # update with 
  A = ssa.parallel.run ( DS="load", p=p, run=1 ) # to load run 1 (to debug) 
  
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
 

  plot( seq(0, t.end, length.out=n.times), apply(out[], 3, mean), pch=".", col="blue", type="b" ) 


  # adding discontinuities -- e.g. fishing
  p$perturbation = "fishing.random"
  p$fishing.event = c( 21  )

  res <- ode.2D( times=p$modeltimeoutput, y=as.vector(A), parms=p, 
      dimens=c(p$nr, p$nc), method=rkMethod("rk45ck"), 
      func=single.species.2D.logistic,    
      events = list(func=perturbation.event, time=p$fishing.event ), 
      atol=p$atol 
  )
  




