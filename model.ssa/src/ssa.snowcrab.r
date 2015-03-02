
############## Model Parameters 
# Basic logistic with spatial processes  
# Using: logistic model as base
# dX/dt = rX(1-X/K)
# Solution via SSA -- Gillespie Alogrithm:  direct computation of everything


  # set.seed(1)

   p$libs = RLibrary( c("parallel", "Rcpp",  "rlecuyer" ))
   p$init = loadfunctions( c( "model.ssa", "model.pde",  "spacetime", "utility", "parallel", "snowcrab"  )  )
   
    # diffusion coef d=D/h^2 ; h = 1 km; per year (range from 1.8 to 43  ) ... using 10 here 
    # ... see b ulk estimation in model.lattice/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r
   p$runname = "debug"
   p$monitor = TRUE  # output figures / summary stats ~ 10% performance hit
   p$ssa.approx.proportion = 0.05

    # System size definitions
    # pde related params already define the snow crab data 
    # rows are easting (x);  columns are northing (y) --- each cell has dimensions of 1 X 1 km ^2
   p$spatial.domain = "snowcrab"
  
    # p$increment   = 5 /100 * 5  # ...  approx max density is ~ 100 t / km^2  
    #  .... so ~ 1 percentage min dX value to use at the lowest level for rate processes; 5 -> ~ 5% 

   p$n.times = 10  # number of censuses  
   p$ t.end =   10   # in model time .. days

  p = model.pde.define.spatial.domain(p)
  p = ssa.parameters( p, DS = "snowcrab.debug" )
  
  # ----------------------------
  # Model definitions

  # Biological rates parameterizations
  # rate of increase per day .. for snow crab r~1 per capita per year ===> therefore per day = 1/365
  # in the stochastic form:: using a birth-death Master Equation approach 
  # birth = b
  # death = d
  # carrying capacity = K
  # r = b-d >0 
 
  
  choose.model = TRUE
  if (choose.model) {
    stop( "stop and choose a model" )
 
    p$jump.increment = 1 
 
    # p = ssa.model.definition( p, DS = "logistic", increment=p$jump.increment ) 
    # p = ssa.model.definition( p, DS = "logistic.randomwalk", increment=p$jump.increment ) 
    p = ssa.model.definition( p, DS = "logistic.correlated.randomwalk", increment=p$jump.increment ) 
  
  }

   
  p$y = 2011  # currently picking a single year for data streams ... must make this more general
  res = ssa.db( p , DS="snowcrab.debug" ) 
  
  # same as  ssa.engine.approximation right now but if additional changes such as fishing etc 
  # ... then it should be done in the snowcrab version takes about 500 MB per run
  res = ssa.engine( p, res )  


  if (ssa.method == "approximation.parallel" ) {
    # use parallel mode to run multiple simulations is the most efficient use of resources 
    # wrapper is "ssa.parallel" (below)
    p$runname = "snowcrab.approximation.parallel"
    p$monitor = FALSE # not possible with parallel mode
    p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    p$cluster = rep( "localhost", 5 )
    p$cluster.message.system = "SOCK" 
    #p$cluster.message.system = "PSOCK"

    p$nruns = 5
  
    ssa.parallel.run ( DS="run", p=p, res=res  ) # run the simulation in parallel
    ssa.parallel.run ( DS="post.process", p=p  ) # postprocess the simulations gathering a few statistics

    # load some of the run results
    X = ssa.parallel.run ( DS="load", p=p, run=1 )  # to load run 1 (to debug) 
    X = ssa.parallel.run ( DS="load", p=p, run="median" ) # etc. .. "mean", "var", "min", "max" ... add as needed.
  
    # visualize data
    image(X[,,1])

    # delete raw simulation files 
    delete = FALSE
    if (delete) ssa.parallel.run ( DS="delete.individual.runs", p=p  ) 
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
  




