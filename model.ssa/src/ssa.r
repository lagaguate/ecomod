

  # Spatial reaction-diffusion model solved via stochastic simulation using the Gillespie Alogrithm
  # exact and approximations with some parallel implementations

  # ---------
  # Runtime parameters and options

  p = list()
  p$libs = loadlibraries( c("parallel", "Rcpp"))
  p$init = loadfunctions( c( "model.ssa", "model.pde", "common" )  )

  loadfunctions( "model.ssa", filepattern="*.rcpp" )  # load and compile supporting C/Rcpp programs

  p$monitor = TRUE  # output figures / summary stats ~ 10% performance hit
  p$ssa.approx.proportion = 0.01  # 0.1% update simultaneously should be safe

  p = ssa.parameters( p, DS = "systemsize.debug" ) 
  p = ssa.parameters( p, DS = "logistic.debug" ) 
  p = ssa.parameters( p, DS = "simtimes.debug" ) 
  # overrides
      p <- within( p, { 
        n.times =  365  # number of censuses  
        t.end =    365   # in model time .. days
        t.censusinterval = t.end / n.times
        modeltimeoutput = seq( 0, t.end, length=n.times )  # times at which output is desired .. used by pde
      })

  # p = ssa.model.definition( p, DS = "logistic" ) 
  # p = ssa.model.definition( p, DS = "logistic.randomwalk" ) 
  p = ssa.model.definition( p, DS = "logistic.correlated.randomwalk" ) 

  # initialize state variables and propensity matrix
  res = ssa.db( p , DS="debug" ) 
  res$simtime = 0       # time in units of the simulation (days)
  res$nevaluations = 0  # used for debugging and counting evaluations to estimate computational speed ...


  if (ssa.method == "exact" ) {
    p$runname = "debug.exact"
    res = ssa.engine.exact( p, res)   # using the exact solution ... ~ 1 day -> every 25-30 minutes
  }

  if (ssa.method == "approximation" ) {
    p$runname = "debug.approximation"
    res = ssa.engine.approximation ( p, res )
  }


  if (ssa.method == "default" ) {   # ~ 2X faster  than ssa.method="approximation" .. uses Rcpp/C++
    p$runname = "default"
    res = ssa.engine.approximation.rcpp ( p, res ) 
    # X = ssa.db( p=p, DS="load", tio=10 )  
    # image(X)
  }


  if (ssa.method == "approximation.parallel" ) {
    # use parallel mode to run multiple simulations .. currently the most efficient use of resources 
    # wrapper is "ssa.parallel" (below)
    p$runname = "debug.approximation.parallel"
    p$libs = c( p$libs,  loadlibraries(  "parallel" , "rlecuyer", "snow" ) )
    # p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    p$cluster = rep( "localhost", detectCores() )
    p$cluster.message.system = "SOCK" 
    #p$cluster.message.system = "PSOCK" 
    p$ssa.engine = ssa.engine.approximation.rcpp 
    p$nruns = 6
   
    p$monitor = FALSE
    
    ssa.parallel.run ( DS="run", p=p, res=res  ) # run the simulation in parallel
    ssa.parallel.run ( DS="post.process", p=p  ) # postprocess the simulations gathering a few statistics

    # load some of the run results
    X = ssa.parallel.run ( DS="load", p=p, run=1 )  # to load run 1 (to debug) 
    X = ssa.parallel.run ( DS="load", p=p, run="mean" ) # etc. .. "mean", "var", "min", "max" ... add as needed.
 
    # image(X[,,365] ) 

    # delete raw simulation files 
    # ssa.parallel.run ( DS="delete.individual.runs", p=p  ) 
  }

  

  if ( pmethod=="rambacked.approximation.parallel" )  {
    p$libs = c( p$libs, loadlibraries(  "parallel", "bigmemory" ) )
    p$cluster = rep( "localhost", detectCores() )
    p$cluster.message.system = "SOCK" 
    
    p = ssa.db( p , DS="debug.big.matrix.rambacked" )
    p$monitor = FALSE
    b = ssa.engine.parallel.bigmemory( p  )
  }



  if (ssa.method == "filebacked.approximation.parallel" ) {
    p$libs = loadlibraries(  "parallel", "bigmemory" )
    p$cluster = rep( "localhost", detectCores() )
    # p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    p$cluster.message.system = "SOCK" 
    p = ssa.db( p , DS="debug.big.matr:ix.filebacked" )
    p$monitor = FALSE
    p = ssa.engine.parallel.bigmemory( p )
  }


  if (debugmodels) {
    require(profr)
    require(rbenchmark)
    res0 = res
    p$monitor = FALSE
    
    benchmark( res = ssa.engine.exact( p, res0 ) , replications=1 )
    benchmark( ssa.engine.approximation ( p, res0 ) , replications=1 )
    benchmark( ssa.engine.approximation.rcpp ( p, res0 ) , replications=1 )
    
    o = profr( {ssa.engine.direct ( p, res0 )} ) ; summary(o)
 
  }


  if ( pmethod=="compare.with.PDE.solution" ) {
    # Compare with a PDE version of the model 
    require (deSolve)
    require (lattice)
    p$parmeterizations = c( "reaction", "diffusion.second.order.central") 
    p = ssa.db( p , DS="debug" )  # update state variable to initial conditions
    A = p$X
    out <- ode.2D(  times=p$modeltimeoutput, y=as.vector(A), parms=p, dimens=c(p$nr, p$nc),
      func=single.species.2D.logistic, 
      method="lsodes", lrw=1e8,  
      atol=p$atol 
    )
   
    image.default( matrix(out[365,2:10001], nrow=100), col=heat.colors(100) )
    diagnostics(out)
    plot(p$modeltimeoutput, apply(out, 1, sum))
    image(out)
    hist( out[1,] )
    select <- c(1, 4, 10, 20, 50, 100, 200, 500 )
    image(out, xlab = "x", ylab = "y", mtext = "Test", subset = select, mfrow = c(2,4), legend =  TRUE)
  }


  plot( seq(0, t.end, length.out=n.times), apply(out[], 3, mean), pch=".", col="blue", type="b" ) 

 







