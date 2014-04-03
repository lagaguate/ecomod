

  # Spatial reaction-diffusion model solved via stochastic simulation using the Gillespie Alogrithm
  # exact and approximations with some parallel implementations

  # set.seed(1)

  p = list()
  p$lib = loadlibraries( c("parallel", "Rcpp", "RcppArmadillo"))
  p$init = loadfunctions( c( "model.ssa", "model.pde", "common" )  )
  
  p = ssa.parameters( p, ptype = "systemsize.debug" ) 
  p = ssa.parameters( p, ptype = "logistic.debug" ) 
  
  
  p = ssa.parameters( p, ptype = "simtimes.debug" ) 
  # overrides
      p <- within( p, { 
        n.times =  365  # number of censuses  
        t.end =    365   # in model time .. days
        t.censusinterval = t.end / n.times
        modeltimeoutput = seq( 0, t.end, length=n.times )  # times at which output is desired .. used by pde
      })


  # p = ssa.model.definition( p, ptype = "logistic" ) 
  # p = ssa.model.definition( p, ptype = "logistic.randomwalk" ) 
  p = ssa.model.definition( p, ptype = "logistic.correlated.randomwalk" ) 
  

  # initialize state variables and propensity matrix
  res = ssa.db( p , ptype="debug" ) 
  res$simtime = 0       # time in units of the simulation (days)
  res$nevaluations = 0  # used for debugging and counting evaluations to estimate computational speed ...


  ## profiling
  profiling =FALSE
  if (profiling) {
    require(profr)
    o = profr( {ssa.engine.approximation ( p, res )} ) 
    o = profr( {ssa.engine.approximation.rcpp ( p, res )} ) 
    summary(o)
    plot(o)
  }


  ## benchmarking
  compare = FALSE
  if (compare) {
    require(rbenchmark)
    res0 = res
    benchmark( 
      res = ssa.engine.approximation.rcpp ( p, res0 ) , 
      res = ssa.engine.approximation ( p, res0 ) ,
      res = ssa.engine.exact ( p, res0 ) 
    )

  }


  if (ssa.method == "exact" ) {
    p$rn = 0  # default run number when not in parallel mode .. no need to change
    p$runname = "debug.exact"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    p$monitor = TRUE
    res = ssa.engine.exact( p, res)   # using the exact solution ... ~ 1 day -> every 25-30 minutes
  }

  
  if (ssa.method == "approximation" ) {
    # approximation simular to the tau-leaping method:: ideally only one process should be picked at a time ... 
    #   sampling from the propensities is time-expensive, so a number of picks are made in advance and then updated ..
    p$rn = 0  # default run number when not in parallel mode .. no need to change
    p$runname = "debug.approximation"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    p$ssa.approx.proportion = 0.01
    p$nsimultaneous.picks =  round( p$nrc * p$ssa.approx.proportion ) # 1% update simultaneously should be /seems to be safe  ~ 1 day -> every 1-2 min or 2hrs->100days 
    p$insp = 1:p$nsimultaneous.picks
    p$monitor = TRUE
    # res = ssa.engine.approximation( p, res )
    res = ssa.engine.approximation ( p, res )
  }


  if (ssa.method == "approximation.rcpp" ) {
    # ~ 2X faster  than ssa.method="approximation"
    p$rn = 0  # default if using single runs ... otherwise, with multiple runs, the run numebers are generated automatically 
    p$runname = "snowcrab.approximation"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    p$nsimultaneous.picks =  round( p$nrc * 0.1 ) # 0.1% update simultaneously should be safe
    p$insp = 1:p$nsimultaneous.picks
    p$monitor = TRUE
    p$outfilenameroot = file.path( p$outdir, "individual.runs", p$rn, "out" )
    res = ssa.engine.approximation.rcpp ( p, res ) 
    # X = ssa.db( ptype="load", outdir=p$outdir, tio=10, rn=p$rn )  
    # image(X)
  }


  if (ssa.method == "approximation_rcpp_direct" ) {
    ## incomplete ... file save and "RE" non yet operational data typing issues
    p$rn = 0  # default if using single runs ... otherwise, with multiple runs, the run numebers are generated automatically 
    p$runname = "snowcrab.approximation"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    p$nsimultaneous.picks =  round( p$nrc * 0.1 ) # 0.1% update simultaneously should be safe
    p$insp = 1:p$nsimultaneous.picks
    p$monitor = TRUE
    p$outfilenameroot = file.path( p$outdir, "individual.runs", p$rn, "out" )
    
    loadfunctions( "model.ssa", directorypattern="rcpp", filepattern="*_rcpp.r" )

    loadfilelist( paste( rcppdir, c("ssa_engine_approximation.rcpp") ## loads ssa_engine_approximation_rcpp into memory
    loadfunctions(  "model.ssa", filepattern="ssa_engine_approximation.rcpp") ## loads ssa_engine_approximation_rcpp into memory
    
    res = ssa_engine_approximation_rcpp_direct ( p, res ) 

    #  ... but if additional changes such as fishing etc ... then a new engine should be created
    # takes about 800 MB per run
    # X = ssa.db( ptype="load", outdir=p$outdir, tio=10, rn=p$rn )  
    # image(X)
  }




  if (ssa.method == "approximation.parallel" ) {

    # use parallel mode to run multiple simulations is the most efficient use of resources 
    # wrapper is "ssa.parallel" (below)
 
    p$runname = "debug.approximation.parallel"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
 
    p$libs = loadlibraries(  "parallel" , "rlecuyer", "snow" )
  
    p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    # p$cluster = 4  # if a single number then run only on localhost with n cores.
    p$cluster = rep( "localhost", 5 )
    
    p$cluster.message.system = "SOCK" 
    #p$cluster.message.system = "PSOCK" 

    # choose and make a copy of the core ssa engine 
    # p$ssa.engine = ssa.engine.exact
    # p$ssa.engine = ssa.engine.approximation
    p$ssa.engine = ssa.engine.approximation 

    p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe
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
    # no real spead up vs exact method ... most time is spent swaping memory space / attaching/detaching
    
    #### BROKEN for now ... no need to fix until speed increase is worth the trouble 
    
    p$libs = loadlibraries(  "parallel", "bigmemory" )
    p$cluster = c( rep("localhost", 4) ) 
    p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    p$pconnectmethod = "SOCK" 
    p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe
    p = ssa.db( p , ptype="debug.big.matrix.rambacked" )

    p$monitor = FALSE
 
    p = ssa.engine.parallel.bigmemory( p  )
  }



  if (ssa.method == "filebacked.approximation.parallel" ) {
    # no real spead up vs exact method ... most time is spent swaping memory space / attaching/detaching
    
    #### BROKEN for now ... no need to fix until speed increase is worth the trouble 
    
    p$libs = loadlibraries(  "parallel", "bigmemory" )
    p$cluster = c( rep("localhost", 4) ) 
    p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    p$cluster.message.system = "SOCK" 
    p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe
    p = ssa.db( p , ptype="debug.big.matr:ix.filebacked" )
       
    p$monitor = FALSE
    p = ssa.engine.parallel.bigmemory( p )
  }





  if ( pmethod=="compare.with.PDE.solution" ) {
    # Compare with a PDE version of the model 
    require (deSolve)
    require (lattice)
    p$parmeterizations = c( "reaction", "diffusion.second.order.central") 
    p = ssa.db( p , ptype="debug" )  # update state variable to initial conditions
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

 







