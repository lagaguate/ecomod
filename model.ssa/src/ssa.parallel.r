
# using local cores parllelization as single threaded RAM-based operations are fastest 
# NOTE :::
#   multicore does not work in MSWindows
#   ssh into tethys first and then run via a screen session


  p = list(
    init = loadfunctions( c( "model.ssa", "model.pde")  ),
    libs = loadlibraries(  "parallel", "foreach", "doParallel"),
    beowulf.cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ), 
    beowulf.message.system = "SOCK",  # MPI"
    ssa.model.definition = "default.logistic", 
    ssa.systemsize.parameters = "systemsize.debug",
    ssa.model.parameters = "logistic.debug", 
    ssa.time.parameters = "simtimes.debug",
    ssa.state.variables = "debug", 
    ssa.approx.proportion = 0.01,  # 1% update simultaneously should be /seems to be safe  ~ 1 day -> every 1-2 min or 2hrs->100days
    nruns = 10 , # number of simulations to run 
    runlabel = "simtest"   
  )
    
  p = ssa.model.definition( p, ptype=p$ssa.model.definition )
  p = ssa.parameters( p, ptype=p$ssa.systemsize.parameters )
  p = ssa.parameters( p, ptype=p$ssa.model.parameters )
  p$nsimultaneous.picks = round( p$nrc * p$ssa.approx.proportion ) # 1% update simultaneously should be /seems to be safe  ~ 1 day -> every 1-2 min or 2hrs->100days
  p = ssa.parameters( p, ptype=p$ssa.time.parameters )
  p = ssa.db( p , ptype=p$ssa.state.variables ) # initialize state variables and propensity matrix



  # run the simulation in parallel
  # ssa.parallel.run ( DS="local", p=p ) 
  ssa.parallel.run ( DS="beowulf", p=p  ) 

  # postprocess the simulations gathering a few statistics
  ssa.parallel.run ( DS="post.process", p=p  ) 

  # load some of the run results
  X = ssa.parallel.run ( DS="load", p=p, run=1 )  # to load run 1 
  X = ssa.parallel.run ( DS="load", p=p, run="median" ) # etc. .. "mean", "var", "min", "max" ... add as needed.
  
  # delete raw simulation files 
  ssa.parallel.run ( DS="delete.individual.runs", p=p  ) 





