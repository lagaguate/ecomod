
# using local cores repeat the simulation a number of time
# NOTE ::: multicore does not work in MSWindows
 

library ("parallel")
library ("foreach" )
library ("doParallel") 

 
nruns = 10

if (running.beowulf) {
  ncores = 2  # on each system
  clusters = c( rep("tethys", min(8, ncores)), rep( "kaos", ncores), rep("nyx", ncores ), rep( "tartarus", ncores) )
  ctype = "SOCK"
  # ctype = "MPI"  --- startup error -- need to check 
  cl = makeCluster( spec=clusters, type=ctype ) 
}

if (running.locally) { 
  ncores = detectCores()-1
  cl = makeCluster (ncores) # essentially using multicore
  registerDoParallel( cl, cores=ncores )  
}



res = foreach( i=1:nruns, .verbose=TRUE ) %dopar% {
  try( {
    # full run of ssa  
    p = list()
    p$init = loadfunctions( c( "model.ssa", "model.pde" )  )
    p$outdir = project.directory( "model.ssa", "data", "simtest"  )
    p$runname = i 
    dir.create( file.path( p$outdir, p$runname), recursive=TRUE )
    p$outfnprefix =  file.path( p$outdir, p$runname, "out") 
    
    p = ssa.model.definition( p, ptype = "default.logistic" )
    p = ssa.parameters( p, ptype = "systemsize.debug" )
    p = ssa.parameters( p, ptype = "logistic.debug" )
    p = ssa.parameters( p, ptype = "simtimes.debug" )
    p = ssa.db( p , ptype="debug" ) # initialize state variables and propensity matrix
    p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 1% update simultaneously should be /seems to be safe  ~ 1 day -> every 1-2 min or 2hrs->100days
    p = ssa.engine.approximation( p )
    p$runname  # return run name
  })

}
 

stopCluster(cl)



