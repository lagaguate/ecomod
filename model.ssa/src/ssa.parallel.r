
# using local cores repeat the simulation a number of time
# NOTE ::: multicore does not work in MSWindows
 
--------  ssh into tethys first and then run via a screen session

library ("parallel")
library ("foreach" )
library ("doParallel") 

runlabel = "simtest"  
nruns = 10  # number of simulations to run 




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
    p$outdir = project.directory( "model.ssa", "data", runlabel )
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





# now load and process data 
 
p$outdir = project.directory( "model.ssa", "data", runlabel  )

with(p, {
  
  ssa.mean = ssa.var = ssa.med = ssa.min = ssa.max = array( NA, dim=c(nr, nc, n.times) )
  for ( it in 1:ntimes ) {
    X = array( NA, dim=c(nr, nc, nruns) 
    for ( ir in 1:nruns ) {
      fnprefix = file.path( outdir, ir, "out") 
      X[,,ir] = ssa.db( fnprefix=fnprefix, ptype="load", tio=it )  
    }
    ssa.med[,,it] = apply( X, 3, median, na.rm=T )
    ssa.mean[,,it] = apply( X, 3, mean, na.rm=T )
    ssa.var[,,it]  = apply( X, 3, var, na.rm=T )
    ssa.min[,,it]   = apply( X, 3, min, na.rm=T )
    ssa.max[,,it]   = apply( X, 3, max, na.rm=T )
  }

  save ( ssa.med, file=file.path( outdir, "0", "ssa.med.rdata" ), compress=TRUE )
  save ( ssa.mean, file=file.path( outdir, "0", "ssa.mean.rdata" ), compress=TRUE )
  save ( ssa.var, file=file.path( outdir, "0", "ssa.var.rdata" ), compress=TRUE )
  save ( ssa.max, file=file.path( outdir, "0", "ssa.max.rdata" ), compress=TRUE )
  save ( ssa.min, file=file.path( outdir, "0", "ssa.min.rdata" ), compress=TRUE )
  save ( p, file=file.path( outdir, "0", "p.rdata" ), compress=TRUE )
}


fn.to.delete = list.files( p$outdir, pattern=".*out.*.rdata", recursive=TRUE, full.names=TRUE )
file.remove( fn.to.delete ) 

dirs.to.delete =  list.files( p$outdir, pattern="*", recursive=TRUE, full.names=TRUE, include.dirs=TRUE )
file.remove( fn.to.delete ) # nonempty directories will not be deleted




