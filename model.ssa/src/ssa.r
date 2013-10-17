

  # Spatial reaction-diffusion model solved via stochastic simulation using the Gillespie Alogrithm


  # set.seed(1)



  p = list()
  
  p$init = loadfunctions( c( "model.ssa", "model.pde" )  )
  p = ssa.model.definition( p, ptype = "default.logistic" ) 
  p = ssa.parameters( p, ptype = "systemsize.debug" ) 
  p = ssa.parameters( p, ptype = "logistic.debug" ) 
  p = ssa.parameters( p, ptype = "simtimes.debug" ) 
   

  
  # details of output storage locations
    p$outdir = project.directory( "ssa", "data" )
    p$runname = "test1"
    dir.create( file.path( p$outdir, p$runname), recursive=TRUE ) 
    p$outfnprefix =  file.path( p$outdir, p$runname, "out")  
  

  # initialize state variables and propensity matrix
  p = ssa.db( p , ptype="debug" )
  

  # Possible solution methods

  # using the exact solution ... ~ 1 day -> every 5-10 minutes
    p = ssa.engine.exact( p )
  

  # using an apporximation simular to the tau-leaping method -- must choose :: default debugging runs: ~ 1 day -> every 1-2 min
    # ideally only one process should be picked at a time ... 
    # but as the sampling from the propensities is the most time-expensive part of this method
    # a slightly larger number of picks are made in advance and then upadted ..
    p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe
    p = ssa.engine.approximation( p )



  # using parallel methods -- both give disappointing results not much faster than the exact method ... most time is spent swaping memory space
    p$clusters = rep( "localhost", 2) 
    # nclu = 24
    # p$clusters = c( rep("kaos", nclu ), rep("nyx", nclu ), rep("tartarus", nclu ) )
    p$pconnectmethod = "SOCK" 
    p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe
    if ( pmethod=="filebacked" ) {
      p = ssa.db( p , ptype="debug.big.matrix.filebacked" )
      p = ssa.engine.parallel.bigmemory( p )
    }

    if ( pmethod=="rambacked" )  {
      p = ssa.db( p , ptype="debug.big.matrix.rambacked" )
      p = ssa.engine.parallel.bigmemory( p )
    }

ssa.engine.parallel.bigmemory.r


  plot( seq(0, t.end, length.out=n.times), apply(out[], 3, mean), pch=".", col="blue", type="b" ) 





  ### ---------------------------------------
  ### Compare with a PDE version of the model 

  require (deSolve)
  require (lattice)
 
  p = list()
  
  p$init = loadfunctions( c( "model.ssa", "model.pde" )  )
  p = ssa.model.definition( p, ptype = "default.logistic" ) 
  p = ssa.parameters( p, ptype = "systemsize.debug" ) 
  p = ssa.parameters( p, ptype = "logistic.debug" ) 
  p = ssa.parameters( p, ptype = "simtimes.debug" ) 
   
  p = ssa.db( p , ptype="debug" )

  A = p$X
  
  p$parmeterizations = c( "reaction", "diffusion.second.order.central") 
  
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
 







