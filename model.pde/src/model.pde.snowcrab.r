

  # simple PDE 2D spatial model of interacting nodes in a lattice
  loadfunctions( c("spacetime", "utility", "parallel", "habitat", "snowcrab", "temperature", "depth", "bathymetry" ) )
  loadfunctions( "model.pde" )


  require (deSolve)
  require (lattice)

  set.seed(1)

  p = list()


  p$spatial.domain = "snowcrab"  # spatial extent and data structure 
  p = model.pde.define.spatial.domain(p)

   
  
  # rate of increase per day .. for snow crab r~1 per capita per year ===> therefore per day = 1/365
  
 # p$modeltimes = c( 0, 5, 100, 200, 400, 800, 1600 )  # times at which output is desired
  p$modeltimeoutput = c( 0, 5, 10, 20, 21, 22, 23, 24, 25, 40, 50 )  # times at which output is desired

  p$parmeterizations = c( 
    "reaction.K.snowcrab.mature", 
    "reaction.r.constant", 
    "diffusion.random.normal",
    "diffusion.second.order.central",
    # "diffusion.first.order.upwind",
    "advection.random.normal",
    #"advection.random.normal",
    ""
  ) 

  p$y = 2010
  
  p = model.pde.parameters(p)

  # abundance::
  A = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" ) 
  A = A * runif(  p$nrc, min=0.8, max=1 ) 

   
  #### NOTE :: lsodes needs lrw to be reset if dimensionality of the problem changes 
  ####.. delete it to obtain a guess from deSolve
  out <- ode.2D(  times=p$modeltimeoutput, y=as.vector(A), parms=p, dimens=c(p$nr, p$nc),
    func=single.species.2D.logistic, 
    method="lsodes", lrw=1e8,  
    atol=p$atol 
  )
 


  # adding discontinuities -- e.g. fishing
    
  p$perturbation = "fishing.random"
  p$fishing.event = c( 21  )

  out <- ode.2D( times=p$modeltimeoutput, y=as.vector(A), parms=p, 
      dimens=c(p$nr, p$nc), method=rkMethod("rk45ck"), 
      func=single.species.2D.logistic,    
      events = list(func=perturbation.event, time=p$fishing.event ), 
      atol=p$atol 
  )
  




  diagnostics(out)
  
  plot(p$modeltimeoutput, apply(out, 1, sum))
  
  image(out)
  hist( out[1,] )
  hist( out[10,] )

  o = array( out, dim=c( nrow(out), p$nr, p$nc)) 

  levelplot( o[1,,] , aspec="iso")  

  image(out)


  select <- c(1, 4, 10, 20, 50, 100, 200, 500 )
  image(out, xlab = "x", ylab = "y", mtext = "Test", subset = select, mfrow = c(2,4), legend =  TRUE)
   
  image(out, subset = (time == 10))
  # image(out, xlab = "x", ylab = "y", mtext = paste("time = ", modeltimes))
 
   

