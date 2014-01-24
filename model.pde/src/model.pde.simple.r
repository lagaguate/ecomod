

  # simple PDE 2D spatial model of interacting nodes in a lattice

  require (deSolve)
  require (lattice)

  set.seed(1)

  p = list()
  p$init = loadfunctions( c( "model.pde" )  )
   
  p$nr = 100  
  p$nc = 100 
  p$nn = p$nr*p$nc

  # rate of increase per day .. for snow crab r~1 per capita per year ===> therefore per day = 1/365
  p$r = 1 / 365
  p$K = 100
  p$Da = matrix( ncol=p$nc, nrow=p$nr, data=10 ) 

  
  # model run dimensions and times
  p$n.times = 365  # number of censuses  
  p$t.end =   365   # in model time .. days
  p$modeltimeoutput = seq( 0, p$t.end, by=5 )  # times at which output is desired .. used by pde
 
  
  # rows are easting (x);  columns are northing (y) --- in R 
  # ... each cell has dimensions of 1 X 1 km ^2

  # pde related params
  p$eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
  p$atol = 1e-9  # atol -- absolute error tolerance for lsoda
  p$rtol = 1e-9  # rtol -- relative error tolerance for lsoda
  
  p$dc = 1
  p$dr = 1

  
 # p$modeltimes = c( 0, 5, 100, 200, 400, 800, 1600 )  # times at which output is desired
  p$modeltimeoutput = c( 0, 5, 10, 20, 21, 22, 23, 24, 25, 40, 50 )  # times at which output is desired

  p$parmeterizations = c( "reaction", "diffusion.second.order.central" ) 

  # abundance::
  A =  array( 0, dim=c(p$nr, p$nc ) ) 
    rwind = floor(p$nr/10*4.5):floor(p$nr/10*5.5)
    cwind = floor(p$nc/10*4.5):floor(p$nc/10*5.5)
    A = array( 0, dim=c(p$nr, p$nc ) ) 
    A[ rwind, cwind ] = round( p$K * 0.8 )
    
  # A = A * runif(  p$nn, min=0.8, max=1 ) 

   
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
  levelplot( o[5,,] , aspec="iso")  




  # or using deSolve's plotting mechanism:
  image(out)
  
  select <- c(5, 10, 50 )
  image(out, xlab = "x", ylab = "y", mtext = "Test", subset = select, mfrow = c(2,4), legend =  TRUE)
   
  image(out, subset = (time == 10))
  # image(out, xlab = "x", ylab = "y", mtext = paste("time = ", modeltimes))
 
   

