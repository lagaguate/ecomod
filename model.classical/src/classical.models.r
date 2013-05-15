
  
  library(deSolve)
	loadfunctions ("model.classical")


  # --------------
  # Logistic equation
	S = c( X=0.01 )  # initial conditions of state variables
  P = list( r=3, K=1 ) # parameters
  t0 = 0; t1 = 10; dt = 0.001
  ti = seq( t0, t1, dt) # times at solutions
  out = ode( S, ti, logistic, P, method="lsoda" ) 
  plot(out, type="l")
 

  # --------------
  # Logistc equation discrete map:: by using euler and dt=1
	S = c( X=0.1 )
  P = list( r=2, K=10 )
  t0 = 0; t1 = 100; dt = 1
  ti = seq( t0, t1, dt) # times at solutions
  out = ode( S, ti, discrete.logistic, P, method="euler" ) 
  plot(out, type="l")


  # --------------
  # Logistic  with time-varying K and r
	S = c( X=0.1 )  # initial conditions of state variables
  P = list( r=2, K=1, rk=2*pi/1, Kk=2*pi/4 ) # parameters
  t0 = 0; t1 = 50; dt = 0.01
  ti = seq( t0, t1, dt) # times at solutions
  out = ode( S, ti, logistic.timevarying.Kr, P, method="lsoda" ) 
  plot(out, type="l")
 

  # --------------
  # Lotka-Volterra 2-species
	S = c( X1=0.01, X2=0.4 )  # initial condition
  P = list( r1=0.5, m1=0.2, ae2=0.1, m2=0.2, K1=10 ) 
    # encouter.rate = 0.2/day, rate of ingestion
    # growth rate = 1.0/day, growth rate of prey
    # predator mortality = 0.2/day, mortality rate of predator
    # assimilation efficiency = 0.5,    # -, assimilation efficiency
    # carrying capacity = 10     # mmol/m3, carrying capacity
  t0 = 0; t1 = 100; dt = 0.1
  ti = seq( t0, t1, dt)
  out = ode( S, ti, lotka.volterra.2, P, method="lsoda" ) 
  plot(out, type="l")


 
  # --------------
  # Lotka-Volterra 3-species (Blasius)
  S = c(u=10, v=5, w=0.1)  # this order structures the data output
  ti = seq( 0, 200, 0.1)
  P = list( a=1, b=1, c=10, alpha1=0.2, alpha2=1, k1=0.05, k2=0, wstar=0.006)
  
  out = ode( S, ti, blasius, P, method="lsoda" ) 
  
  plot(out, type="l")

  # plot the simulation results as time series or state trajectory
  par(mfrow=c(2,2))
  plot(out[-1,4], out[-nrow(out),4], type="l")
  
  library(scatterplot3d)
  scatterplot3d(out[,2:4], type="l")
  
	plot(0,0, xlim=c(0,2), ylim=c(0,1.5),
  type="n", xlab="b", ylab="w")
  for (b in seq(0.02,1.8,0.02)) {
    P["b"] = b
    out = as.data.frame( ode( S, ti, M, P, method="lsoda", hmax=0.1))
    l = length(out$w) %/% 3
    out = out[(2*l):(3*l),]
    p = peaks(out$w)
    l = length(out$w)
    S = c(u=out$u[l], v=out$v[l], w=out$w[l])
    points(rep(b, length(p)), p, pch=".")
  }

  S = c(u=10, v=5, w=0.1)  # this order structures the data output
  ti = seq(0, 200, 0.1)
  P = list( a=1, b=1, c=10, alpha1=0.2, alpha2=1, k1=0.05, k2=0, wstar=0.006)
  l = length(ti)
  res = NULL
  for (b in seq(0.02,1.8,0.02)) {
    print (b)
    P["b"] = b
    for (i in 1:10) {
      j = ceiling(runif(1) * l) # new start position
      out = as.data.frame( ode( S, ti, M, P, method="lsoda", hmax=0.1))
      S = c(u=out$u[j], v=out$v[j], w=out$w[j])
      sol = out[l,]
      res = rbind( res, cbind(b, sol) )
  }}



  # --------------
  # Lotka-Volterra 4-species
	P = list(
    r = c(r1 = 0.1, r2 = 0.1, r3 = -0.1, r4 = -0.1),
    A = matrix(c(0.0,  0.0, -0.2,   0.01,      # prey 1
                 0.0,  0.0,  0.02, -0.1,       # prey 2
                 0.2,  0.02, 0.0,   0.0,       # predator 1; prefers prey 1
                 0.01, 0.1,  0.0,   0.0),      # predator 2; prefers prey 2
                 nrow = 4, ncol = 4, byrow=TRUE)
  )
  t0 = 0; t1 = 500; dt = 0.1
  ti = seq( t0, t1, dt)
  S = c( prey1 = 1, prey2 = 1, pred1 = 2, pred2 = 2)
  out = ode( S, ti, lotka.volterra.matrix, P, method="lsoda" ) 
  plot(out, type = "l")


  # --------------
  # Lotka-Volterra 4-species competition with logistic growth
	P = list(
    r = c(r1 = 0.1, r2 = 0.1, r3 = -0.1, r4 = -0.1),
    A = matrix(c(0.0,  0.0,  0.2,   0.01,      # prey 1
                 0.0,  0.0,  0.02,  0.1,      # prey 2
                 0.2,  0.02, 0.0,   0.0,       # predator 1; prefers prey 1
                 0.01, 0.1,  0.0,   0.0),      # predator 2; prefers prey 2
                 nrow = 4, ncol = 4, byrow=TRUE)
  )
  t0 = 0; t1 = 1000; dt = 0.1
  ti = seq( t0, t1, dt)
  S = c( prey1 = 0.1, prey2 = 0.1, pred1 = 0.2, pred2 = 0.2)
  out = ode( S, ti, lotka.volterra.matrix.logistic.growth, P, method="lsoda" ) 
  plot(out, type = "l")


  # -----------
  # Lotka Volterra with diffusion
  
  P = list( 
    rIng   = 0.2,    # /day, rate of ingestion
    rGrow  = 1.0,    # /day, growth rate of prey
    rMort  = 0.2 ,   # /day, mortality rate of predator
    assEff = 0.5,    # -, assimilation efficiency
    K      = 5  )    # mmol/m3, carrying capacity

  R  = 20                      # total length of surface, m
  N  = 50                      # number of boxes in one direction
  dx = R/N                     # thickness of each layer
  Da = 0.05                    # m2/d, dispersion coefficient

  NN = N*N                     # total number of boxes

  ## initial conditions
  S = rep(0, 2*N*N)
  cc = c((NN/2):(NN/2+1)+N/2, (NN/2):(NN/2+1)-N/2)
  S[cc] = S[NN+cc] = 1

  ## solve model (5000 state variables...
  ti = seq(0, 50, by = 1)
  out = ode.2D( S, ti, M, P,
    dimens = c(N, N), N = N, dx = dx, Da = Da, lrw = 5000000)

  ## plot results
  Col = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                            "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

  for (i in seq(1, length(ti), by = 1))
    image(matrix(nr = N, nc = N, out[i, 2:(NN+1)]),
    col = Col(100), xlab = "x", ylab = "y", zlim = range(out[,2:(NN+1)]))



 ## =======================================================================
 ## An example with a cyclic boundary condition.
 ## Diffusion in 2-D; extra flux on 2 boundaries,
 ## cyclic boundary in y
 ## =======================================================================

  dy    <- dx <- 1   # grid size
  Dy    <- Dx <- 1   # diffusion coeff, X- and Y-direction
  r     <- 0.05     # consumption rate

  nx  <- 50
  ny  <- 100
  S  <- matrix(nr=nx,nc=ny,1.)
  ST3 <- ode.2D( 
		S, times=1:100, func=lotka.volterra.2D.diffusion.cyclic.boundaries, 
		parms=NULL, dimens=c(nx,ny), verbose=TRUE,
    lrw=400000, atol=1e-10, rtol=1e-10, cyclicBnd=2
	)

 ## Not run:

   zlim <- range(ST3[,-1])
   for (i in 2:nrow(ST3)) {
     S <- matrix(nr=nx,nc=ny,data=ST3[i,-1])
     filled.contour( S, zlim=zlim, main=i)
     for (ii in 1:1000) print("asdc") 
   }

