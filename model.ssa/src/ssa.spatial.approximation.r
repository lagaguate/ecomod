

 


  ####################   SSA 
  # Spatial prototype for Gillespie Alogrithm: approximation/optimizations



############## Model Parameters 
# Basic logistic with spatial processes  
# Using: logistic model as base
# dX/dt = rX(1-X/K)

  

  # set.seed(1)



  p = list()
  p$init = loadfunctions( c( "model.ssa", "model.pde" )  )


  p$RE = function( X, b, d, K, dr, dc, np) {
    # propensity calculations .. returns as a vector of reaction process rates ...
    c(
      b[]*X[] ,
      (d[]+(b[]-d[])*X[]/K)[]*X[] ,
      dr[]*X[] ,
      dr[]*X[] ,
      dc[]*X[] ,
      dc[]*X[] 
    )
  }
  
  
  p$NU = list (
    # Changes associated with Reaction processes 
    # Lagrangian operator structure: (row, column, operation) where 0,0 is the focal cell and the last element is the operation
    rbind( c(0,0,1) ),  # for the focal cell (0,0), the birth process: "bX"
    rbind( c(0,0,-1) ), # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
    rbind( c(0,0,-1), c(-1,0,1) ), # "jump to adjacent row from the focal cell:: X[i] -> X[i+/-1] {dr0}
    rbind( c(0,0,-1), c(+1,0,1) ),
    rbind( c(0,0,-1), c(0,-1,1) ),  # same as above but now for column-wise jumps
    rbind( c(0,0,-1), c(0,+1,1) )
  )


  p$np = length(p$NU)  # no. of processes
    
  # pre-sorted data indices ... at most there will be 2 sets as all interactions are binary
  p$po = list(
    rep(1:6), # for 1 set
    c(t(matrix( rep(1:6,2), ncol=2)))  # for 2 sets 
  )
 


  p$nr = 100  
  p$nc = 100 
  p$nrc = p$nr*p$nc
 
  # ideally only one process should be picked at a time ... 
  # but as the sampling from the propensities is the most time-expensive part of this method
  # a slightly larger number of picks are made in advance and then upadted ..
  p$nsimultaneous.picks =  round( p$nrc * 0.001 ) # 0.1% update simultaneously should be safe


  

  # pde related params
  p$eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
  p$atol = 1e-9  # atol -- absolute error tolerance for lsoda
  p$rtol = 1e-9  # rtol -- relative error tolerance for lsoda
  

  # in the stochastic form:: using a birth-death Master Equation approach 
  # birth = b
  # death = d
  # carrying capacity = K
  # r = b-d >0 
 
   
  # model parameters
  p$b = 3 / 365 # birth rate
  p$d = 2 / 365 # death rate
  p$K = 100

  p$r = p$b - p$d  ## used by pde model
  

  # diffusion coef d=D/h^2 ; h = 1 km; per year (range from 1.8 to 43  ) ... using 10 here 
  # ... see b ulk estimation in model.lattice/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r
  p$dr=10 
  p$dc=10 
  p$Da = matrix( ncol=p$nc, nrow=p$nr, data=10 ) 

  
  
  
  # model run dimensions and times
  p$n.times = 365  # number of censuses  
  p$t.end =   365   # in model time .. days
  p$t.censusinterval = p$t.end / p$n.times
  p$modeltimeoutput = seq( 0, p$t.end, length=p$n.times )  # times at which output is desired .. used by pde
 
  
  # rows are easting (x);  columns are northing (y) --- in R 
  # ... each cell has dimensions of 1 X 1 km ^2


  attach(p)
    #output storage
    out = array( 0, dim=c( nr, nc, n.times ) )
    
    # state space
    X = array( 0, dim=c( nr, nc ) ) 
    debug = TRUE
    if (debug) {
      rwind = floor(nr/10*4.5):floor(nr/10*5.5)
      cwind = floor(nc/10*4.5):floor(nc/10*5.5)
      X[ rwind, cwind ] = round( K[] * 0.8 )
    }

    # initiate P the propensities 
    # P = ff( initdata=0, dim=c( p$nr, p$nc, p$np ), filename = paste( p$fnroot, ".P.ff.tmp", sep="" ) , overwrite=TRUE, finalizer="delete" )
    P = array( 0, dim=c( nr, nc, np ) )
    for ( ip in 1:np ) P[] = RE( X[], b, d, K, dr, dc, np )  
    P.total = sum(P[])
    p$nP = length(P)
   
  detach(p)



  
  
  ####################################################
  ################# simulation engine ################
  ####################################################


  simtime = tio = tout = nevaluations = 0
  


  attach(p)
    repeat {
       
      # pre-caluclate these factor outside of the loop as they change slowly
      prop = .Internal(pmax(na.rm=FALSE, 0, P[]/P.total ) )
      J = .Internal(sample( nP, size=nsimultaneous.picks, replace=FALSE, prob=prop ) ) 
      time.increment = -(1/P.total)*log(runif( nsimultaneous.picks ) ) 
      
      for ( w in 1:nsimultaneous.picks ) {
        
        j = J[w]
   
        # remap random element to correct location and process
        jn  = floor( (j-1)/nrc ) + 1  # which reaction process
        jj = j - (jn-1)*nrc  # which cell 

        # focal cell coords
        cc = floor( (jj-1)/nr ) + 1
        cr = jj - (cc-1) * nr 

        # determine the appropriate operations for the reaction
        o = NU[[jn]] 
        no = dim(o)[1]
        
        ro = .Internal( pmin( na.rm=FALSE, nr, .Internal( pmax( na.rm=FALSE, no, cr + o[,1] ) ) ) )
        co = .Internal( pmin( na.rm=FALSE, nc, .Internal( pmax( na.rm=FALSE, no, cc + o[,2] ) ) ) )

        cox = cbind( ro, co)  ## in X
        cop = cbind( ro, co, po[[no]] )   # in P

        # update state (X) 
        X[cox] = Xcx = .Internal( pmax( na.rm=FALSE, 0, X[cox] + o[,3] ) )  # Xcx is a temp copy to skip another lookup below

        # update propensity in focal and neigbouring cells 
        dP = RE( Xcx, b, d, K, dr, dc, np )
        P.total = P.total + sum( P[cop] - dP )
        P[cop] = dP

        nevaluations = nevaluations + 1
        simtime = simtime - (1/P.total) * log( runif( 1))   # ... again to optimize for speed
        if (simtime > tout ) {
          tout = tout + t.censusinterval 
          tio = tio + 1  # time as index
          out[,,tio] = X[]
          P.total = sum(P) # reset P.total to prevent divergence due to floating point errors
          cat( paste( tio, round(P.total), round(sum(X)), nevaluations, Sys.time(), sep="\t\t" ), "\n" )
          image( X[], col=heat.colors(100)  )
        }
        if (simtime > t.end ) break()
      }  # end for
    }  # end repeat
  detach(p)



  plot( seq(0, t.end, length.out=n.times), out[1,1,], pch=".", col="blue", type="b" ) 
  


  ### ---------------------------------------
  ### Compare with a PDE version of the model 


  require (deSolve)
  require (lattice)

  A = array( 0, dim=c(p$nr, p$nc ) ) 
  debug = TRUE
  if (debug) {
    rwind = floor(p$nr/10*4.5):floor(p$nr/10*5.5)
    cwind = floor(p$nc/10*4.5):floor(p$nc/10*5.5)
    A = array( 0, dim=c(p$nr, p$nc ) ) 
    A[ rwind, cwind ] = round( p$K * 0.8 )
    # X[,] = round( runif(nrc) * K )
  }
 
  
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
 







