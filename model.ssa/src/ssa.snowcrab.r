




############## Model Parameters 
# Basic logistic with spatial processes  
# Using: logistic model as base
# dX/dt = rX(1-X/K)
# Solution via SSA -- Gillespie Alogrithm:  direct computation of everything


# NOTE:: using .Internal or .Primitive is not good syntax but 
#        this gives a major perfance boost upto 40%


  # set.seed(1)

  # require(ff)  # try using disk to share data across systems
  

  p = list()

  p$init = loadfunctions( c( "model.pde", "common", "snowcrab" )  )
  
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
  
 

  ## read in PDE -related parameterizations
  p$spatial.domain = "snowcrab"  # spatial extent and data structure 
  p = model.pde.define.spatial.domain(p)

  # pde related params
  p$eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
  p$atol = 1e-9  # atol -- absolute error tolerance for lsoda
  p$rtol = 1e-9  # rtol -- relative error tolerance for lsoda


  # ideally only one process should be picked at a time ... 
  # but as the sampling from the propensities is the most time-expensive part of this method
  # a slightly larger number of picks are made in advance and then upadted ..
  p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe


  p$dir.out = project.directory( "model.ssa", "data" )
  p$fnroot = tempfile( "ssa", tmpdir=p$dir.out )
  p$outfile = paste( tempfile( "ssa", tmpdir=p$dir.out ), ".res.rdata", sep="" )



  p$nX = 10^6 # multiplier to convert to unit individuals


  # rows are easting (x);  columns are northing (y) --- in R 
  # ... each cell has dimensions of 1 X 1 km ^2

  # rate of increase per day .. for snow crab r~1 per capita per year ===> therefore per day = 1/365
  # p$modeltimes = c( 0, 5, 100, 200, 400, 800, 1600 )  # times at which output is desired
  p$modeltimeoutput = c( 0, 5, 10, 20, 21, 22, 23, 24, 25, 40, 50 )  # times at which output is desired

  p$parmeterizations = c( 
    "reaction.K.snowcrab.mature", 
    "reaction.r.constant", 
    # "diffusion.random.normal",
    "diffusion.second.order.central",
    # "diffusion.first.order.upwind",
    # "advection.random.normal",
    #"advection.random.normal",
    ""
  ) 

  p$y = 2010
  
 
  # model parameters
  p$b = matrix( nrow=p$nr, ncol=p$nc, data=3/365 ) # birth rate
  p$d = matrix( nrow=p$nr, ncol=p$nc, data=2/365 ) # death rate
  
  p$r = p$b - p$d   ### not used in SSA but must match above

  # p$K = matrix( nrow=p$nr, ncol=p$nc, data=rnorm( p$nrc, mean=p$K, sd=p$K/10) )
  p$K = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" ) 
  p$K = p$K * p$nX 
  # p$K[ p$inothabitat ] = p$eps


  iifin = which (!is.finite( p$K) ) 
  if (length(iifin)>0) p$K[iifin] = p$eps


  # abundance::
  # X = ff( initdata = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" ),
  #  dim=c( p$nr, p$nc), filename = paste( p$fnroot, ".X.ff.tmp", sep="" ) , overwrite=TRUE, finalizer="delete" )

  X = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" ) 
  X[] = X[] * p$nX

    debug = TRUE
    if (debug) X[] = round( X[] * runif( 1:length(X) ) )  




  # in the stochastic form:: using a birth-death Master Equation approach 
  # birth = b
  # death = d
  # carrying capacity = K
  # r = b-d >0 
 
  
  # diffusion coef d=D/h^2 ; h = 1 km; per year (range from 1.8 to 43  ) ... using 10 here 
  # ... see b ulk estimation in model.lattice/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r
  p$dr=matrix( ncol=p$nc, nrow=p$nr, data=10)
  p$dc=matrix( ncol=p$nc, nrow=p$nr, data=10) 
  p$Da = matrix( ncol=p$nc, nrow=p$nr, data=10 ) 
  # p$Da = matrix( ncol=p$nc, nrow=p$nr, data=rnorm( p$nrc, mean=p$Da, sd=p$Da/10 ) )
  
  
  
  # model run dimensions and times
  p$n.times = 365  # number of censuses  
  p$t.end =   365   # in model time .. days
  p$t.censusinterval = p$t.end / p$n.times
  p$modeltimeoutput = seq( 0, p$t.end, length=p$n.times )  # times at which output is desired .. used by pde
  
  # calc here to avoid repeating calculations
  p$nr_1 = p$nr-1
  p$nc_1 = p$nc-1


 
  attach(p) 
   
    out = array( 0, dim=c( nr, nc, n.times ) )
    # out = ff( initdata=0, dim=c( nr, nc, n.times ), filename = paste( fnroot, ".out.ff.tmp", sep="" ) , overwrite=TRUE, finalizer="delete" )


    # initiate P the propensities 
    # P = ff( initdata=RE( X[], b, d, K, dr, dc, np ), dim=c( nr, nc, np ), filename = paste( fnroot, ".P.ff.tmp", sep="" ) , overwrite=TRUE, finalizer="delete" )
    P = array( RE( X[], b, d, K, dr, dc, np ), dim=c( nr, nc, np ) )
    p$nP = length(P)
    P.total = sum(P[])

  detach(p)




  
  ####################################################
  ################# simulation engine ################
  ####################################################


  simtime = tio = tout = nevaluations = next.daily.process= 0
 

  attach(p)

    repeat {

      # pre-caluclate these factor outside of the loop as they change slowly
      prop = .Internal(pmax(na.rm=FALSE, 0, P[]/P.total ) )
      J = .Internal(sample( nP, size=nsimultaneous.picks, replace=FALSE, prob=prop ) ) 
      time.increment = -(1/P.total)*log(runif( nsimultaneous.picks ) ) 
    
      for ( w in 1:nsimultaneous.picks ) {
        
        j = J[w]
        # can try to parallelize this one ... but not working yet

        # remap random element to correct location and process
        jn  = floor( (j-1)/nrc ) + 1  # which reaction process
        jj = j - (jn-1)*nrc  # which cell 

        # focal cell coords
        cc = floor( (jj-1)/nr ) + 1
        cr = jj - (cc-1) * nr 

        # determine the appropriate operations for the reaction
        o = NU[[ jn ]]  
        no = dim(o)[1]
        
        ro = cr + o[,1] 
        co = cc + o[,2]
        
        # ensure boundary conditions are sane
        ro[ro < 1] = 2
        ro[ro > nr] = nr_1
        
        co[co < 1] = 2
        co[co > nc] = nc_1 
  
        cox = cbind( ro, co)  ## in X
        cop = cbind( ro, co, po[[no]] )   # in P

        # update state (X) 
        Xcx = X[cox] + o[,3]   # Xcx is a temp copy to skip another lookup below
        Xcx[Xcx<0] = 0
        X[cox] = Xcx
  
        # update propensity in focal and neigbouring cells 
        dP = RE( Xcx, b[cox], d[cox], K[cox], dr[cox], dc[cox], np )
        P.total = P.total + sum( dP - P[cop] )
        P[cop] = dP

        nevaluations = nevaluations + 1
        simtime = simtime + time.increment[w]  # ... again to optimize for speed
        if (simtime > t.end ) break()
        if (simtime > next.daily.process) {
          next.daily.process = next.daily.process + 1
          # do.fishing.activity( simetime )
        }
        if (simtime > tout ) {
          tout = tout + t.censusinterval 
          tio = tio + 1  # time as index
          out[,,tio] = X[]
          P.total = sum(P) # reset P.total to prevent divergence due to floating point errors
          cat( paste( tio, round(P.total), round(sum(X[])), nevaluations, Sys.time(), sep="\t\t" ), "\n" )
          image( X[], col=heat.colors(100)  )
        }
      } #end for
    } # end repeat
  
  detach(p) 


  res = out[]
  save( res, file=p$outfile, compress=TRUE )

 
  plot( seq(0, t.end, length.out=n.times), apply(res[], 3, mean), pch=".", col="blue", type="b" ) 












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
    # X[,] = round( runif(p$nrc) * K )
  }
 
  
  p$parmeterizations = c( "reaction", "diffusion.second.order.central") 

  
  res <- ode.2D(  times=p$modeltimeoutput, y=as.vector(A), parms=p, dimens=c(p$nr, p$nc),
    func=single.species.2D.logistic, 
    method="lsodes", lrw=1e8,  
    atol=p$atol 
  )
 

  image.default( matrix(res[365,2:10001], nrow=100), col=heat.colors(100) )

  diagnostics(res)
  
  plot(p$modeltimeoutput, apply(res, 1, sum))
  
  image(res)
  hist( res[1,] )

  select <- c(1, 4, 10, 20, 50, 100, 200, 500 )
  image(res, xlab = "x", ylab = "y", mtext = "Test", subset = select, mfrow = c(2,4), legend =  TRUE)
 







  # adding discontinuities -- e.g. fishing
    
  p$perturbation = "fishing.random"
  p$fishing.event = c( 21  )

  res <- ode.2D( times=p$modeltimeoutput, y=as.vector(A), parms=p, 
      dimens=c(p$nr, p$nc), method=rkMethod("rk45ck"), 
      func=single.species.2D.logistic,    
      events = list(func=perturbation.event, time=p$fishing.event ), 
      atol=p$atol 
  )
  




