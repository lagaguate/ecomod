




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
  p$lib = loadlibraries( c("parallel", "Rcpp", "RcppArmadillo"))
  p$init = loadfunctions( c( "model.ssa", "model.pde", "common", "snowcrab" )  )
  


  # ----------------------------
  # System size definitions
  # pde related params already define the snow crab data 
  # rows are easting (x);  columns are northing (y) --- in R 
  # ... each cell has dimensions of 1 X 1 km ^2
  p$spatial.domain = "snowcrab"  # spatial extent and data structure 
  p = model.pde.define.spatial.domain(p)
  

  
  # ----------------------------
  # Additional parameters and some calculations here to avoid repeating within the simulation loop
  # p$increment   = 5 /100 * 5  # 5 = approx max density t / km^2; /100 so ~ 1 percentage min dX value to use at the lowest level for rate processes; *5 -> ~ 5% 
  p$increment   = 100L
  p$eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
  p$atol = 1e-9  # atol -- absolute error tolerance for lsoda
  p$rtol = 1e-9  # rtol -- relative error tolerance for lsoda
  p$nr_1 = p$nr-1
  p$nc_1 = p$nc-1


 

  # ----------------------------
  # Time dimensions and constraints
  p <- within( p, { 
    n.times = 100  # number of censuses  
    t.end =   10   # in model time .. days
    t.censusinterval = t.end / n.times
    modeltimeoutput = seq( 0, t.end, length=n.times )  # times at which output is desired .. used by pde
    #modeltimeoutput = c( 0, 5, 10, 20, 21, 22, 23, 24, 25, 40, 50 )  # times at which output is desired
  })


  

  # ----------------------------
  # Model definitions
  
  choose.model = TRUE
  if (choose.model) {
    stop( "stop and choose a model" )
    p = ssa.model.definition( p, ptype = "logistic", increment=p$increment ) 
    p = ssa.model.definition( p, ptype = "logistic.randomwalk", increment=p$increment ) 
    p = ssa.model.definition( p, ptype = "logistic.correlated.randomwalk", increment=p$increment ) 
  }


  # ----------------------------
  # Biological rates parameterizations
  # rate of increase per day .. for snow crab r~1 per capita per year ===> therefore per day = 1/365
  # in the stochastic form:: using a birth-death Master Equation approach 
  # birth = b
  # death = d
  # carrying capacity = K
  # r = b-d >0 
 
  
  # diffusion coef d=D/h^2 ; h = 1 km; per year (range from 1.8 to 43  ) ... using 10 here 
  # ... see b ulk estimation in model.lattice/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r

  p$y = 2011  # currently picking a single year for data streams ... must make this more general
  
  p <- within(p, {
    parmeterizations = c( 
        "reaction.K.snowcrab.mature", 
        "reaction.r.constant", 
        # "diffusion.random.normal",
        # "diffusion.second.order.central",
        # "diffusion.first.order.upwind",
        "advection.fixed",
        #"advection.random.normal",
        "")
    b = matrix( nrow=nr, ncol=nc, data=3/365 ) # birth rate
    d = matrix( nrow=nr, ncol=nc, data=2/365 ) # death rate
    r = b-d # not used in SSA but must match above for PDE
    K = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" )
    # K = matrix(K, sparse=TRUE)

    # K = matrix( nrow=p$nr, ncol=p$nc, data=rnorm( p$nrc, mean=p$K, sd=p$K/10) )
    # K[ inothabitat ] = eps
   
    K = K *10^3  # convert t to kg ;; ### Assume 1 individual ~ 1 kg --> K is in t/km^2 --> x 1000
    K [ K<= eps] = 0 

    randomstep = TRUE
    if (randomstep) {
      
      move_velocity = function(n=1){
        # for movement parameterization see: 
        # /home/jae/ecomod/model.pde/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r
        # move_median = 0.06907182 # km/day
        # move_geomean =  -2.729117 # log(km/day)
        # move_geosd = 1.282633 #log scale
        rlnorm( n, meanlog= -2.729117, sdlog=1.282633) #return on the normal scale
      }
      move_velocity = exp(-2.729117)  # km/day

      # RRM1_2 = "relative risk" of moving from 2 -> 1 
      #        ~ proportional to RRM1_2 = [ Pr( observation in 1) / Pr (observation in 2)  ]
      # then, no. moving from 2 -> 1        
      #        ~ RRM1_2 * (sampled move velocities per day~0.07 km/day) * no. individuals  
      
      # load Habitat probability and compute direction-specific "relative risks"
      rrr = rep(1, nc)  # row vector of 1's
      ccc = rep(1, nr)  
      H = model.pde.external.db( p=p, method="snowcrab.male.mature" , variable="habitat.mean" )
      H[ H<eps ] = eps
      Hr = H[1:(nr-1),] /  H[2:nr,] 
     
      # a ratio of two probabilities should have ~ normal distribution (?? source ??)
      # using a quantile-based truncation at p=0.025, and p=0.975 
      Qr = quantile( Hr, probs=c(0.025, 0.975), na.rm=T )
      Hr[ Hr < Qr[1] ] = Qr[1] 
      Hr[ Hr > Qr[2] ] = Qr[2] 
      
      Hr0 = rbind( rrr, Hr )  # hazzard ratio of up moving across rows in the negative direction
      Hr1 = rbind( 1/Hr, rrr ) # down positive
      
      Hc = H[,1:(p$nc-1)] /  H[,2:p$nc]
      Qc = quantile( Hc, probs=c(0.025, 0.975), na.rm=T )
      Hc[ Hc < Qc[1] ] = Qc[1] 
      Hc[ Hc > Qc[2] ] = Qc[2] 
       
      Hc0 = cbind( ccc, Hc) # hazzard ratio of Pr of moving in negative direction
      Hc1 = cbind( 1/Hc, ccc ) #  positive direction

      # scale probability ratios to magitude of RMS velocities
      Hr0 = Hr0 * move_velocity
      Hr1 = Hr1 * move_velocity
      Hc0 = Hc0 * move_velocity
      Hc1 = Hc1 * move_velocity

      H = Hc = Hr = rrr = ccc = NULL
    }

    Da = 0.5  # diffusion coefficient median km^2/day  
    
    })



  iifin = which (!is.finite( p$K) ) 
  if (length(iifin)>0) p$K[iifin] = p$eps


  # state variable 
  res = with( p, {
    # initiate state space with some random noise and a core area in the center of the system
    X = model.pde.external.db( p=p, method="snowcrab.male.mature", variable="abundance.mean" )
    X = X * 10^3
    X [ X<= eps] = 0 
# convert to integer data -- 0.5 to 1 kg ~ 1 crab fully mature; X is in t/km^2; X1000 to convert to individuals/km^2
    # X = matrix(X, sparse=TRUE)

    # add some random noise and a core area in the center of the system
    debug = FALSE
    if( debug) { 
      rwind = floor(nr/10*4.5):floor(nr/10*5.5)
      cwind = floor(nc/10*4.5):floor(nc/10*5.5)
      X[ rwind, cwind ] = round( X[ rwind, cwind ] * runif( length(X[ rwind, cwind ]) ) )
    }  
    
    X = X * runif( length(X), min=0.75, max=0.95 )  
    
    # initiate P the propensities 
    P = array( RE(p, as.matrix( X) ),  dim=c( nr, nc, np ) ) 
    P.total = sum( P[] )
    simtime = 0       # time in units of the simulation (days)
    nevaluations = 0  # used for debugging and counting evaluations to estimate computational speed ...
    return( list( X=X, P=P, P.total=P.total, simtime=simtime, nevaluations=nevaluations ) )
  }) 

 

  if (ssa.method == "exact" ) {
    p$rn = 0  # default if using single runs ... otherwise, with multiple runs, the run numebers are generated automatically 
    p$runname = "snowcrab.exact"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    p$monitor = TRUE
    res = ssa.engine.exact( p, res )  # same as  ssa.engine.approximation right now ... but if additional changes such as fishing etc ... then it should be done in the snowcrab version
    # takes about 500 MB per run
  }


  if (ssa.method == "approximation" ) {
    p$rn = 0  # default if using single runs ... otherwise, with multiple runs, the run numebers are generated automatically 
    p$runname = "snowcrab.approximation"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    p$nsimultaneous.picks =  round( p$nrc * 0.1 ) # 0.1% update simultaneously should be safe
    p$monitor = TRUE
    
    use.jit = TRUE
    if (use.jit) {
      require(compiler)
      enableJIT(3)
      p$RE = cmpfun( p$RE )
      # ssa.engine.approximation = cmpfun(ssa.engine.approximation) 
      ssa.engine.approximation.cpp = cmpfun(ssa.engine.approximation.cpp) 
    }

    # res = ssa.engine.approximation ( p, res ) 
    res = ssa.engine.approximation.cpp ( p, res ) 

    #  ... but if additional changes such as fishing etc ... then a new engine should be created
    # takes about 800 MB per run
    # X = ssa.db( ptype="load", outdir=p$outdir, tio=10, rn=p$rn )  
    # image(X)
  }



  if (ssa.method == "approximation.parallel" ) {
    # use parallel mode to run multiple simulations is the most efficient use of resources 
    # wrapper is "ssa.parallel" (below)
    p$runname = "snowcrab.approximation.parallel"
    p$outdir = project.directory( "model.ssa", "data", p$runname )
    
    p$libs = loadlibraries(  "snow", "parallel" , "rlecuyer" )
    
    p$monitor = FALSE # not possible with parallel mode
   
    p$cluster = c( rep("tethys", 7), rep( "kaos", 23), rep("nyx", 24), rep( "tartarus", 24) ) 
    p$cluster = rep( "localhost", 5 )
    p$cluster.message.system = "SOCK" 
    #p$cluster.message.system = "PSOCK"

    p$ssa.engine = ssa.engine.approximation.snowcrab
 
    p$nsimultaneous.picks =  round( p$nrc * 0.01 ) # 0.1% update simultaneously should be safe
    p$nruns = 5
  
    ssa.parallel.run ( DS="run", p=p, res=res  ) # run the simulation in parallel
    ssa.parallel.run ( DS="post.process", p=p  ) # postprocess the simulations gathering a few statistics

    # load some of the run results
    X = ssa.parallel.run ( DS="load", p=p, run=1 )  # to load run 1 (to debug) 
    X = ssa.parallel.run ( DS="load", p=p, run="median" ) # etc. .. "mean", "var", "min", "max" ... add as needed.
  
    # visualize data
    image(X[,,1])

    # delete raw simulation files 
    delete = FALSE
    if (delete) ssa.parallel.run ( DS="delete.individual.runs", p=p  ) 
  }




  ### ---------------------------------------
  ### Compare with a PDE version of the model 


  require (deSolve)
  require (lattice)

  # update with 
  A = ssa.parallel.run ( DS="load", p=p, run=1 ) # to load run 1 (to debug) 
  
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
 

  plot( seq(0, t.end, length.out=n.times), apply(out[], 3, mean), pch=".", col="blue", type="b" ) 


  # adding discontinuities -- e.g. fishing
  p$perturbation = "fishing.random"
  p$fishing.event = c( 21  )

  res <- ode.2D( times=p$modeltimeoutput, y=as.vector(A), parms=p, 
      dimens=c(p$nr, p$nc), method=rkMethod("rk45ck"), 
      func=single.species.2D.logistic,    
      events = list(func=perturbation.event, time=p$fishing.event ), 
      atol=p$atol 
  )
  




