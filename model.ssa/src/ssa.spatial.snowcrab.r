




############## Model Parameters 
# Basic logistic with spatial processes  
# Using: logistic model as base
# dX/dt = rX(1-X/K)
# Solution via SSA -- Gillespie Alogrithm:  direct computation of everything


# NOTE:: using .Internal or .Primitive is not good syntax but 
#        this gives a major perfance boost upto 40%


  # set.seed(1)

  # require(ff)  # try using disk to share data across systems
  

  # Reaction processes ...
  RE = list(
    "b[jr,jc]*X[jr,jc]" ,  # birth
    "(d[jr,jc]+(b[jr,jc]-d[jr,jc])*X[jr,jc]/K[jr,jc])*X[jr,jc]" ,   #  death
    "dr[jr,jc]*X[jr,jc]" ,   # in balance equation: X[i] <-> X[i+1] :: therefore, this is X[i] -> X[i+1] {dr0}
    "dr[jr,jc]*X[jr,jc]" ,   # and this is the coupled:  X[i+1] -> X[i] {dr1}
    "dc[jr,jc]*X[jr,jc]" ,   #diffusion
    "dc[jr,jc]*X[jr,jc]"  
  )
  
  NU = list( 
    rbind( c(0,0,1) ),  # for the focal cell (0,0), the birth process: "bX"
    rbind( c(0,0,-1) ) , # for the focal cell (0,0), the death process: "(d+(b-d)*X/K)*X""
    rbind( c(0,0,-1), c(-1,0,1) ), # "jump to adjacent row from the focal cell:: X[i] -> X[i+/-1] {dr0}
    rbind( c(0,0,-1), c(+1,0,1) ),
    rbind( c(0,0,-1), c(0,-1,1) ),  # same as above but now for column-wise jumps
    rbind( c(0,0,-1), c(0,+1,1) )
  )




  p = list()

  p$dir.out = project.directory( "model.ssa", "data" )
  p$fnroot = tempfile( "ssa", tmpdir=p$dir.out )
  p$outfile = paste( tempfile( "ssa", tmpdir=p$dir.out ), ".res.rdata", sep="" )



  p$init = loadfunctions( c( "model.ssa", "model.pde", "common", "snowcrab" )  )
  
  p$np = 6  # no. of processes
  
  
  # pde related params
  p$eps  = 1e-6   # A in units of t/km^2 -- number below which abundance can be considered zero ~ 1 kg/ km^2 = 1g / m^2
  p$atol = 1e-9  # atol -- absolute error tolerance for lsoda
  p$rtol = 1e-9  # rtol -- relative error tolerance for lsoda
  


  ## read in PDE -related parameterizations
  p$spatial.domain = "snowcrab"  # spatial extent and data structure 
  p = model.pde.define.spatial.domain(p)
  


  # ideally only one process should be picked at a time ... 
  # but as the sampling from the propensities is the most time-expensive part of this method
  # a slightly larger number of picks are made in advance and then upadted ..
  p$nsimultaneous.picks =  round( p$nrc * 0.001 ) # 0.1% update simultaneously should be safe

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
  X[] = round( X[] * runif( 1:length(X) ) )  

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
 

  out = array( 0, dim=c( p$nr, p$nc, p$n.times ) )
  # out = ff( initdata=0, dim=c( p$nr, p$nc, p$n.times ), filename = paste( p$fnroot, ".out.ff.tmp", sep="" ) , overwrite=TRUE, finalizer="delete" )

  
  nevaluations = 0
  simtime = 0
  itime = 0
  next.output.time = 0
  next.daily.process = 0
 


  attach(p) 
  
    # initiate P the propensities 
    # P = ff( initdata=0, dim=c( p$nr, p$nc, p$np ), filename = paste( p$fnroot, ".P.ff.tmp", sep="" ) , overwrite=TRUE, finalizer="delete" )
    P = array( 0, dim=c( nr, nc, np ) )
    nP = length(P)
    jr = 1:nr
    jc = 1:nc
    for ( ip in 1:np ) {
      dyn = RE [[ ip ]]
      P[,,ip] = eval( .Internal( parse(file=stdin(), n=NULL, text=dyn, promt="?", srcfile="NULL", encoding="unknown" )) )
    }
 

#   Rprof()
 

    repeat {
 #     if ( nevaluations > 10*nsimultaneous.picks) break()   # for profiling/debugging

      # pre-caluclate these factor outside of the loop as they change slowly
      P.total = sum(P[])
      prop = P[]/P.total
      J = .Internal(sample( nP, size=nsimultaneous.picks, replace=FALSE, prob=.Internal(pmax(na.rm=FALSE, 0, prop  )) ) )
      time.increment = -(1/P.total)*log(runif( nsimultaneous.picks ) ) 
      
      print( P.total )

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
        ro = .Internal( pmin( na.rm=FALSE, nr, .Internal( pmax( na.rm=FALSE, no, cr + o[,1] ) ) ) )
        co = .Internal( pmin( na.rm=FALSE, nc, .Internal( pmax( na.rm=FALSE, no, cc + o[,2] ) ) ) )
    
        # update state (X) 
        coord = cbind(ro,co)
        X[coord] = .Internal( pmax( na.rm=FALSE, 0, X[coord] + o[,3] ) )
       
        # update propensity (P) in focal and neigbouring cells 
        for( u in 1:no ) {
          jr = .Internal( pmin( na.rm=FALSE, nr, .Internal( pmax( na.rm=FALSE, 1, ro[u] + c(-1,0,1) ) ) ) )
          jc = .Internal( pmin( na.rm=FALSE, nc, .Internal( pmax( na.rm=FALSE, 1, co[u] + c(-1,0,1) ) ) ) )

          for ( iip in 1:np) {
            dyn = RE[[ iip ]]
            P[jr,jc,iip]  = .Internal( eval( 
              .Internal( parse( 
                file=stdin(), n=NULL, text=dyn, promt="?", srcfile="NULL", encoding="unknown" )), 
              parent.frame(), enclos=baseenv() ))
          }
        }

        nevaluations = nevaluations + 1
         
        simtime = simtime + time.increment[w]  # ... again to optimize for speed
        
        if (simtime > next.daily.process) {
          next.daily.process = next.daily.process + 1
          # do.fishing.activity( simetime )
        }

        if (simtime > next.output.time ) {
          next.output.time = next.output.time + t.censusinterval 
          itime = itime + 1  # time as index
          out[,,itime] = X[]
          # cat( paste( itime, round(P.total), round(sum(X)), Sys.time(), sep="\t\t" ), "\n" )
          cat( paste( itime, round(P.total), round(sum(X[])), nevaluations, Sys.time(), sep="\t\t" ), "\n" )
          # nevaluations = 0 # reset
          image( X[], col=heat.colors(100)  )
        }
      
        if (simtime > t.end ) break()
      
      } #end for

    } # end repeat

  
  #    Rprof(NULL)
  #  summaryRprof()


  detach(p) 


  res = out[]
  save( res, file=p$outfile, compress=TRUE )

  plot( seq(0, t.end, length.out=n.times), res[1,1,], pch=".", col="blue", type="b" ) 
  











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
  




