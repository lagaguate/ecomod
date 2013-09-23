 

----- not finished ... spatial aliasing seems to be occuring .. abandonned for now.



# Spatial prototype for Gillespie Alogrithm with parallell:
#   direct computation of everything

#   require( snow )
  require(bigmemory) 
  require(parallel)
  require(fields)
  
  cl <- makeCluster(getOption("cl.cores", 2)) # Use option cl.cores to choose an appropriate cluster size.
   
  p = list()
  p$init.files = loadfunctions( c( "model.ssa"  ) )
  p$clusters = rep( "localhost", 4 )
 
  p$dir.ssa = project.directory( "model.ssa", "data"  )
  dir.create(  p$dir.ssa,recursive=TRUE )

  p$fn.X = file.path( "ssa.X.tmp" )
  p$fn.P = file.path( "ssa.P.tmp" )

  p$fn.X.desc = file.path( "ssa.X.desc.tmp" )
  p$fn.P.desc = file.path( "ssa.P.desc.tmp" )
    

  # Using: logistic model as base
  # dX/dt = rX(1-X/K)
  # birth = b
  # death = d
  # carrying capacity = K
  # r = b-d >0 
   
  # model parameters
  p$b = 3 / 365 # birth rate 
  p$d = 2 / 365 # death rate
  p$K = 100

  # diffusion coef d=D/h^2 ; h = 1 km; per day (range from 1.8 to 43  ) ... using 10 here 
  # ... see b ulk estimation in model.lattice/src/_Rfunctions/estimate.bulk.diffusion.coefficient.r
  p$dr=10 
  p$dc=10
 
  p$n.times = 365  # number of censuses  
  p$t.end =   365   # in model time .. days
  p$t.censusinterval = p$t.end / p$n.times
 
  
  # rows are easting (x);  columns are northing (y) --- in R 
  # ... each cell has dimensions of 1 X 1 km ^2

  p$nr = 100
  p$nc = 100
  p$np = 6  # no. of processes
  
  p$nn = p$nr*p$nc


  attach (p )
    # initialize state vector
    X = big.matrix( nrow=nr, ncol=nc, type="double", init=0, backingfile=fn.X, descriptorfile=fn.X.desc, backingpath=p$dir.ssa  )  
    debug = TRUE
      if (debug) {
        rwind = floor(nr/10*4.5):floor(nr/10*5.5)
        cwind = floor(nc/10*4.5):floor(nc/10*5.5)
        X[ rwind, cwind ] = round( K * 0.8 )
        # X[,] = round( runif(nn) * K )
      }

    # initiate P the propensities .. .add columns as matrix, bigmatrix use column-priority
    P = big.matrix( nrow=nr, ncol=nc*np, type="double", init=0, backingfile=fn.P, descriptorfile=fn.P.desc, backingpath=p$dir.ssa )  
    nP = length(P)
    jr = 1:nr  # row indices do not change
    jc = 1:nc 
    for ( ip in 1:np ){
      jcP = jc+(ip-1)*nc  # increment col indices depending upon reaction process
      P[,jcP] = eval( parse(text=RE.logistic.spatial( ip) ) )  
    }
    P.total = sum(P[])
  detach(p)

  
  # Rprof()
  # system.time(  clusterApplyLB( cl, 1:ssn, ssa.blockwise, p=p, jn=jn, cc=cc, cr=cr, P.total=P.total ) )

  mindist = 5
  SS = 8 * 2  # max number of parallel subgrids involved in "one" reaction step
  simtime = itime = next.output.time = nevaluations = 0



  repeat {

    prop = .Internal(pmax(na.rm=FALSE, 0, P[]/P.total  ))   # using .Internal is not good syntax but this gives a major perfance boost > 40%
    jss = .Internal(sample( nP, size=SS, replace=FALSE, prob=prop ) )  

    # remap random element to correct location and process  ... default of matrix and big.matrix is to fill columns first
    jn  = floor( (jss-1)/p$nn ) + 1  # which reaction process
    jj = jss - (jn-1)*p$nn  # which cell 

    # focal cell coords in X
    cc = floor( (jj-1)/p$nr ) + 1
    cr = jj - (cc-1) * p$nr 
 
    # remove candidate locations that are too close to each other 
    coord = data.frame( cbind( cr, cc ) )
    dists = rdist( coord )
    dists[ lower.tri(dists, diag=TRUE) ] = 0

    good = list()
    taken = NULL
    for ( g in 1:SS ) {
      candidates = which( dists[g,] >= mindist ) 
      good[[g]] = ifelse ( length(candidates) > 0 , setdiff( sample( candidates, 1), taken), integer(0) )
      taken = unique( c(taken, good[[g]], g) ) 
    }
   
    oo = which( is.finite( unlist(good) ) )

    if (length(oo) > 0 ) {
      jss = jss[oo]
      cc = cc[oo]
      cr = cr[oo]
      jn = jn[oo]
    }
     
    ssn = length( jss )
      
    # main call to the parallelized version of the Gillespie algorithm
    ssa.block = clusterApplyLB( cl, 1:ssn, 
      fun = function( k, p, jn, cc, cr, P.total) {
        require( bigmemory)
        if (!is.null( p$init.files)) for( i in p$init.files ) source (i)
        attach(p)
          X <- attach.big.matrix( fn.X.desc, path=p$dir.ssa )
          P <- attach.big.matrix( fn.P.desc, path=p$dir.ssa )
          dPsum = 0  # changes to P.total associated with this chunk
          o = NU.logistic.spatial(jn[k]) # determine the appropriate operations for the reaction
          for( u in 1:dim(o)[1] ) {
            # update state vector (X) 
            ro = .Internal( pmin( na.rm=FALSE, nr, .Internal( pmax( na.rm=FALSE, 1, cr[k] + o[u,1] ) ) ) )
            co = .Internal( pmin( na.rm=FALSE, nc, .Internal( pmax( na.rm=FALSE, 1, cc[k] + o[u,2] ) ) ) )
            X[ro,co] = .Internal( pmax( na.rm=FALSE, 0, X[ro,co] + o[u,3] ) )
            # update propensity in focal and neigbouring cells 
            jr = .Internal( pmin( na.rm=FALSE, nr, .Internal( pmax( na.rm=FALSE, 1, ro + c(-1,0,1) ) ) ) )
            jc = .Internal( pmin( na.rm=FALSE, nc, .Internal( pmax( na.rm=FALSE, 1, co + c(-1,0,1) ) ) ) )  
            for ( iip in 1:np) {
              jcP = jc + (iip-1)*nc
              P.new = eval( parse(text=RE.logistic.spatial( iip) ) )  # this strangeness is required due to scoping rules in parallel proccessing 
              dPsum = dPsum + sum( P[jr,jcP] - P.new )
              P[jr,jcP] = P.new
            }
          }
        detach(p)
        time.increment = - (1/P.total) * log( runif( 1))   
        return( list(time.increment, dPsum ) )
      }, 
      p=p, jn=jn, cc=cc, cr=cr, P.total=P.total 
    )


    sb = colSums( matrix( unlist( ssa.block ), ncol=2, byrow=TRUE ) )
    simtime = simtime + sb[1] 
    P.total = P.total + sb[2] 

    nevaluations = nevaluations + ssn
    
    if (simtime > p$t.end ) {
      X = as.matrix( attach.big.matrix( p$fn.X.desc, path=p$dir.ssa ) )
      P = as.matrix( attach.big.matrix( p$fn.P.desc, path=p$dir.ssa ) )
      break()
    }

    if (simtime > next.output.time ) {
      X = as.matrix( attach.big.matrix( p$fn.X.desc, path=p$dir.ssa ) )
      P = as.matrix( attach.big.matrix( p$fn.P.desc, path=p$dir.ssa ) )
     
      next.output.time = next.output.time + p$t.censusinterval 
      itime = itime + 1  # time as index
      save( X, file=file.path( p$dir.ssa, paste("X", itime, "rdata", sep=".") ), compress=TRUE )
      P.total = sum(P[]) # reset P.total to prevent divergence due to floating point errors
      cat( paste( itime, round(P.total), round(sum(X[])), nevaluations, Sys.time(), sep="\t\t" ), "\n" )
      nevaluations = 0 # reset
      image( X[], col=heat.colors(100) )
    }
  }


#  Rprof(NULL)
#  plot( seq(0, p$t.end, length.out=p$n.times), out[1,1,], pch=".", col="blue", type="b" ) 
  

      file.remove( p$fn.X )
      file.remove( p$fn.P )
      file.remove( p$fn.X.desc )
      file.remove( p$fn.P.desc )
 
