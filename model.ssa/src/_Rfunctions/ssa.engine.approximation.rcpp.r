
ssa.engine.approximation.rcpp = function( p, res ) {
  # optimized and some minor approximations 
  
  on.exit( return(res) )  # in case we need to restart the sim with the last run
  #  on.exit( browser())   # to debug
  
  st0 = system.time() 
  attach(p)
    
    tio = tout = 0  # internal time counters to trigger data output (disk saves)
    
    X = as.vector( res$X )
    P = as.vector( res$P )
    P.total = res$P.total
    simtime = res$simtime 
    nevaluations = res$nevaluations 
    rm (res); gc()

    while (simtime <= t.end )  {

      tn = insp
      time.increment = random_deviate_exponential_rcpp( nsimultaneous.picks, P.total)
      tnew = simtime + sum( time.increment )
      
      if ( tnew > tout ) {
        tcs = cumsum( time.increment )
        tn = which( tcs <= tout ) 
        time.increment = time.increment[ tn ]
        tnew = simtime + sum( time.increment )
      }

      tnlen =length(tn)

      if ( tnlen > 0 ) {

        prop = P[]/P.total
        J = random_deviate_uniform_weighted_rcpp( nsimultaneous.picks, prop )  
 
        # remap random element to correct location and process
        # %/% is integer division
        # %% is modulus
        J = J - 1 # to get C-index starting at 0
        cr =  J %% nr + 1;           #    -- row no
        cc = (J%/%nr) %% nc + 1;       #    -- col no
        jn =  J %/% nrc + 1;             # -- processes np

        for ( w in tn ) {
          o = NU[[ jn[w] ]]  
          # no = dim(o)[1]  # nrows = # operations (ie, unary, or binary)
          ro = cr[w] + o[,1]  # row of the focal cell
          co = cc[w] + o[,2]  # column of the focal cell
          # OP = o[,3] # operation upon the state variable
          # ensure boundary conditions are sane (reflective boundary conditions)
          ro[ro < 1] = 2
          ro[ro > nr] = nr_1
          co[co < 1] = 2
          co[co > nc] = nc_1 
          # determine the appropriate operations for the reaction and their 'real' locations
          # build correctly structured indices 
          ix = .Internal( cbind( deparse.level=1, ro, co))   ## row, column in X .. Internal speeds up 26%
          ip = .Internal( cbind( deparse.level=1, ro, co, po[[length(ro)]] ) )  # rows and columns in P 
          # update state space and associated propensities in cells where state has changed, etc
          
          X[ix] = X[ix] + o[,3] 
          P[ip] = RE( p, X=X[ix], ix=ix ) 
        }  # end for

        P.total = sum( P )
        simtime = tnew
        nevaluations = nevaluations + tnlen
      }

      if ( tnew >= tout ) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        # print( P.total - sum(P[]) ) # debug
        ssa.db( ptype="save", out=X[], tio=tio, outdir=outdir, rn=rn )  
        # P.total = sum(P[]) # P.total in case of divergence due to floating point errors
        # browser()
        if (monitor) {
          # P = RE( p, X ) # full refresh of propensities in case of numerical drift
          cat( paste( tio, nevaluations, round(sum(X)), round(P.total), Sys.time(), sep="\t\t" ), "\n" )
          image( X[], col=heat.colors(100)  )
          # assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary
        }
      }

    } # end while 
 
    detach(p)

    return( system.time() - st0 )
  
}


