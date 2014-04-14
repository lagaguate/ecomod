
ssa.engine.approximation.rcpp = function( p, res ) {

  # optimized more than approximation with and some minor approximations and C-language / Rccp functions  
  # about 2X faster than the "approximation" method
  res <- with (p, { 
     
    #  on.exit( browser())   # to debug
    on.exit( return(res) )  # in case we need to restart the sim with the last run
    tio = tout = 0  # internal time counters to trigger data output (disk saves)
    
    NU = as.vector(NU) # convert to vector as it is easier to operate with in C
    
    nsimultaneous.picks = round( p$nrc * ssa.approx.proportion )
    tn0 = 1:nsimultaneous.picks

    while ( res$simtime <= t.end )  {

      tn = tn0
      time.increment = random_deviate_exponential( nsimultaneous.picks, res$P.total) # rcpp function to sample from an exponential distrib
      tnew = res$simtime + sum( time.increment )
      
      if ( tnew > tout ) {
        tcs = cumsum( time.increment )
        tn = which( tcs <= tout ) 
        time.increment = time.increment[ tn ]
        tnew = res$simtime + sum( time.increment )
      }

      tnlen =length(tn)

      if ( tnlen > 0 ) {
        probs = res$P[]/res$P.total
        J = random_deviate_uniform_weighted( nsimultaneous.picks, probs )  # rcpp function to sample from a weighted uniform dist.
   
        ri = reaction_location_indices( J, NU, nr, nc ) # rcpp function to remap random element to correct location and process
        ix = cbind( ri$xr, ri$xc ) # rows and columns
        ip = cbind( ri$pr, ri$pc, ri$po ) 
       
        # update state space and associated propensities in cells where state has changed, etc
        res$X[ ix] = XX = .Internal( pmax( na.rm=FALSE, 0, res$X[ix] + ri$xo ) )

        # update propensities
        Pold = res$P[ip]
        res$P[ip] = Pnew = RE( p, X=XX, ix=ix )
        dP =  Pnew -Pold 

        # update state space and associated propensities in cells where state has changed, etc
        res$P.total = res$P.total + sum(dP) 
        res$simtime = tnew
        res$nevaluations = res$nevaluations + tnlen
      }

      if ( tnew >= tout ) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        ssa.db( p=p, DS="save", out=res$X[], tio=tio)  
        if (monitor) {
          res$P[] =  array( RE( p, res$X ),  dim=c( nr, nc, np ) ) # full refresh of propensities in case of numerical drift
          res$P.total = sum( res$P[] )
          cat( paste( tio, res$nevaluations, round(sum(res$X)), round(res$P.total), Sys.time(), sep="\t\t" ), "\n" )
          image( res$X[], col=heat.colors(100)  )
          # assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary
        }
      }

    } # end while 
    return(res)
  })  # end repeat and within
  return(res)
}


