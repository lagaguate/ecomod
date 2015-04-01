

ssa.engine.approximation.rcpp = function( p, res, rn=0 ) {

  ## load and compile supporting C/Rcpp programs
  ## -- NOTE each parallel process needs it own copy of the Rcpp functions .. 
  ##         better to make into a package for a quick load in future.
  print( "Compiling supporting methods ...")

  ssadir = project.codedirectory( "model.ssa", "src",  "_Rfunctions")
  source( file.path( ssadir, "random_deviate_exponential.rcpp" ) )
  source( file.path( ssadir, "reaction_locations.rcpp" ) )
  
  print( " ... finished." )
  print( " Running simulation ... ")
  print( " Model time     No. evaluations   Sum state variable   Sum propensity  System time")
 
  # optimized more than approximation with and some minor approximations and C-language / Rccp functions  
  # about 10 X faster than the "approximation" method

  res <- with (p, { 
     
    #  on.exit( browser())   # to debug
    on.exit( return(res) )  # in case we need to restart the sim with the last run
    tio = tout = 0  # internal time counters to trigger data output (disk saves)
    
    NU = as.vector(NU) # convert to vector as it is easier to operate with in C
    
    res$X = as.vector( res$X )
    res$P = as.vector( res$P )

    ###  next optimaization (todo) --- convert P and X to vectors and operate upon vectors rather than matricses/arrays

    nsimultaneous.picks = round( p$nrc * ssa.approx.proportion )
    tn0 = 1:nsimultaneous.picks

    while ( res$simtime <= t.end )  {

      tn = tn0
      time.increment = random_deviate_exponential( nsimultaneous.picks, res$P.total) # rcpp function to sample from an exponential distrib
      time.increment = time.increment * jump.increment # account for jumps != 1
      tnew = res$simtime + sum( time.increment )
       
      if ( tnew > tout ) {
        tcs = cumsum( time.increment )
        tn = which( tcs <= tout ) 
        time.increment = time.increment[ tn ]
        time.increment = time.increment * jump.increment # account for jumps != 1
        tnew = res$simtime + sum( time.increment )
      }

      tnlen =length(tn)

      if ( tnlen > 0 ) {
        probs = res$P[]/res$P.total
        # rcpp function to sample from a weighted uniform dist and then remap random elements to correct location and process 
        ri = reaction_locations ( nsimultaneous.picks, probs, NU, nr, nc, np ) 
       
        # update state space and associated propensities in cells where state has changed, etc
        res$X[ ri$ix ] = .Internal( pmax( na.rm=FALSE, 0, res$X[ ri$ix ] + ri$xo ) )

        # update propensities
        Pold = res$P[ ri$ip ]
        res$P[ ri$ip ] =  RE( p, X=res$X[ ri$ix ], ix=ri$ix )

        # update state space 
        res$P.total = res$P.total + sum( res$P[ ri$ip ] ) -sum( Pold )
        res$simtime = tnew
        res$nevaluations = res$nevaluations + tnlen
      }

      if ( tnew >= tout ) {
        tout = tout + t.censusinterval 
        tio = tio + 1  # time as index
        ssa.db( p=p, DS="save", out=res$X[], tio=tio, rn=rn )  
        if (monitor) {
          res$P[] =  RE( p, res$X[] ) # full refresh of propensities in case of numerical drift
          res$P.total = sum( res$P[] )
          cat( paste( tio, res$nevaluations, round(sum(res$X)), round(res$P.total), Sys.time(), sep="\t\t" ), "\n" )
          image( matrix(res$X[], nrow=nr, ncol=nc ), col=heat.colors(100)  )
          # assign( "res", res, pos=1 ) # update the res output in the gloabl environment in case a restart is necessary
        }
      }

    } # end while 
  
    res$X[] =  matrix( res$X , nrow=nr, ncol=nc )   
    res$P[] =  array( RE( p, res$X ),  dim=c( nr, nc, np ) )

    return(res)
  
  })  # end repeat and within
    
  return(res)
}


