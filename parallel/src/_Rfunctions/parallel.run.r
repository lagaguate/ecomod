
  
  parallel.run = function( FUNC, p, export=NULL, rndseed = 1, ... ) {
    # expectation of all relevant parameters in a list 'pl'
      require(parallel)
      
      res = with( p, {

        if (!exists("clusters")) {
          k = detectCores()
          clusters = "localhost"
          print( paste( "Using serial mode as no clusters were defined.", k, "cores are found on localhost, Define 'p$clusters' if you wish to run in parallel mode." ))
        }

        if (!exists("clustertype")) {
          clustertype = "SOCK"
          print( paste( "Using", clustertype, "connections as default, 'clustertype' was not defined." ))
        }
        
        if (!exists("rndseed")) {
          print( paste( "Using", rndseed, "as the default random number seed for parallel operations, Specify 'rndseed' to change." ))
        }

        if (!exists("nruns")) stop( "Must define 'nruns' in the paramater list")
        
        if ( length(clusters) == 1 ) {
          out = FUNC( p=p, ... )
        } else {
          cl = makeCluster( spec=clusters, type=clustertype ) # SOCK works well but does not load balance as MPI 
          clusterSetRNGStream(cl, iseed=rndseed )
          ssplt = lapply( clusterSplit( cl, 1:nruns ), function(i) i )
          if ( !is.null(export)) clusterExport( cl, export )
          out = clusterApplyLB( cl, ssplt, FUNC, p=p, ... )
          # clusterApply( cl, ssplt, FUNC, p=p, ... )
          stopCluster( cl )
        }
        return( out )  # return this
      })
      
    return(res)
  }


