parallel.run.growth <- function( FUNC, export=NULL, clustertype="SOCK", ... ) {
    # used for running in parallel where output is directly to a storage device ... 
                # no need to process /reformat data .. eg.  maps, etc
      
       n = 1# detectCores()
      cl = makeCluster( n)  # SOCK works well but does not load balance as MPI 
		
      idx = clusterSplit( cl, 1:n )
      ssplt = lapply( idx, function(i) i )
      if ( !is.null(export)) clusterExport( cl, export )
      #                 clusterApplyLB( cl, ssplt, FUNC, ... )
                        clusterApply( cl, ssplt, FUNC, ... )

      stopCluster( cl )
    return ("Completed parallel run")
  }
