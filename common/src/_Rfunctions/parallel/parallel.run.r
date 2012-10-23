
  
  parallel.run = function( clusters, n, FUNC, export=NULL, ... ) {
    # used for running in parallel where output is directly to a storage device ... 
		# no need to process /reformat data .. eg.  maps, etc
      require(snow)

      cl = makeCluster( spec=clusters, type="SOCK" )  # works well but does not load balance 
#      cl = makeCluster( spec=length(clusters), type="MPI" )

      idx = clusterSplit( cl, 1:n )
      ssplt = lapply( idx, function(i) i )
      if ( !is.null(export)) clusterExport( cl, export )
      #			clusterApplyLB( cl, ssplt, FUNC, ... )
			clusterApply( cl, ssplt, FUNC, ... )

      stopCluster( cl )
    return ("Completed parallel run")
  }


