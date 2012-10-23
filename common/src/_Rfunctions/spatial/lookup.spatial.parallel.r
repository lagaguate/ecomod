
  lookup.spatial.parallel = function( X, Y, do.parallel, init.files, distance.threshold.km=1, ix=1, iy=2, iz=3 ) {
    nid = nrow( X )
    pp = prep.parallel.run(p$clusters, nid)
    print(pp)
    pp.out = clusterApplyLB( pp$cl, pp$ssplt, lookup.spatial, X=X, Y=Y, do.parallel=T, init.files=init.files, distance.threshold.km=1, ix=ix, iy=iy, iz=iz )
    stopCluster(pp$cl)
    X = NULL
    for (m in 1:length(pp.out))  X = rbind( X, pp.out[[m]] )
    return ( X )
  }


