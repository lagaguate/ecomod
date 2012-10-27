
krige.map = function( p=NULL, init.files=NULL ) {
  if (!p$do.parallel) {
    krige.map.core( p=p, init.files=init.files )
  } else  {
     
      pp = prep.parallel.run( p$clusters, p$nruns )
    clusterApplyLB( pp$cl, pp$ssplt, krige.map.core, p=p, init.files=init.files )
    stopCluster(pp$cl)
  }
}

