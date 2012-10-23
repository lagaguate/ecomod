
  prep.parallel.run = function(clusters, nid, type="SOCK") {
    require(snow)
    if (type=="SOCK")  cl = makeCluster( spec=clusters, type=type)
    if (type=="MPI")   cl = makeCluster( spec=length(clusters), type=type)
    idx = clusterSplit(cl, 1:nid)
    ssplt = lapply( idx, function(i) i )   # subset data into lists
    return( list(cl=cl, idx=idx, ssplt=ssplt) )
  }


