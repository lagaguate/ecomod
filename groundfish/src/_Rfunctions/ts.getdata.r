
ts.getdata = function (sm=NULL, from.file=T, variables=NULL, plottimes=NULL, regions=NULL, do.parallel=F, fname="all", custom="normal", clusters=NULL, init.files=NULL ) {

  outfile1 = paste( "byyear", fname, "rdata", sep=".")
  outfile2 = paste( "bystrata", fname, "rdata", sep=".")

  if (custom != "normal" ) {
    outfile1 = paste( "byyear", custom, fname, "rdata", sep=".")
    outfile2 = paste( "bystrata", custom, fname, "rdata", sep=".")
  }

  if (from.file) {
    load ( file.path(R.gs, outfile1 ) )
  } else {

    bystrata = byyear = ts.yr = NULL
    nid = length(regions)

    if (!do.parallel) {
      res = get.ts.core( sm=sm, do.parallel=F, regions=regions, plottimes=plottimes, variables=variables, custom=custom , init.files=init.files)
      byyear = res$byyear
      bystrata = res$bystrata
    }
    if (do.parallel) {
      snow = prep.parallel.run(clusters, nid)
      print(snow)
      ts.snow = clusterApplyLB( snow$cl, snow$ssplt, get.ts.core, sm=sm, do.parallel=T, regions=regions, plottimes=plottimes, variables=variables, custom=custom, init.files=init.files   )
      stopCluster(snow$cl)
      byyear = bystrata = NULL
      for (m in 1:length(ts.snow)) {
        byyear = rbind( byyear, ts.snow[[m]]$byyear)
        bystrata = rbind( bystrata, ts.snow[[m]]$bystrata)
      }
    } # end parallel

    save(byyear, file=file.path(R.gs, outfile1), compress=T )
    save(bystrata, file=file.path(R.gs, outfile2), compress=T )

  } # end if from file

  return(byyear)
}


