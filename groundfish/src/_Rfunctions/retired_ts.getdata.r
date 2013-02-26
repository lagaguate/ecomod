
archive.ts.getdata = function (sm=NULL, from.file=T, variables=NULL, plottimes=NULL, regions=NULL, do.parallel=F, fname="all", custom="normal" ) {

  outfile1 = paste( "byyear", fname, "rdata", sep=".")
  outfile2 = paste( "bystrata", fname, "rdata", sep=".")

  if (custom != "normal" ) {
    outfile1 = paste( "byyear", custom, fname, "rdata", sep=".")
    outfile2 = paste( "bystrata", custom, fname, "rdata", sep=".")
  }

  if (from.file) {
    load ( outfile1 )
  } else {

    bystrata = byyear = NULL

    if (!do.parallel) {

      for (re in regions) {

        uu = switch( custom,
               normal = sm[ filter.region.polygon(sm, re) , ] ,
               ca = sm[ which(sm$ca.quant==re) , ]
             )

        for (ti in plottimes) {
          td = recode.time( uu, ti, vector=T )
          tmp = NULL
          for (iy in sort(unique( td$yr ))) {
            vv = uu[ which(td$yr==iy) ,]
            for (va in variables) {
              if (! (va %in% colnames(vv)) ) next
              vars.to.extract = c("yr", "strat", "area", va)
              ww = vv[, vars.to.extract]
              ww = ww[ is.finite(ww$yr) ,]
              ww = ww[ is.finite(ww[,va]), ]
              if (nrow(ww) == 0) next
              ww[,va] = variable.recode( ww[,va] , va, db="groundfish")
              wm = means.strata(v=ww[,va], strata=ww$strat, w=ww$area)
              if (is.null(wm)) next
              wm$yr = iy
              wm$variable = va
              wm$region = re
              wm$period = ti
              ts2 = ts.collapse( wm )
              ts2$nsets = length( is.finite(ww[,va]) )
              byyear = rbind( byyear, ts2 )
              bystrata = rbind( bystrata, wm )
            }
          }
        } # end plottimes
      } # end regions
    } # end parallel

    save(byyear, file=outfile1, compress=T )
    save(bystrata, file=outfile2, compress=T )

  } # end if from file

  return(byyear)
}


