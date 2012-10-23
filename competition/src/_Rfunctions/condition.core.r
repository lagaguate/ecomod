
condition.core = function(id=NULL, xdet, bins, taxa, regions, plottimes, variables, do.parallel,  init.files ) {
  
    if (!is.null( init.files)) for (i in init.files) source( i )

  # the first index is a list that is passed from the calling prog: in this case "ssplt" (if parallel)
  if (do.parallel) id = as.numeric(id)
  if (!do.parallel) id = c(1:nrow(bins))

  byyear = NULL

  for (isc in 1:length(id)) {
    sc = id[isc]
    print(sc)
    i.xsc = which(xdet$sizeclass==sc)

    for (tx in taxa) {
      i.smtxi = filter.taxa( x=xdet$spec, method=tx )
      i.x = sort(intersect(i.xsc, i.smtxi))
      if (length(i.x) < 30) next
      xdet.tx = xdet[i.x,]
      for (re in regions) {
        uu = NULL
        uu = xdet.tx[ filter.region.polygon(x=xdet.tx[ , c("lon","lat")], region=re) , ]
        for (ti in plottimes) {
          td = NULL
          td = recode.time( uu$yr, ti, vector=T )
          for (iy in sort(unique( td ))) {
            vv = uu[ which(td==iy) ,]
            for (va in variables) {
              if (! (va %in% colnames(vv)) ) next
              wm = NULL
              for ( us in sort(unique(vv$strat))) {
                gc()
                i.strat = which(vv$strat==us)
                xx = vv[i.strat,c(va,"cf")]
                mean = wtd.mean(xx[,va], xx[,"cf"], normwt=T, na.rm=T)
                variance = wtd.var(xx[,va], xx[,"cf"], normwt=T, na.rm=T) # from Hmisc
                nind = length(is.finite(xx[,va]))
                out = NULL
                out = data.frame( mean=mean, variance=variance, nind=nind, strat=us )
                wm = rbind( wm, out )
              }

              wm$sumwgt = wm$area
              wm = wm[ is.finite(wm[,"mean"]), ]
              if (is.null(wm)) next
              if (nrow(wm) == 0) next
              ts2 = NULL
              if (nrow(wm) == 1) {
                ts2=wm[, c("mean", "variance", "nind")]
              }
              if (nrow(wm) > 1) {
                wmmean = wtd.mean(wm$mean, wm$sumwgt, normwt=T, na.rm=T)
                wmvariance = wtd.var(wm$mean, wm$sumwgt, normwt=T, na.rm=T) # from Hmisc
                wmnind = sum(wm$nind, na.rm=T)
                ts2 = data.frame( mean=wmmean, variance=wmvariance, nind=wmnind )
              }
              nsets = length(is.finite(wm$mean))
              ts2 = cbind(ts2, data.frame( nsets=nsets, yr=iy, variable=va, region=re, period=ti, sizeclass=sc, taxa=tx ) )
              byyear = rbind( byyear, ts2 )

          } #end variables
        } #end year
      } # end plottimes
    } # end regions
  } # end taxa
  } # end size class

  return(byyear)
}


