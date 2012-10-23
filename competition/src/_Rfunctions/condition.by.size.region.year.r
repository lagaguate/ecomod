

  condition.by.size.region.year = function(loc=".", taxa="all", from.file=T,
    fname="sizecondition.rdata", base=2, vname="mass", bins=NULL,
    variables="residual", plottimes="annual", regions="4vwx", season="summer", do.parallel=F, clusters=NULL) {

    if (from.file) {
      load(file=file.path(R.gs, fname))
    } else {

    xdet = groundfish.db( "det" )
    xdet = xdet[ xdet$settype %in% c(1,2,5) ,] # remove bad sets
      #  1=stratified random, 2=regular survey, 3=unrepresentative(net damage)
      #  4=representative sp recorded(but only part of total catch),
      #  5=comparative fishing experiment, 6=tagging, 7=mesh/gear studies,
      #  8=exploratory fishing, 9=hydrography
    xdet = xdet[ , c("id", "spec", "len", "mass", "cf","residual")]

    sm = groundfish.db( "sm.base" )
    sm = sm[, c("id","strat","yr", "julian","lon", "lat" )]
    
    gsstratum = groundfish.db( DS="gsstratum" ) 
    gsstratum = gsstratum[, c("strat","area")]
    gsstratum$area = as.numeric(gsstratum$area)
    sm = merge(sm, gsstratum, by="strat", all.x=T, all.y=F, sort=F)

    xdet = merge(xdet,sm , all.x=T, all.y=F, sort=F)
    rm(sm);  gc()
    xdet = xdet[ filter.season( xdet$julian, period=season, index=T ) , ]
    xdet = xdet[ , c("id", "strat", "area", "yr", "lon", "lat", "spec", "len", "mass", "cf", "residual")]
    gc()

    xdet = xdet[ is.finite(xdet$residual), ]

    x.transf = log( xdet[,vname], base=base )
    xdet$sizeclass = cut( x.transf, breaks=bins$lb, labels=F, include.lowest=F, right=T)
    
    # closed on the right: (x,x]
    # midpoints = (l.bound [2:n.size] + l.bound [1:(n.size-1)] ) /2
    nid = nrow(bins)
    byyear = NULL

    gc()

    if (!do.parallel) {
      byyear = condition.core( id=NULL, xdet, bins, taxa, regions, plottimes, variables, do.parallel=F , init.files=init.files)
    } else if (do.parallel) {
      snow = prep.parallel.run( clusters, nid )
        condition.snow = clusterApplyLB( snow$cl, snow$ssplt, fun=condition.core, xdet, bins, taxa, regions, plottimes, variables, do.parallel=F, init.files=init.files )
      stopCluster(snow$cl)
      for (m in 1:length(condition.snow)) byyear = rbind(byyear , condition.snow[[m]])
    }

    save(byyear, file=file.path(R.gs, fname), compress=T)

    } # end if from file
    return(byyear)
  }



