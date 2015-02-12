
get.ts.core = function(id=NULL, set, do.parallel=T, regions, plottimes, variables, custom, init.files=NULL ) {

	  #source( "/home/jae/.Rprofile" )
	loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
	
  # the first index is a list that is passed from the calling prog: in this case "ssplt" (if parallel)
  if (do.parallel) id = as.numeric(id)
  if (!do.parallel) id = c(1:length(regions))

  nid = length(id)
  byyear = bystrata = NULL
  for (regs in id) {
    re = regions[regs]
    uu = switch( custom,
           normal = set[ filter.region.polygon(set[,c("lon", "lat")], re) , ] ,
           ca = set[ which(set$ca.quant==re) , ]
         )
    out = NULL
    for (ti in plottimes) {
      td = recode.time( uu, ti )
      tmp = NULL
      for (iy in sort(unique( td$yr ))) {
        vv = uu[ which(td$yr==iy) ,]
        for (va in variables) {
          
          if (! (va %in% colnames(vv)) ) next()
          
          print( paste(va, iy))
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
    }}}
  } # end regions
  return( list(byyear=byyear, bystrata=bystrata ) )
}


