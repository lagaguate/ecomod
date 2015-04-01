
	
	loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
require(parallel)	
  data.location = file.path( project.datadirectory("groundfish"), "data", "2014")
  set =  groundfish.db( "set.complete" )

  variables = variable.list.expand("all")
  plottimes = c("annual")
  regions = c("4v", "4w", "4x", "4vwx", "4vw")
  season = "summer"

  if (redo.byyear) {
    set = set[ filter.season( set$julian, period=season, index=T ) , ]

    # clusters= c( "tethys", "tethys",  "tethys", "tethys" )
    byyear = ts.getdata(set, from.file=F, variables, plottimes, regions, do.parallel=T )
  # this will take ~12 hr .. try to get a parallel version running
  }
  byyear = ts.getdata(season=season)
  byyear = byyear[ which(byyear$nsets>=3) ,]

  outdir = "timeseries"
  years = sort( unique( byyear$yr ))
  xrange = range( years[years>1960] )
  xrange = xrange + c(-0.5, +0.5)

  for (pe in plottimes) {
  for (re in regions) {
  for (va in variables) {
    u = byyear[ which( byyear$variable==va & byyear$region==re & byyear$period==pe) , ]
    u$mean[ which(u$mean==0) ] = NA
    u = u[is.finite(u$mean) & is.finite(u$variance),]
    if (nrow(u) < 3) next
    s = sqrt(u$variance/u$nsets)
    eb = errbar(u$yr, u$mean, u$mean+s, u$mean-s, type="n", axes=F, xlab="Years", ylab=va, xlim=xrange )  # from Hmisc
    year.lo = u$yr
    umean = u$mean
    eb.loess = predict( loess( umean ~ year.lo, span=0.4, weights=u$nsets, degree=1),
      data.frame(year.lo=year.lo), se=T)
    lines( year.lo, eb.loess$fit, col="orange", lty="solid", lwd=4 )
    points(x=u$yr, y=u$mean, pch=10)
    axis( 1 ); axis( 2 )
    fname = paste(va, re, pe, sep=".")
    Pr (dev="png", dname=outdir, fname=fname)
  }}}



