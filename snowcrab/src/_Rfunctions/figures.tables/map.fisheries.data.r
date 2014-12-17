
  map.fisheries.data = function(p, outdir ) {
    x = logbook.db( DS="logbook" )
    x$landings = x$landings/1000  # convert kg to ton
    x$sa = 1  # this a dummy variable required by the mapping routine
    x = x [filter.region.polygon( x, region="isobath1000m"),]
    x = x[ which(x$effort <= 300) ,]
    x = x[ which(x$cpue < 500),]

    p$block = T
    p$tension = "-T0.5" # 1= harmonic surface
    p$maskres = "-S4k"
    p$interpres = "-nb"
    outdir = file.path(outdir, p$spatial.domain )
    dir.create( outdir, recursive=T, showWarnings=F  )

    variables = c("landings", "effort")
    p$blocktype="sum"
    gmt.map.variables ( x, p, variables, p$plottimes, outdir, p$conversions, init.files=p$init.files)

    variables = c("cpue")
    p$blocktype="mean"
    gmt.map.variables( x, p, variables, p$plottimes, outdir, p$conversions, init.files=p$init.files)
    return( "Done" )
  }


