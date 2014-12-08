
  map.fisheries.data = function(p, outdir ) {
    x = logbook.db( DS="logbook" )
    x$landings = x$landings/1000  # convert kg to ton
    x$sa = 1  # this a dummy variable required by the mapping routine
    x = x [filter.region.polygon( x, region="isobath1000m"),]
    x = x[ which(x$effort <= 300) ,]
    x = x[ which(x$cpue < 500),]

    p$mapres = "1min"
    p = gmt.resolution(p) # refresh due to change in mapres
    
    p$block = T
    
    p$tension = "-T0.5" # 1= harmonic surface
    p$maskres = "-S4k"
    p$interpres = "-nb"
    outdir = file.path(outdir, paste(p$mapres, p$spatial.domain, sep=".") )
    dir.create( outdir, recursive=T, showWarnings=F  )

    variables = c("landings", "effort")
    p$blocktype="sum"
    make.maps( x, p, variables, p$plottimes, outdir, p$conversions, init.files=p$init.files)

    variables = c("cpue")
    p$blocktype="mean"
    make.maps( x, p, variables, p$plottimes, outdir, p$conversions, init.files=p$init.files)
    return( "Done" )
  }


