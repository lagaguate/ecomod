
  map.maturity = function(p, outdir ) {
    load(file.path( project.directory("snowcrab"), "R", "maturity.rdata"))
    p$tension = "-T.4"  # 0.35+ for steep; 0.25 for smooth
    p$maskres = "-S16k"
    p$interpres = "-nb"
    p$mapres = "1min"

    for (sex in unique(maturity$sex)) {
      x = maturity[ which(maturity$sex==sex) ,]
      x$sa =  1

      variables = c("cw50")
      x = x[ is.finite(rowSums(x[, c("yr", "lon", "lat")])), ]
      basedir = "maturity"
      outdir = file.path( basedir, paste("sex",sex,sep=""),
                          paste(p$mapres, p$spatial.domain, sep=".") )
      make.maps( x, p, variables, plottimes=p$plottimes, outdir, p$conversions, init.files=p$init.files )
    }
  }


