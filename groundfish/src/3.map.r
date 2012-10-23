
	
	init.files = loadfunctions( "groundfish", functionpattern="load.groundfish.environment.r") 

	gs.datayear = 2009
	data.dir = file.path( project.directory("groundfish"), "data", gs.datayear )


# ----------------------------
# Map various data streams
# most of these stream are created in "groundfish.r"

# ----------------------------
# 1 - define plot parameters

plottimes = c("decadal",  "globalaverage")

conversions = c("ps2png", "ps2pdf")
  # choose from: conversions = c("ps2png", "ps2pdf", "ps2eps", "ps2jpg",
  #                              "eps2pdf", "png2gif", "png2mpg","png2swf")

params = NULL
params = list()
params = get.gmtparams() # default settings

params$overlay = ""
  #    choose from:
  #    params$overlay = c("cfa20", "cfa21", "cfa22", "cfa23", "cfa24", "cfa4x",
  #      "cfa24a", "cfa24b", "cfa24c", "cfa24d", "cfa24e", "cfa23a", "cfa23b", "cfa23c", "cfa23d",
  #      "cfa22inner", "cfa22outer", "cfa20inner", "cfa20outer", "cfaslope" )
  #    define others in ../common/map.functions.r

params$mapres = "2min"
params$gmt.projection.long = "Lambert.conformal.conic"
params$spatial.domain = "4vwx"
params$delete.postscript = T






# ----------------------------
# 2 - redo basemap ?
if (redo.basemap) gmt.basemap (params)


# ----------------------------
# 3 - obtain isobath coords

if (get.isobath) {  # tighter isobath-related outline of shelf at 1000m
  isobath = c(1000, 2000)
  for (i in isobath) {
    d = gmt.isobath (params, depth=i)
    d = d[,c(1,2)]
    plot(d$V1, d$V2, type="b")

    # corners
    pt1 = cbind(-56, 45)
    pt2 = cbind(-68, 42)
    pt3 = cbind(-66, 44)
    pt4 = cbind(-61, 47)
    pt5 = cbind(-60, 48)

    d = rbind(pt1, d, pt2, pt3, pt4, pt5, pt1)
    write.table(d, file.path( project.directory("polygons"), paste("isobath", i, "m.dat", sep="") ), col.names =F, row.names=F, quote=F)
}


# ----------------------------
# 4 - map data from the set-level database
# the variables being numerous are defined in a separate file which is called by
# "get.variables" in "variablelist.r"

if (map="sm") {
  dirbase = "maps"
  sm = groundfish.db( "sm.complete" )
  sm$sa = 1  # dummy required for mapping
  season = "summer"
  sm = sm[ filter.season( sm$julian, period=season, index=T ) , ]

  params$tension = "-T.4"  # 0.35+ for steep; 0.25 for smooth
  params$maskres = "-S16k"
  params$interpres = "-S16k"
  params$do.parallel = F

  variables =  get.variables("all")
  outdir = paste(dirbase, params$mapres, params$spatial.domain, season, sep=".")
  make.maps( sm, params, variables, plottimes, outdir, conversions, init.files=init.files, db="groundfish" )

}


# ----------------------------
# 11 - map maturity information (trawl data)

if (map="maturity") {
  #load(file.path(R.sc, "maturity.rdata"))
  params$tension = "-T.4"  # 0.35+ for steep; 0.25 for smooth
  params$maskres = "-S16k"
  params$interpres = "-S16k"

  plottimes=c("annual", "annualsmoothed", "globalaverage")

  for (sex in unique(maturity$sex)) {
    x = maturity[ which(maturity$sex==sex) ,]
    x$sa =  1

    variables = c("cw50")
    x = x[ is.finite(rowSums(x[, c("yr", "lon", "lat")])), ]
    basedir = "maturity"
    outdir = file.path( basedir, paste("sex",sex,sep=""),
                        paste(params$mapres, params$spatial.domain, sep=".") )
    make.maps( x, params, variables, plottimes=plottimes, outdir, conversions, init.files=init.files, db="groundfish")
  }
}


# if (params$delete.postscript) cmd('find', params$basedir, '-iname "*.ps" | xargs rm -f')


