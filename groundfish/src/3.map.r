
	
	init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

	gs.datayear = 2009
	data.dir = file.path( project.datadirectory("groundfish"), "data", gs.datayear )


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

params$overlay = ""
  #    choose from:
  #    params$overlay = c("cfa20", "cfa21", "cfa22", "cfa23", "cfa24", "cfa4x",
  #      "cfa24a", "cfa24b", "cfa24c", "cfa24d", "cfa24e", "cfa23a", "cfa23b", "cfa23c", "cfa23d",
  #      "cfa22inner", "cfa22outer", "cfa20inner", "cfa20outer", "cfaslope" )

params$spatial.domain = "4vwx"
params = gmt.parameters( params ) # default settings

params$delete.postscript = T


# ----------------------------
# 2 - redo basemap ?
if (redo.basemap) {
  # do not redo unless you are really sure you want to do this .. .matching resolutions etc is a pain 
  params$isobaths_toplot = c( 50, 100, 150, 200, 250, 300, 350, 400 )
  gmt.basemap (params)
}

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
    write.table(d, file.path( project.datadirectory("polygons"), paste("isobath", i, "m.dat", sep="") ), col.names =F, row.names=F, quote=F)
}


# ----------------------------
# 4 - map data from the set-level database
# the variables being numerous are defined in a separate file which is called by
# "variable.list.expand" in "variablelist.r"

if (map="set") {
  dirbase = "maps"
  set = groundfish.db( "set.complete" )
  set$sa = 1  # dummy required for mapping
  season = "summer"
  set = set[ filter.season( set$julian, period=season, index=T ) , ]

  params$tension = "-T.4"  # 0.35+ for steep; 0.25 for smooth
  params$maskres = "-S16k"
  params$interpres = "-n16k"
  params$do.parallel = F

  variables =  variable.list.expand("all")
  outdir = paste(dirbase, params$spatial.domain, season, sep=".")
  gmt.map.variables( set, params, variables, plottimes, outdir, conversions, init.files=init.files, db="groundfish" )

}


# ----------------------------
# 11 - map maturity information (trawl data)

if (map="maturity") {
  #load(file.path(R.sc, "maturity.rdata"))
  params$tension = "-T.4"  # 0.35+ for steep; 0.25 for smooth
  params$maskres = "-S16k"
  params$interpres = "-n16k"

  plottimes=c("annual", "annualsmoothed", "globalaverage")

  for (sex in unique(maturity$sex)) {
    x = maturity[ which(maturity$sex==sex) ,]
    x$sa =  1
    variables = c("cw50")
    x = x[ is.finite(rowSums(x[, c("yr", "lon", "lat")])), ]
    basedir = "maturity"
    outdir = file.path( basedir, paste("sex",sex,sep=""), params$spatial.domain )
    gmt.map.variables ( x, params, variables, plottimes=plottimes, outdir, conversions, init.files=init.files, db="groundfish")
  }
}


# if (params$delete.postscript) cmd('find', params$basedir, '-iname "*.ps" | xargs rm -f')


