
# code used to generate stats and figures for St Anns Bank assessment
# this is the main calling program


p = list( project.name="mpa" )

p$project.outdir.root = project.datadirectory( p$project.name, "analysis" )

p$libs = RLibrary ( c(
  "lubridate", "fields", "mgcv", "sp", "parallel", "rgdal", "INLA", 
  "raster", "rasterVis", "parallel", "maps", "mapdata", "lattice"  ))

p$init.files = loadfunctions( c(
  "utility", "groundfish", "snowcrab", "bio", "biochem", "indicators", "remote.sensing", "habitat", "taxonomy",
  "bathymetry", "substrate", "temperature", "polygons", "netmensuration", "spacetime", "sorted.ordination", "stomachs", 
  "condition", "speciesarea", "speciescomposition", "metabolism", "sizespectrum", "coastline", "mpa" ))

p = spatial.parameters( p, "SSE.mpa" )  

p$default.spatial.domain = "canada.east"  # for temperature lookups
p$taxa =  "maxresolved"
p$seasons = "allseasons"
p$data.sources = c("groundfish", "snowcrab")  # for bio.db
p$nw = 10 # for lookup of temperature: number of intervals in time within a year in the temperature interpolations ( must match temperature.r 's value )

p$map.regions = c("Canada", "USA") # library "map" coastline polygon designations
p$map.output.directory = file.path( p$project.outdir.root, "maps")
p$map.palette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
p$map.depthcontours = c( 100, 200, 300, 400, 500, 600 ) # to plot on maps
p$map.depthcontours.colours = c( "gray90", "gray85", "gray80", "gray74", "gray72", "gray70" ) 

polys = mpa.db( p=p, DS="polygons.redo" ) # obtain and save a local cache of polygons of the mpa/aoi
# polys = mpa.db( p=p, DS="polygons" )


# 1. close-up map of area of interest:
pdf( file=file.path(p$project.outdir.root, "maps", "mpa_closeup.pdf") )
  figure.mpa.closeup(p )
dev.off()


# 2. map of area of interest:
pdf( file=file.path(p$project.outdir.root, "maps", "mpa.pdf") )
  figure.mpa.aoi(p )
dev.off()


pdf( file=file.path(p$project.outdir.root, "trawl.time.density.pdf") )
  ss = bio.db( DS="set" )
  dscols = c( snowcrab="green", groundfish="orange" ) 
  plot( jitter( dyear) ~ jitter(yr), ss, pch=".", col=dscols[ss$data.source], xlab="Year", ylab="Fractional year", cex=1.5 )
dev.off()


pdf( file=file.path(p$project.outdir.root, "maps", "trawl.spatial.density.pdf") )
  ss = bio.db( DS="set" )
  dscols = c( snowcrab="green", groundfish="orange" ) 
  figure.trawl.density(p=p, ss=ss, dscols=dscols )
dev.off()






#  mybreaks = classIntervals( u, n=length(mypalette), style="quantile")$brks
 
