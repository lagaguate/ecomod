#load required packages
library(sp)
library(rgdal)
library(rgeos)
library(mapdata)
library(maptools)

#'START USER ENTRY
#'select your input and output projections here
crs.orig = "+proj=longlat +datum=WGS84" #original projection
crs.new = "+init=epsg:32620" #+init=epsg:32620=UTMZ20N   #desired projection
limits <- data.frame(X=c(-72,-53),Y=c(42,52)) #boundaries of plot
#'END USER ENTRY

## create bounding boxes - native and projected 
coordinates(limits) <- c("X", "Y")
proj4string(limits) <- CRS(crs.orig)
boundbox <- SpatialPolygons(list(Polygons(list(Polygon(cbind(
  mm=c(limits$X[2],
    seq(limits$X[2],limits$X[1],length=200),
    seq(limits$X[1],limits$X[2],length=200)),
  nn=c(limits$Y[2],
    seq(limits$Y[1],limits$Y[1],length=200),
    seq(limits$Y[2],limits$Y[2],length=200))))),
  ID="bb")), 
  proj4str=CRS(crs.orig))
boundbox_pr <- spTransform(boundbox, CRS(crs.new))

#grab data for desired areas and clip it to desired limits
map.data = maps::map(database="worldHires", 
                     regions=c("Canada","USA", "Greenland"),
                     xlim=limits$X, ylim=limits$Y, plot=F, fill=T)
IDs <- sapply(strsplit(map.data$names, ":"), function(x) x[1])
map <- map2SpatialPolygons(map.data, IDs=IDs, proj4string=CRS(crs.orig))
clip.map <- gIntersection(gBuffer(map, byid=TRUE, width=0), boundbox)

#'using the clipped data (pre-projection), capture information for the grid,
#'including information about the gridlines, as well as their labels
gridat <- gridat(clip.map, easts=seq(boundbox@"bbox"[1],boundbox@"bbox"[3],by=2), 
                 norths=seq(boundbox@"bbox"[2],boundbox@"bbox"[4],by=1))
grid <- gridlines(map, easts=seq(boundbox@"bbox"[1],boundbox@"bbox"[3],by=1), 
                  norths=seq(boundbox@"bbox"[2],boundbox@"bbox"[4],by=1))
#project all of the source data (as well as grids and grid attributes)
map_pr <- spTransform(map, CRS(crs.new))
gridat_pr <- spTransform(gridat, CRS(crs.new))
grid_pr <- spTransform(grid, CRS(crs.new))
#clip the data to match our bounidng box
clip.map_pr <- gIntersection(gBuffer(map_pr, byid=TRUE, width=0), boundbox_pr)
clip.grid_pr <- gIntersection(grid_pr, boundbox_pr)

#'make our map
#'This adds a bounding box, map, gridlines and grid labels
png(filename="SampleRMap.png",
    width = 1200, height = 1200, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png")
)
plot(boundbox_pr, border="white", add=F, lwd=1)                    #bbox first
plot(clip.map_pr, col="navajowhite2",border="navajowhite4", add=T) #data
lines(clip.grid_pr, col="grey77", lty=2)                           #gridlines
text(coordinates(gridat_pr),                                       #grid labels 
     labels=parse(text=as.character(gridat$labels)), 
     pos=gridat_pr$pos, offset=0.4, col="black", cex=1)
# # #Example bathy data
p = list( project.name = "bathymetry" )
p$project.root = project.datadirectory( p$project.name )
p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry", "polygons" ) )
p$libs = RLibrary( "rgdal", "maps", "mapdata", "maptools", "lattice", "geosphere", "sp", "raster", "colorspace" )
p = spatial.parameters( type="canada.east.highres", p=p ) 
depths = c(100, 200, 500, 1000) #, 2000, 5000 )
plygn = isobath.db( p=p, DS="isobath", depths=depths, return.lonlat=TRUE  )
#data must be clipped so it doesn't extend beyond the bounding box
clip.100 <- gIntersection(spTransform(plygn["100"], CRS(crs.new)), boundbox_pr)
clip.200 <- gIntersection(spTransform(plygn["200"], CRS(crs.new)), boundbox_pr)
clip.500 <- gIntersection(spTransform(plygn["500"], CRS(crs.new)), boundbox_pr)
clip.1000 <- gIntersection(spTransform(plygn["1000"], CRS(crs.new)), boundbox_pr)
lines(spTransform(clip.100, CRS(crs.new)), col="darkslategray3")
lines(spTransform(clip.200, CRS(crs.new)), col="deepskyblue")
lines(spTransform(clip.500, CRS(crs.new)), col="dodgerblue2")
lines(spTransform(clip.1000, CRS(crs.new)), col="darkslateblue")
plot(boundbox_pr, border="black", add=T, lwd=3)                    #dark bbox
title(main="Sample Map in R", sub="example_crs_map.R", line = -3,cex=2)
dev.off()
# #Example point data:
# pts <- data.frame(ID = 1:length(x), X = c( -67,-65,-63,-61,-59), 
#                                     Y = c( 44, 46, 48, 50, 51))
# coordinates(pts) <- c("X", "Y")
# proj4string(pts) <- CRS(crs.orig)
# points(spTransform(pts, CRS(crs.new)), pch=20, col="red")

