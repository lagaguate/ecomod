# If reqd, load ecomod functions ------------------------------------------
if (F) {
  loadfunctions("utility/src/_Rfunctions/colours")
  #loadfunctions("spacetime", functionname="grid.internal.r")
}

# Get rid of stuff from previous runs -------------------------------------
rm(pts)
rm(limits)
rm(boundbox)
rm(poly_grd)
rm(basemap)

# Load the packages -------------------------------------------------------
library(sp)
library(Grid2Polygons)
library(mapdata)  #for getting basemapobjects
library(maptools) #for converting basemap lines to polygons

# User Parameters ---------------------------------------------------------
limits = data.frame(X=c(-68,-55),Y=c(42,50)) #boundaries of plot
gridres = 1 #for now, limits should be divisible by gridres
crs.orig = "+proj=longlat +datum=WGS84" #initial projection of all data
crs.new = "+proj=utm +zone=20 +datum=WGS84"
use.groundfish=F

# Load the desired point data (from MARFIS) -------------------------------
setwd("C:/Users/mcmahonm/Documents/Assistance/ChoiJ/20150209")
if (use.groundfish==T){
pts = read.csv2("201502_GroundfishTest.csv")
}else{
pts = read.csv("201502_MarfissciTest.csv")
}
req.fields = c("LAT","LON","LOG_EFRT_STD_INFO_ID")

# #'Also tried groundfish data - for convenience, rename columns as necessary and 
# #'drop sdate
# pts=groundfish.track.selector()[[1]]
# names(pts)[names(pts)=="LONG"]="LON"
# pts$LOG_EFRT_STD_INFO_ID <- 1:nrow(pts) 
# pts$SDATE=NULL

# Create bounding boxes, make the grid, and convert it to polygons---------
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

#'from http://tinyurl.com/jjz4p3t
cs <- c(gridres, gridres) #cellsize = 1deg*1deg
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(boundbox@bbox))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(boundbox)))
poly_grd=Grid2Polygons(sp_grd)

# Prepare point data for spatial analyses ---------------------------------
#'remove rows with NAs in any required fields
pts = data.frame(pts[complete.cases(pts[req.fields]),])  
coordinates(pts) <- c("LON", "LAT")
proj4string(pts) <- CRS(crs.orig)
ovr=over(poly_grd, pts, fn=length)[1] #find how many points in each poly
colnames(ovr) = "COUNT"
#join the count data to the correct polygon
poly_grd@data=cbind(ovr[1], z=poly_grd@data[, "z"][match(rownames(ovr[1]), rownames(poly_grd@data))])



# Get colours proportional to values of counted field ---------------------

the.col.codes=seq(from=min(as.numeric(poly_grd@data$COUNT), na.rm =T), 
                  to=max(as.numeric(poly_grd@data$COUNT), na.rm =T),by=1)
the.col.cols=color.code( type="colourblind1", n=length(the.col.codes) )

  #colour.scale(type="mike", nlevels=length(the.col.codes)+1, x=poly_grd@data$COUNT, transparency=0.9)$cols
#the.col.cols=paste("#", the.col.cols$code, sep="")
color.df=cbind(COUNT=as.numeric(the.col.codes),colcode=the.col.cols)
#join the colour to the correct polygon
poly_grd@data=merge(poly_grd@data,color.df, by="COUNT", all.x=T)
poly_grd@data=poly_grd@data[order(poly_grd@data$z),] #order by z to ensure correct coloring

# Get basemap data --------------------------------------------------------
p=map("worldHires", regions=c("Canada","USA", "Greenland"), col="navajowhite2",border="navajowhite4", xlim=limits$X, ylim=limits$Y, plot=F, fill=T)
IDs <- sapply(strsplit(p$names, ":"), function(x) x[1])
basemap <- map2SpatialPolygons(p, IDs=IDs, proj4string=CRS(crs.orig))


# Plot the results --------------------------------------------------------
#sp::plot(poly_grd, col=poly_grd@data$colcode)
plot(poly_grd, col=poly_grd@data$colcode)
plot(basemap, 
     col="navajowhite2",
     border="navajowhite4", add=T)
#points(pts,col="black", pch=20, cex=0.2)


# Test reprojection -------------------------------------------------------
plot(spTransform(poly_grd, CRS(crs.new)), col=poly_grd@data$colcode)
plot(spTransform(basemap, CRS(crs.new)), 
     col="navajowhite2",
     border="navajowhite4", add=T)
     points(spTransform(pts, CRS(crs.new)),col="red", pch=20, cex=0.2)
