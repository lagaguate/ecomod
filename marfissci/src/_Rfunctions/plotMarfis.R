# If reqd, load ecomod functions ------------------------------------------
if (F) {
  loadfunctions("utility/src/_Rfunctions/colours")
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
library(classInt) #for generating classes of data


# User Parameters ---------------------------------------------------------
limits = data.frame(X = c(-68,-58),Y = c(40,50)) #boundaries of plot
gridres = 1 
crs.orig = "+proj=longlat +datum=WGS84" #initial projection of all data
crs.new = "+proj=utm +zone=20 +datum=WGS84" #what proj to show fproduct
use.groundfish = T
ruleOf = 5  #min num of pts for a polygon to receive a colour
nclasses= 6  #num of data buckets
anal.field = "TOTNO"
anal.fn = "mean"
# Load the desired point data (from MARFIS) -------------------------------
setwd("C:/Users/mcmahonm/Documents/Assistance/ChoiJ/20150209")
if (use.groundfish == T){
pts = read.csv2("201502_GroundfishTest.csv")
}else{
pts = read.csv("201502_MarfissciTest.csv")
}
req.fields = c("LAT","LON",anal.field)
#hack to keep data from overlapping gridlines
pts$LAT = pts$LAT+(pi/10000000)
pts$LON = pts$LON+(pi/10000000)
# Create bounding boxes, make the grid, and convert it to polygons---------
coordinates(limits) = c("X", "Y")
proj4string(limits) = CRS(crs.orig)
boundbox = SpatialPolygons(list(Polygons(list(Polygon(cbind(
  mm= c(limits$X[2],
       seq(limits$X[2],limits$X[1],length=200),
       seq(limits$X[1],limits$X[2],length=200)),
  nn = c(limits$Y[2],
       seq(limits$Y[1],limits$Y[1],length=200),
       seq(limits$Y[2],limits$Y[2],length=200))))),
  ID = "bb")), 
  proj4str = CRS(crs.orig))

#'from http://tinyurl.com/jjz4p3t
bb = boundbox@bbox
cs = c(gridres, gridres) #cellsize = 1deg*1deg
cc = bb[, 1] + (cs/2)  # cell offset
cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
grd = GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
sp_grd = SpatialGridDataFrame(grd,
                               data = data.frame(id = 1:prod(cd)),
                               proj4string = CRS(proj4string(boundbox)))
poly_grd = Grid2Polygons(sp_grd)

# Prepare point data for spatial analyses ---------------------------------
pts = data.frame(pts[complete.cases(pts[req.fields]),])  
coordinates(pts) = c("LON", "LAT")
proj4string(pts) = CRS(crs.orig)
#'perform function on anal.field points in each poly using length
ovr = over(poly_grd, pts[anal.field], fn=eval(anal.fn))
#colnames(ovr) = "COUNT"
#join the count data to the correct polygon
poly_grd@data = cbind(ovr[1], z = poly_grd@data[, "z"][match(rownames(ovr[1]), rownames(poly_grd@data))])

# Use class intervals to bucket data for display --------------------------
# Use ruleof to control coloring polys w/o sufficient data ----------------
#& poly_grd@data$COUNT > ruleOf
 classes = classIntervals(as.data.frame(poly_grd@data[anal.field])[!is.na(as.data.frame(poly_grd@data[anal.field])) ], 
                          n=nclasses, style= "quantile") 
#"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks"
 colcode = findColours(classes, c("#ffffd9", "#081d58"))
 color.df = as.data.frame(cbind(varname=classes$var,colcode))
 names(color.df)[names(color.df)=="varname"] <- toString(anal.field)
 poly_grd@data = merge(poly_grd@data,unique(color.df), by= anal.field, all.x = T)
 poly_grd@data = poly_grd@data[order(poly_grd@data$z),] #order by z to ensure correct coloring

# Get basemap data --------------------------------------------------------
p = map("worldHires", regions = c("Canada","USA", "Greenland"), col = "navajowhite2",border = "navajowhite4", xlim=limits$X, ylim=limits$Y, plot=F, fill=T)
IDs = sapply(strsplit(p$names, ":"), function(x) x[1])
basemap = map2SpatialPolygons(p, IDs = IDs, proj4string = CRS(crs.orig))

# reproject to UTM- -------------------------------------------------------
plot(spTransform(poly_grd, CRS(crs.new)), col = poly_grd@data$colcode, border = "gray90")
plot(spTransform(basemap, CRS(crs.new)), 
     col = "navajowhite2",
     border = "navajowhite4", add = T)
     points(spTransform(pts, CRS(crs.new)),col = "red", pch = 20, cex = 0.2)
     legend("topleft", legend = c(names(attr(colcode, "table")),"no data"), 
            fill = c(attr(colcode, "palette"),"white"), 
            title = paste0(anal.fn, anal.field, "  /cell"))
     