aggregate.marfis <-function(pts, show.pts=F,
                            xlim=c(-71,-56), ylim=c(40,48), gridres=1, 
                            anal.fn = "mean", anal.field = "RND_WEIGHT_KGS",
                            privacy.fields = c("VR_NUMBER_FISHING","LICENCE_ID"),
                            nclasses= 5, class.style="pretty",
                            title="aggregate.marfis.R" ){
#'MMM - Feb 2016
#'This function seeks to facilitate the distribution of marfis data by 
#'automating  the measures specified for protecting fishers' private 
#'information.  It aggregates data while ensuring that each resultant cell has 
#'the minimum required number of unique values of all privacy-related fields 
#'(currently 5, and specified as "ruleOf").  For example, if a cell has only 3 
#'unique values for a field identified within "privacy.fields" (e.g. 3 VRNs), 
#'that grid cell will not be shown. If multiple privacy fields are provided, 
#'this script will ensure that sufficient unique values are present in each cell 
#'for ALL fields.
#'
#' USER PARAMETERS 
#'pts = the input data.frame containing data with LAT and LON fields
#'xlim/ylim = bounding coordinates of output grid (degrees)
#'gridres = size if the grid square in decimal degrees
#'anal.fn = an R function to be applied to the anal.field (e.g. mean, sum, 
#'          length, min, max...)
#'anal.field = the field upon which the anal.fn function will work
#'privacy.fields = one or more fields containing sensitive data
#'nclasses = the number of "bins" used to classify the data
#'class.style = method of binning data offered by the classInt package
#'              options include "fixed", "sd", "equal", "pretty", "quantile", 
#'              "kmeans", "hclust", "bclust", "fisher", or "jenks"
#'title = the map title
#'
#'NOTES
#'Any records missing values in LAT, LON or the anal.field will be dropped
#'Final data projection is in UTM Zone 20
  
# Load the packages -------------------------------------------------------
library(sp)
library(Grid2Polygons) #for converting grid to polygons
library(mapdata)  #for getting basemapobjects
library(maptools) #for converting basemap lines to polygons
library(classInt) #for generating classes of data

crs.orig = "+proj=longlat +datum=WGS84" #initial projection of all data
crs.new = "+proj=utm +zone=20 +datum=WGS84" #what proj to show fproduct
  
# Privacy Controls ------------------------------------------------------- 
ruleOf = 5  #this many unique values of EACH of the privacy.fields must be present

req.fields = c("LAT","LON",anal.field)

#hack to keep data from overlapping gridlines
pts$LAT = pts$LAT+(pi/10000000)
pts$LON = pts$LON+(pi/10000000)
# Create bounding boxes, make the grid, and convert it to polygons---------
limits = data.frame(X = xlim, Y = ylim) 

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

pts = data.frame(pts[complete.cases(pts[req.fields]),])  
coordinates(pts) = c("LON", "LAT")
proj4string(pts) = CRS(crs.orig)

# Determine the number of unique values for each privacy field  -----------
# Privacy field counts are identified by the prefix 'pri_' ----------------
priv_cnt = over(poly_grd, pts[privacy.fields], fn=function(x) length(unique(x)))
colnames(priv_cnt) <- paste("priv", colnames(priv_cnt), sep = "_")
priv_cnt$z = as.numeric(gsub("X","",rownames(priv_cnt)))

#perform desired analytic (anal.fn) on desired field (anal.field) in each cell 
res = over(poly_grd, pts[anal.field], fn=eval(anal.fn))
res$z = gsub("X","",rownames(res))

#join the privacy and analytic data to the correct polygon
poly_grd@data = merge(poly_grd@data,priv_cnt, by="z")
poly_grd@data = merge(poly_grd@data,res, by="z")

#Find records where all privacy fields have sufficient unique records/cell
  public = as.data.frame(priv_cnt[complete.cases(priv_cnt),])
  public = as.data.frame(public[apply(public, 1, function(row) {all(row >= ruleOf)}),])

if (nrow(public)<1) {
  stop(return(print("No data can be displayed - none meets privacy requirements")))
}
public=merge(public,res, by="z")

#  class intervals to bucket data for display --------------------------
classes = classIntervals(public[[anal.field]], n=nclasses, style= class.style) 
 colcode = findColours(classes, c("#edf8b1", "#081d58")) #colorblind-friendly blues
 color.df = as.data.frame(cbind(varname=classes$var,colcode))
 names(color.df)[names(color.df)=="varname"] <- toString(anal.field)
 poly_grd@data = merge(poly_grd@data,unique(color.df), by= anal.field, all.x = T)
 poly_grd@data = poly_grd@data[order(poly_grd@data$z),] #order by z to ensure correct coloring

 # Get basemap data --------------------------------------------------------
p = map("worldHires", regions = c("Canada","USA", "Greenland"), col = "navajowhite2",border = "navajowhite4", xlim=limits$X, ylim=limits$Y, plot=F, fill=T)
IDs = sapply(strsplit(p$names, ":"), function(x) x[1])
basemap = map2SpatialPolygons(p, IDs = IDs, proj4string = CRS(crs.orig))

plot(spTransform(poly_grd, CRS(crs.new)), col = poly_grd@data$colcode, border = "gray90", main=title)
plot(spTransform(basemap, CRS(crs.new)), col = "navajowhite2", border = "navajowhite4", add = T)
     # points obviously shouldn't be plotted, but is shown here for purposes
     #  of initial validation of output
      if (show.pts) points(spTransform(pts, CRS(crs.new)),col = "red", pch = 20, cex = 0.2)
      legend("topleft", legend = c(names(attr(colcode, "table")),"no data"), 
             fill = c(attr(colcode, "palette"),"white"), 
             title = paste0(anal.fn, " ", anal.field, "  /cell"))
      # dev.off()    
}
# setwd("C:/Users/mcmahonm/Documents/Assistance/ChoiJ/20150209")
# df = read.csv2("20160225.csv")