aggregate.marfis <-function(pts, LatField="LAT", LonField="LON",
                            xlim=c(-71,-56), ylim=c(40,48), gridres=1, 
                            anal.fn = "mean", anal.field = "RND_WEIGHT_KGS",
                            privacy.field = c("VR_NUMBER_FISHING","LICENCE_ID"),
                            nclasses= 5, class.style="pretty",
                            plot.data= T, show.pts=F,
                            title="aggregate.marfis.R" ){

  #'MMM - Feb 2016
#'This function seeks to facilitate the distribution of marfis data by 
#'automating the measures specified for protecting fishers' private information.
#'It aggregates data while ensuring that each resultant cell has the minimum 
#'required number of unique values of all privacy-related fields (currently 5, 
#'and specified as "ruleOf").  For example, if a cell has only 3 unique values 
#'for a field identified within "privacy.field" (e.g. 3 VRNs), that grid cell 
#'will not be shown. If multiple privacy fields are provided, this script will 
#'ensure that sufficient unique values are present in each cell for ALL fields.
#'
#'The input is a dataframe containing LAT, LON, an "anal.field" (on which to 
#'perform an analysis) and at least one "privacy.field" (which is counted to 
#'identify the number of unique values/cell).  The output is a 
#'SpatialPolygonsDataFrame, which can easily be converted to a shapefile. 
#'
#'The output data contains a field called "public" which is either "Yes", "No" 
#'or NA.  
#'public is NA when there is no data in the cell. 
#'public is "Yes" when the cell is sufficiently aggregated for public display.
#'public is "No" when there is not enough data for public display.
#'Additionally, columns are generated for each "privacy.field", and these hold 
#'a count of how many unique records for that field exist in that cell
#'
#' USER PARAMETERS 
#'pts = the input data.frame containing data with LAT and LON fields
#'xlim/ylim = bounding coordinates of output grid (degrees)
#'gridres = size if the grid square in decimal degrees
#'anal.fn = an R function to be applied to the anal.field (e.g. mean, sum, 
#'          length, min, max...)
#'anal.field = the field upon which the anal.fn function will work
#'privacy.field = one or more fields containing sensitive data
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
library(classInt) #for generating classes of data

crs.orig = "+proj=longlat +datum=WGS84" #initial projection of all data
crs.new = "+proj=utm +zone=20 +datum=WGS84" #what proj to show result
  
# Privacy Controls ------------------------------------------------------- 
ruleOf = 5  #this many unique values of EACH of the privacy.field must be present

req.fields = c(LatField, LonField, anal.field, privacy.field)
missing = req.fields[!(req.fields %in% colnames(pts))]
if (length(missing)>0){
  errormsg=paste("The following field(s) are required for this analysis: "
                 , paste(missing, collapse = ', ')) 
  stop(return(print(errormsg)))
}
#hack to keep data from overlapping gridlines
pts[[LatField]] = pts[[LatField]]+(pi/10000000)
pts[[LonField]] = pts[[LonField]]+(pi/10000000)
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
coordinates(pts) = c(LonField, LatField)
proj4string(pts) = CRS(crs.orig)

# Determine the number of unique values for each privacy field  -----------
# Privacy field counts are identified by the prefix 'cnt_' ----------------
priv_cnt = over(poly_grd, pts[privacy.field], fn=function(x) length(unique(x)))
colnames(priv_cnt) <- paste("cnt", colnames(priv_cnt), sep = "_")
priv_cnt$z = as.numeric(gsub("X","",rownames(priv_cnt)))

#perform desired analytic (anal.fn) on desired field (anal.field) in each cell 
res = over(poly_grd, pts[anal.field], fn=eval(anal.fn))
res$z = gsub("X","",rownames(res))

#join the privacy and analytic data to the correct polygon
poly_grd@data = merge(poly_grd@data,priv_cnt, by="z")
poly_grd@data = merge(poly_grd@data,res, by="z")

#Find records where all privacy fields have sufficient unique records/cell
  public = as.data.frame(priv_cnt[complete.cases(priv_cnt),!(colnames(priv_cnt) == "z")])
  public = as.data.frame(public[apply(public, 1, function(row) {all(row >= ruleOf)}),])
  public$z = as.numeric(gsub("X","",rownames(public)))

if (nrow(public)<1) {
  stop(return(print("No data can be displayed - none meets privacy requirements")))
}

public=merge(res, public, all.y=T, by="z")
#  class intervals to bucket data for display --------------------------
 classes = classIntervals(public[[anal.field]], n=nclasses, style= class.style) 
 #colcode = findColours(classes, c("#deebf7", "#9ecae1","#3182bd")) #colorblind-friendly blues
 #colcode = findColours(classes, c("#fee6ce","#fdae6b","#e6550d")) #colorblind-friendly oranges
 colcode = findColours(classes, c("#edf8b1","#7fcdbb","#2c7fb8")) #colorblind-friendly
 color.df = as.data.frame(cbind(varname=classes$var,colcode))

 color.df$public = "Yes"
 names(color.df)[names(color.df)=="varname"] <- toString(anal.field)
 
 poly_grd@data = merge(poly_grd@data,unique(color.df), by= anal.field, all.x = T)
 # browser()
 if(NROW(poly_grd@data[which(is.na(poly_grd@data$public) && !is.na(poly_grd@data[anal.field])),])>0){
   poly_grd@data[which(is.na(poly_grd@data$public) && !is.na(poly_grd@data[anal.field])),]$public = "No"
 }

 poly_grd@data = poly_grd@data[order(poly_grd@data$z),] #order by z to ensure correct coloring

 if (plot.data){
   library(mapdata)  #for getting basemapobjects
   library(maptools) #for converting basemap lines to polygons
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
 } 
 return(poly_grd)
}
#EXAMPLE USAGE
#df = read.csv2("my_marfis_extraction.csv")
#
# mygrid=aggregate.marfis(this, LatField="Lat_DD", LonField="Long_DD", 
#                         xlim=c(-66.275,-65.875), ylim=c(42.575,42.841), 
#                         grid=0.033333, anal.field = "RND_WEIGHT", 
#                         anal.fn = "sum", 
#                         privacy.field = c("VRN","BUYER_BUYER_CODE","LIC_LICENCE_ID"), 
#                         plot.data = T, show.pts = T)
#
#Convert grid to shapefile
# library(rgdal)
# writeOGR(mygrid, dsn = '.', layer = 'MARFIS_Grid', driver = "ESRI Shapefile", overwrite=T)
#
##cheatsheet converting marfis coords to dd
#df.qc$LAT = round(as.numeric(substr(df.qc$LATITUDE,1,2)) + as.numeric(substr(df.qc$LATITUDE,3,4))/60 + as.numeric(substr(df.qc$LATITUDE,5,6))/3600,4)
#df.qc$LON = -1*round(as.numeric(substr(df.qc$LONGITUDE,1,2)) + as.numeric(substr(df.qc$LONGITUDE,3,4))/60 + as.numeric(substr(df.qc$LONGITUDE,5,6))/3600,4)
