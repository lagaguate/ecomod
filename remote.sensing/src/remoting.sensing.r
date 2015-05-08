# MODIS chl images

#install.packages("rasterVis")
#install.packages("rgdal")

library(rgdal)
library(rasterVis)
library(maps)
library(mapdata)
library(raster)
library(maptools)

fn="A2002auga1-NA-chlor_a.tif" 
fn1="1998auga1-noaa-sst-cyl.tif"
sstPath="C:/home/choij/ecomod_data/remote.sensing/sst/"

r=raster(fn)
sst=raster(paste(sstPath,fn1,sep=""))

# remove land and clouds. Why 28???
r[(r<0.1|(r>28))]=NA


# EXTRACT DATA USING POLYGON

# STEP 1: DEFINE POLYGON

# load St.Anns polygon
path="C:/home/choij/work/"
fn="StAnnsMPA_polygon.csv" 

pol=read.csv(paste0(path,fn))

# convert to spatial polygon
pp=Polygon(pol)
pp1=SpatialPolygons(list(Polygons(list(pp),1)))


# METHOD 1: result is dataframe that looks like pixEx file from BEAM

#extract data from the polygon; results will be in dataframe
pd=extract(r,pp1,cellnumbers=TRUE, df=TRUE)

# this does not have lat and lon for each pixel. 
# To convert from cellnumber to coordinates use xyFromCell
cc=data.frame(xyFromCell(r,pd$cell))

# add coordinates to extracted pixels
pd=cbind(pd,cc)

# end of METHOD 1


# METHOD 2: extracted data is vector, no locations associated with the pixel.
pdv=extract(r,pp1)
pdv=unlist(pdv)
# end of METHOD 2



sst[(sst< -5)]=NA
levelplot(sst,margin=FALSE,col.regions=cp, main=fn1)

# plot raster
# remove data for each parameter

cp=colorRampPalette(c("purple","blue","cyan","green","yellow","red"))
levelplot(r,margin=FALSE,col.regions=cp, main=fn)

# to subset
ss=extent(-71,-48,39,48)
rsub=crop(r,ss)
fns=paste0("subset_",fn)
writeRaster(rsub,fns)



# make a jpg with coastline

# find the extent of the raster
lim=extent(r)
xlim=lim[1:2]
ylim=lim[3:4]

# get coastline
coast.map =maps:: map( "worldHires", regions=c("Canada", "USA"), xlim=xlim, ylim=ylim, 
                 fill=TRUE, plot=FALSE )

IDs=sapply(strsplit(coast.map$names,":"), function(x) x[1])

bPols=map2SpatialPolygons(coast.map, IDs=IDs, proj4string=CRS(projection(r)))

# color map
cp=colorRampPalette(c("purple","blue","cyan","green","yellow","red"))

# plot raster and coastline
levelplot(r,margin=FALSE, col.regions=cp, main=fn, zlim=c(0,28)) + layer(sp.polygons(bPols,lwd=0.8, col="darkgray"))

