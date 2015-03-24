#----------------------------------------------------
# generate map using PBSmapping plotting functions
# derived from Ben Zisserson's original work (June 14, 2013 08:15:00 PM)
# couple of mods by Adam June 14, 2013 01:21:02 PM 
# Simplified and generalized by Mike M, Mar 12, 2015
#  - area extents added as a separate function (getExtent.R)
#  - numerous more extents added for use by that function
#----------------------------------------------------
makeMapBasic= function(x,xlim=c(-67,-57), ylim=c(42,47.5), title="", area=c('ALL'),main=""){
  require(PBSmapping)
  require("raster")
	require("geosphere")
  
# read in shapefiles
#--------------------------------------
  basemap= importShapefile(find.ecomod.gis("map_base_region"))
  dm200= importShapefile(find.ecomod.gis("dm200_region"))
  dm100= importShapefile(find.ecomod.gis("dm100_region"))
  land= importShapefile(find.ecomod.gis("landmass_region"))
  coast=importShapefile(find.ecomod.gis("coastline_polyline"))

# Provide projection information
#---------------------------------
  proj.abbr=attr(basemap, "projection") # abbreviated projection info
  proj.full=attr(basemap, "prj") # full projection info

  #use getExtent.R to find the bound of the various areas
  #(snowcrab, NAFO, Strata and more)
  #if multiple areas specified, get bounds that contain them all
  #if extent not found, alert user
  allExtents<-data.frame()
  for (i in 1:length(area)){
    if (!is.null(ncol(getExtent(area[i])))){
      allExtents<-rbind(allExtents, data.frame(getExtent(area[i])))
    }else{
      print(paste0("Excluded the extent of area '",area[i],"' - could not find it"))
    }
  }
  minX<-min(allExtents[,2])
  maxX<-max(allExtents[,3])
  minY<-min(allExtents[,4])
  maxY<-max(allExtents[,5])
  xlim<-c(minX,maxX)
  ylim<-c(minY,maxY)
  
  plotPolys(basemap, projection="LL", col="royalblue2", border="black",
  font.lab=1,  xlab="Longitude", ylab="Latitude", axes=T, tck=-.01,
  tckLab=TRUE, ylim=ylim, xlim=xlim,main=main)
     
  title(main=title, line=1, cex.main = .7)
  addPolys(dm200, col="steelblue2", border="steelblue2")
  addPolys(dm100, col="lightblue1", border="lightblue1")

#Try adding a csv data layer
#  ports <- as.EventData(read.csv(find.ecomod.gis("ports.csv")))
#  addPoints(ports,col="red")
#  addLabels(ports)

  #Overlay land and coastline such that any bad data (on land) is hidden
  addPolys(land, col="moccasin", border="moccasin")
  addLines(coast, col="black")
  box()
}