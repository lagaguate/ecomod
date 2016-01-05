#MMM Sept 2014 - altered paths to reflect new ecomod polygon paths
plot.observer.shapefiles= function(x,
                                   wd=wd <- file.path(project.datadirectory('observers'))){
  require(PBSmapping)
  library(sp) 
  library(rgdal)
  
  
  loadfunctions("utility/src/_Rfunctions/spatial")
  
  #crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
  crs.geo <- CRS("+init=epsg:4326") 
  
  the.shape= importShapefile(file.path(wd,x),projection = "UTM")
  
  #rgdal
  the.shape <- readOGR(wd, "sp_4321_2013-12-01_tow_83UTM20N")
  the.shape=spTransform(the.shape, crs.geo)   
  the.shape2<-as.PolySet(the.shape)


  makeMapBasic()
  addLines(the.shape2, col="black")
}
