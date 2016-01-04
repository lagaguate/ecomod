makeSpMap<-function(xlim=c(-67,-57),ylim=c(42,47.5)){
  #//MMM - 2016 - This makes a simple basemap from shapefiles using sp and rgdal
  #//I kind of like it because the inital extent can be set prior to the map
  #//ever being drawn, and each layer is added only once 
  #//
  #//Additional layers can be added like this:
  #//#create the base map
  #//base<-makeSpMap()
  #//#load in the new shapefile
  #//newshape="myshapefile_83UTM20N.shp"
  #//newshape<-readOGR(<PATH_TO_SHAPEFILE>,gsub(".shp","",newshape))
  #//#if necessary, transform it to WGS84 to match the base data
  #//crs.geo <- CRS("+init=epsg:4326")
  #//newshape=spTransform(newshape, crs.geo)
  #//#add it to the base map
  #//myMap=base+layer(sp.lines(newshape, col="black", under=F)) 
  #//#repeat for each additional layer, and finally, draw the final product
  #//myMap
  
  
  library(sp)
  library(rgdal) #readOGR
  library(latticeExtra) # For layer()
  loadfunctions("polygons")
  
  shapeloader<-function(i){
    loadfunctions("utility/src/_Rfunctions/spatial")
    readOGR(
      gsub(
        paste0("/",
               regmatches(
                 find.ecomod.gis(i),
                 regexpr("([^/]+$)+",
                         find.ecomod.gis(i)))),"",
        find.ecomod.gis(i)),
      gsub(".shp","",regmatches(
        find.ecomod.gis(i),
        regexpr("([^/]+$)+",
                find.ecomod.gis(i)))), useC=FALSE)
  }

#baselayers - list in order drawn (subsequent layers will be drawn on top of preceding ones)
baselayer.shapes<-c("dm200_region",
                    "dm100_region",
                    "landmass_region",
                    "coastline_polyline")

baselayer<-list()
for (i in 1:length(baselayer.shapes)){
  baselayer[i]<-shapeloader(baselayer.shapes[i])
}
#polygons added with sp.polygons, lines added with sp.lines
s<- spplot(baselayer[[1]], "ID", xlim=xlim, ylim=ylim, 
           col.regions="steelblue2", col="steelblue2", colorkey=FALSE)
s<-s+layer(sp.polygons(baselayer[[2]], fill="lightblue1", col="lightblue1", under=F))   
s<-s+layer(sp.polygons(baselayer[[3]], fill="moccasin", col="moccasin", under=F))    
s<-s+layer(sp.lines(baselayer[[4]], col="navajowhite3", under=F)) 
return(s)
}