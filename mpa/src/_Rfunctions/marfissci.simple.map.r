marfissci.simple.map<-function(rds, 
                               colour.by = "AGG_FIELD.SUM",
                               crs.out="+proj=utm +zone=20 +datum=WGS84",
                               xlim=c(-72,-52),
                               ylim=c(40,50),
                               valid.only = T
){
  library(classInt)
  library(rgdal)
  limits = data.frame(X = xlim, Y = ylim) 
  coordinates(limits) = c("X", "Y")
  proj4string(limits) = CRS("+proj=longlat +datum=WGS84")
  boundbox = SpatialPolygons(list(Polygons(list(Polygon(cbind(
    mm= c(limits$X[2],
          seq(limits$X[2],limits$X[1],length=200),
          seq(limits$X[1],limits$X[2],length=200)),
    nn = c(limits$Y[2],
           seq(limits$Y[1],limits$Y[1],length=200),
           seq(limits$Y[2],limits$Y[2],length=200))))),
    ID = "bb")), 
    proj4str = CRS("+proj=longlat +datum=WGS84"))
  boundbox.pr = spTransform(boundbox,crs.out)
  #make slightly bigger bbox to reserve space in final
  boundbox2 = SpatialPolygons(list(Polygons(list(Polygon(cbind(
    mm= c(limits$X[2]+2,
          seq(limits$X[2]+2,limits$X[1]-2,length=200),
          seq(limits$X[1]-2,limits$X[2]+2,length=200)),
    nn = c(limits$Y[2]+2,
           seq(limits$Y[1]-2,limits$Y[1]-2,length=200),
           seq(limits$Y[2]+2,limits$Y[2]+2,length=200))))),
    ID = "bb2")), 
    proj4str = CRS("+proj=longlat +datum=WGS84"))
  boundbox2.pr = spTransform(boundbox2,crs.out)
  
   if (valid.only) rds = rds[rds@data$VALIDITY == 'VALID',]
   rds@data$ORD = seq.int(nrow(rds))
#   df.sp = SpatialPointsDataFrame(cbind(df$LON, df$LAT), df, match.ID = FALSE)
#   proj4string(df.sp) = CRS("+proj=longlat +datum=WGS84")
#   
  classes = classIntervals(rds@data[,c(colour.by)], n=5, style= "quantile", dataPrecision=0)
  colcode = findColours(classes, c("#edf8b1","#7fcdbb","#2c7fb8")) #colorblind-friendly yellow-blue
  #c("#deebf7", "#9ecae1","#3182bd") #colorblind-friendly blues
  #c("#fee6ce","#fdae6b","#e6550d") #colorblind-friendly oranges
  color.df = as.data.frame(cbind(varname=classes$var,colcode))
  names(color.df)[names(color.df)=="varname"] <- colour.by
  rds@data = merge( rds@data,unique(color.df), all.x = T)
  rds@data = rds@data[order(rds@data$ORD),]
  rds.clipped = rds[boundbox,]  #clip data to bbox
  rds.clipped.pr = spTransform(rds.clipped, CRS(crs.out), match.ID=F)
  
  if (!exists("coast") && !exists("coast.clipped")) {
    loadfunctions("coastline")
    writeLines("Building the coastline...")
    coast <<- coastline.db( DS="gshhg coastline highres", 
                          crs="+proj=longlat +datum=WGS84", 
                          p=NULL, level=1, xlim=NULL, ylim=NULL )
    if (!exists("coast.clipped")) {  
      library(rgeos)
      writeLines("Trimming the data to match the selected bounding box (so that data can be projected)")
      coast.clipped <<- gIntersection(gBuffer(coast, byid=TRUE, width=0), boundbox)
      coast.clipped.pr <<- spTransform(coast.clipped,crs.out)
    }
   
    #'using the clipped data (pre-projection), capture information for the grid,
    #'including information about the gridlines, as well as their labels
    grid <- gridat(boundbox, easts=seq(boundbox@"bbox"[1],boundbox@"bbox"[3],by=2), 
                     norths=seq(boundbox@"bbox"[2],boundbox@"bbox"[4],by=2))
    grid.pr <- spTransform(grid, CRS(crs.out))
    gridlines <- gridlines(boundbox, easts=seq(boundbox@"bbox"[1],boundbox@"bbox"[3],by=1), 
                      norths=seq(boundbox@"bbox"[2],boundbox@"bbox"[4],by=1))
    gridlines.pr <- spTransform(gridlines, CRS(crs.out))
  }

  plot(boundbox2.pr, border="transparent", add=F, lwd=1) #add transparent boundbox first to ensure all data shown
  plot(coast.clipped.pr, col="navajowhite2", border="navajowhite4", axes=F, add=T )  #add coastline
  plot(gridlines.pr, col="grey77", lty=2, add=T)                           #gridlines
  points(rds.clipped.pr, col = rds.clipped.pr@data$colcode, pch = 22, cex = 0.2)
  text(coordinates(grid.pr), pos=grid.pr$pos, labels=parse(text=as.character(grid$labels)), 
       offset=0.2, col="black", cex=1)
  plot(boundbox.pr, border="black", add=T, lwd=1) #add actual boundbox

  legend(min(boundbox.pr@bbox[1,])+(0.075*(max(boundbox.pr@bbox[1,])-min(boundbox.pr@bbox[1,]))),
         min(boundbox.pr@bbox[2,])+(0.95*(max(boundbox.pr@bbox[2,])-min(boundbox.pr@bbox[2,]))),
         cex=0.5, y.intersp=0.8,
         legend = c(gsub(",","-",names(attr(colcode, "table"))),"no data"), 
         fill = c(attr(colcode, "palette"),"white"))
}