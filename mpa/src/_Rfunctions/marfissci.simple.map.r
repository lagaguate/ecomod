marfissci.simple.map<-function(rds, 
                               agg.by = "SPECIES_CODE",
                               colour.by = "SUM_RND_WEIGHT_KGS",
                               crs.out="+proj=utm +zone=20 +datum=WGS84",
                               xlim=c(-68,-53),
                               ylim=c(40,49),
                               valid.only = T,
                               show.legend = T,
                               save.plot = T,
                               out.folder = "marfissci",
                               plot.title="",
                               name.det = NULL,
                               nclasses=5,
                               add.OCMD = c("St_Ann","Gully","Vazella_Emerald","Vazella_Sambro","Lophelia", "NE_Channel")
){
  proj.metric = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 
                 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'
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
    mm= c(limits$X[2]+1,
          seq(limits$X[2]+1,limits$X[1]-1,length=200),
          seq(limits$X[1]-1,limits$X[2]+1,length=200)),
    nn = c(limits$Y[2]+1,
           seq(limits$Y[1]-1,limits$Y[1]-1,length=200),
           seq(limits$Y[2]+1,limits$Y[2]+1,length=200))))),
    ID = "bb2")), 
    proj4str = CRS("+proj=longlat +datum=WGS84"))
  boundbox2.pr = spTransform(boundbox2,crs.out)
  
  #should clip the data to desired frame here so that classIntervals only works 
  #on visible data!
  #Do nrow check AFTER that
  rds.clipped = rds[boundbox,]  #clip data to bbox
  if (valid.only) rds.clipped = rds.clipped[rds.clipped@data$VALIDITY == 'VALID',]
  ncheck=length(unique(rds.clipped@data[,c(colour.by)])) #don't have enough data for requested number of classes
  if (ncheck<2) return(NULL) #can't classify on a single value
  if (nclasses>ncheck) nclasses=ncheck
  rds.clipped@data$ORD = seq.int(nrow(rds.clipped))
  classes = classIntervals(rds.clipped@data[,c(colour.by)], n=nclasses, style= "quantile", dataPrecision=0)
  colcode = findColours(classes, 
  c("#c7e9b4","#41b6c4","#225ea8","#081d58")) #colorblind-friendly yellow-blue
  #c("#deebf7", "#9ecae1","#3182bd")) #colorblind-friendly blues
  #c("#fee6ce","#fdae6b","#e6550d")) #colorblind-friendly oranges
  colour.df = as.data.frame(cbind(varname=classes$var,colcode))
  names(colour.df)[names(colour.df)=="varname"] <- colour.by
  rds.clipped@data = merge( rds.clipped@data,unique(colour.df), all.x = T)
  #rds.clipped@data = rds.clipped@data[order(rds.clipped@data$ORD),]
  
  rds.clipped.pr = spTransform(rds.clipped, CRS(crs.out), match.ID=F)
  rds.clipped.pr@data = rds.clipped.pr@data[order(rds.clipped.pr@data$ORD),]
  
  
  if (!exists("coast.aea") || 
      !exists("coast.clipped.aea") || 
      !exists("coast.clipped.pr") || 
      !exists("the.grid") || 
      !exists("grid.pr") || 
      !exists("these.gridlines") || 
      !exists("these.gridlines.pr") )
    {
    loadfunctions("coastline")
    writeLines("Building the coastline...")
    coast.aea <<- coastline.db( DS="gshhg coastline highres", 
                          crs=proj.metric, 
                          p=NULL, level=1, xlim=NULL, ylim=NULL )
      library(rgeos)
      writeLines("Trimming the data to match the selected bounding box (so that data can be projected)")
      coast.clipped.aea <<- gIntersection(gBuffer(coast.aea, byid=TRUE, width=0), spTransform(boundbox,proj.metric))
      coast.clipped.pr <<- spTransform(coast.clipped.aea,crs.out)

    #'using the clipped data (pre-projection), capture information for the grid,
    #'including information about the gridlines, as well as their labels
    the.grid <<- gridat(boundbox, easts=seq(boundbox@"bbox"[1],boundbox@"bbox"[3],by=2), 
                     norths=seq(boundbox@"bbox"[2],boundbox@"bbox"[4],by=2))
    grid.pr <<- spTransform(the.grid, CRS(crs.out))
    these.gridlines <<- gridlines(boundbox, easts=seq(boundbox@"bbox"[1],boundbox@"bbox"[3],by=1), 
                      norths=seq(boundbox@"bbox"[2],boundbox@"bbox"[4],by=1))
    these.gridlines.pr <<- spTransform(these.gridlines, CRS(crs.out))
  }
  if (!exists("clip.100") || 
      !exists("clip.200") || 
      !exists("clip.300") || 
      !exists("clip.400") || 
      !exists("clip.500") || 
      !exists("clip.600") || 
      !exists("clip.700") || 
      !exists("clip.800") || 
      !exists("clip.900"))
  {
  #bathy data
    writeLines("Generating contours")
   p = list( project.name = "bathymetry" )
   p$project.root = project.datadirectory( p$project.name )
   p$init.files = loadfunctions( c( "spacetime", "utility", "parallel", "bathymetry", "polygons" ) )
   p$libs = RLibrary( "rgdal", "maps", "mapdata", "maptools", "lattice", "geosphere", "sp", "raster", "colorspace" )
   p = spatial.parameters( type="canada.east.highres", p=p ) 
   depths = c(100, 200, 300, 400, 500, 600, 700, 800, 900) #, 2000, 5000 )
   plygn = isobath.db( p=p, DS="isobath", depths=depths  )
   #data must be clipped so it doesn't extend beyond the bounding box
  clip.100 <<- gIntersection(spTransform(plygn["100"], CRS(crs.out)), boundbox.pr)
  clip.200 <<- gIntersection(spTransform(plygn["200"], CRS(crs.out)), boundbox.pr)
  clip.300 <<- gIntersection(spTransform(plygn["300"], CRS(crs.out)), boundbox.pr)
  clip.400 <<- gIntersection(spTransform(plygn["400"], CRS(crs.out)), boundbox.pr)
  clip.500 <<- gIntersection(spTransform(plygn["500"], CRS(crs.out)), boundbox.pr)
  clip.600 <<- gIntersection(spTransform(plygn["600"], CRS(crs.out)), boundbox.pr)
  clip.700 <<- gIntersection(spTransform(plygn["700"], CRS(crs.out)), boundbox.pr)
  clip.800 <<- gIntersection(spTransform(plygn["800"], CRS(crs.out)), boundbox.pr)
  clip.900 <<- gIntersection(spTransform(plygn["900"], CRS(crs.out)), boundbox.pr)
  #clip.1000 <<- gIntersection(spTransform(plygn["1000"], CRS(crs.out)), boundbox.pr)
  }
  #get the desired OCMD areas
  if (length(add.OCMD)>0) OCMD.areas=get.ocmd.areas(add.OCMD)

 if (save.plot){
   if (!is.null(name.det)){
     name.detail=paste0(name.det,"_")
   }else{
     name.detail=""
   }
   if (range(rds.clipped.pr@data[agg.by])[1] == range(rds.clipped.pr@data[agg.by])[2]) {
     the.filename = range(rds.clipped.pr@data[agg.by])[1]
   }else{
     the.filename = paste(range(rds.clipped.pr@data[agg.by]),collapse = "_")
   }
#    plot.title.clean=gsub("(\\(|\\)|\\s|\\/|,)","_",plot.title) 
#    plot.title.clean=gsub("__","_", plot.title.clean)
#    plot.title.clean=substr(plot.title.clean,1,15)
#    plot.title.clean=paste0(sub('_$', '', plot.title.clean),"_")
   
   agg.type=paste0(substr(agg.by, 1, 4),"_")
   
   the.filename=paste0(name.detail,agg.type,the.filename,".png")
   
   png(filename=paste0(project.datadirectory("mpa"),"/",out.folder,"/figures/",the.filename),
      width = 6, height = 4, units = "in", res= 300, pointsize = 4,
      bg = "white", family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"))
 }
  par(mar=c(2,2,1,1),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
  plot(boundbox2.pr, border="transparent", add=F, lwd=1) #add transparent boundbox first to ensure all data shown
  plot(coast.clipped.pr, col="navajowhite2", border="navajowhite4", lwd=0.5, axes=F, add=T )  #add coastline
#   lines(clip.1000, col="#666666", lwd=0.5)
  lines(clip.900, col="#717171", lwd=0.5)
  lines(clip.800, col="#7C7C7C", lwd=0.5)
  lines(clip.700, col="#888888", lwd=0.5)
  lines(clip.600, col="#939393", lwd=0.5)
  lines(clip.500, col="#9E9E9E", lwd=0.5)
  lines(clip.400, col="#AAAAAA", lwd=0.5)
  lines(clip.300, col="#B5B5B5", lwd=0.5)
  lines(clip.200, col="#C0C0C0", lwd=0.5)
  lines(clip.100, col="#CCCCCC", lwd=0.5)
  for (o in 1:length(OCMD.areas)){
    plot(spTransform(OCMD.areas[[o]], CRS(crs.out)), border="olivedrab4", lwd=0.5, add=T)
  }
  plot(these.gridlines.pr, col="grey77", lty=2, lwd=0.5, add=T)                           #gridlines
  points(rds.clipped.pr, col = rds.clipped.pr@data$colcode, pch = 15, cex = 0.5)
  text(coordinates(grid.pr), pos=grid.pr$pos, labels=parse(text=as.character(the.grid$labels)), 
       offset=0.2, col="black", cex=1)
  plot(boundbox.pr, border="black", add=T, lwd=1) #add actual boundbox
  if (show.legend){
    legend(title = plot.title ,min(boundbox.pr@bbox[1,])+(0.075*(max(boundbox.pr@bbox[1,])-min(boundbox.pr@bbox[1,]))),
         min(boundbox.pr@bbox[2,])+(0.95*(max(boundbox.pr@bbox[2,])-min(boundbox.pr@bbox[2,]))),
         cex=1, y.intersp=0.8,
         legend = c(gsub(",","-",names(attr(colcode, "table"))),"no data"), 
         fill = c(attr(colcode, "palette"),"white"))
  }
  if (save.plot) dev.off()
}