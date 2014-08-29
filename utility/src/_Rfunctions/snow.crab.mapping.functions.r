require("raster")
require("PBSmapping")
require("geosphere")
#----------------------------------------------------
# generate map using PBSmapping plotting functions
#----------------------------------------------------

makemap= function(x,area="ens", wd="C:/project/mapping/maps", addlabels=T, title="" ){
  require(PBSmapping)

  borders= read.csv(file=file.path(wd,"areaborders.csv"), head=T, sep=",")
  b=borders[which(borders$area==area),]

# read in shapefiles
#--------------------------------------
  basemap= importShapefile(file.path(wd,"map_base_region"))
  dm200= importShapefile(file.path(wd,"dm200_region"))
  dm100= importShapefile(file.path(wd,"dm100_region"))
  zones= importShapefile(file.path(wd,"sczones2010_polyline"))
  land= importShapefile(file.path(wd,"landmass_region"))
  coast=importShapefile(file.path(wd,"coastline_polyline"))
  axis=importShapefile(file.path(wd,"axis_polyline"))

# Provide projection information
#---------------------------------
  proj.abbr=attr(basemap, "projection") # abbreviated projection info
  proj.full=attr(basemap, "prj") # full projection info

  ylim=c(b$slat,b$nlat)
  xlim=c(-(b$wlon),-(b$elon))

   plotPolys(basemap, projection=proj.abbr, col="royalblue2", border="black",
   font.lab=2,  xlab="Longitude", ylab="Latitude", axes=T, tck=-.01,
   tckLab=TRUE, ylim=ylim, xlim=xlim)
     
  title(main=title, line=2)
  addPolys(dm200, col="steelblue2", border="steelblue2")
  addPolys(dm100, col="lightblue1", border="lightblue1")
  addLines(zones, col="darkgoldenrod1", lwd=2)


#Overlay land and coastline such that any bad data (on land) is hidden
  addPolys(land, col="khaki", border="khaki")
  addLines(coast, col="black")
  abline(h=b$slat, lwd=3)
  abline(h=b$nlat, lwd=3)
  abline(v=-b$wlon, lwd=3)
  abline(v=-b$elon, lwd=3)

#function to add area labels
#--------------------------------------------
  if (addlabels) {
    text("CFA 23", x=-58.05, y=44.55, font=2, cex=1.0)
    text("CFA 24", x=-60.9, y=43.75, font=2, cex=1.0)
    text("CFA 4X", x=-64.2, y=43.25, font=2, cex=1.0)
    text("N-ENS", x= -59.15, y=46.65, font=2, cex=1.0)
  }
 

}

cover= function(x,area="all"){
  require(PBSmapping)

  borders= read.csv(file=file.path(wd, "/areaborders.csv"), head=T, sep=",")
  b=borders[which(borders$area==area),]

# read in shapefiles
#--------------------------------------
  land= importShapefile(file.path(wd, "/landmass_region"))
  coast=importShapefile(file.path(wd, "/coastline_polyline"))


#Overlay land and coastline such that any bad data (on land) is hidden
  addPolys(land, col="khaki", border="khaki")
  addLines(coast, col="black")
  
  
  abline(h=b$slat, lwd=3)
  abline(h=b$nlat, lwd=3)
  abline(v=-b$wlon, lwd=3)
  abline(v=-b$elon, lwd=3)

}

plotRaster= function(path, xlab ="", ylab="", transpar = NULL, semitranspar = NULL, axes=F, tck= "",
                     tckLab=F, cellcount = NULL, xlim = NULL, ylim = NULL, quality = 1, ...){
  
  
  r = raster(path)
  print(r)
  if(is.null(cellcount)) cellcount = ncell(r)*quality
  if(is.null(xlim) & is.null(ylim)){
    xlim <<- c(xmin(extent(r)), xmax(extent(r)))
    ylim <<- c(ymin(extent(r)), ymax(extent(r)))
  }
  
  if(grepl("longlat", projection(r))) labelProjection = "LL"
  else labelProjection = "UTM"
  plt = c(.03, .97, .03, .97)
  
  # save settings in 'options'
  options(map.xlim = xlim);
  options(map.ylim = ylim);
  options(map.projection = labelProjection);
  
  # create plot region
  .initPlotRegion(projection=labelProjection, xlim=xlim, ylim=ylim, plt=plt);
  
  
   
  if(!is.null(transpar)){
    ctab = r@legend@colortable
    ind = which(col2rgb(ctab)[1,] == col2rgb(transpar)[1] & col2rgb(ctab)[2,] == col2rgb(transpar)[2] & col2rgb(ctab)[3,] == col2rgb(transpar)[3])
    if(length(ind)>0)r@legend@colortable[ind] = paste(r@legend@colortable[ind], "00", sep = "" )
    if(semitranspar){
      invind = setdiff(1:length( r@legend@colortable), ind)
      if(length(invind)>0)r@legend@colortable[invind] = paste(r@legend@colortable[invind], "AA", sep = "" )
    }
  }
  
  
  plot(r, maxpixels = cellcount, add = T)
  
  if (axes) {
    .addAxis(xlim = xlim, ylim = ylim, tckLab = tckLab, tck = tck,
             tckMinor = 0.5 * tck, ...);
    
  }
  
  # labels must go after axis
  
  .addLabels(projection = labelProjection, xlab = xlab, ylab = ylab, ...);
  
  
  
  
  
}
addRaster= function(path, ncel = NULL, semitranspar = F, transpar = NULL, quality = 1){
  r <- raster(path)
  
  if(is.null(ncel)) ncel = ncell(r)*quality 
  if(!is.null(transpar)){
    ctab = r@legend@colortable
    ind = which(col2rgb(ctab)[1,] == col2rgb(transpar)[1] & col2rgb(ctab)[2,] == col2rgb(transpar)[2] & col2rgb(ctab)[3,] == col2rgb(transpar)[3])
    if(length(ind)>0)r@legend@colortable[ind] = paste(r@legend@colortable[ind], "00", sep = "" )
    if(semitranspar){
      invind = setdiff(1:length( r@legend@colortable), ind)
      if(length(invind)>0)r@legend@colortable[invind] = paste(r@legend@colortable[invind], "AA", sep = "" )
    }
  }
  r@legend@colortable
  plot(r, add = T, maxpixels = ncel)
  
}
addGullyMPA = function(){
  x = NULL
  # <name>GULLY</name>
  x$X = c(-59.1, -58.5833333, -58.5833333, -59.1333333, -59.1333333, -59.3333333)
  x$Y = c(44.2166667, 43.7833333, 43.5833333, 43.5833333, 43.9166667, 44.1)
  x$PID = c(1, 1,1,1,1,1)
  x$POS = c(1, 2,3,4,5,6)
  x = as.PolySet(data.frame(x), projection = "LL")
  rc = col2rgb("red")
  addPolys(x, col = rgb(rc[1]/255, rc[2]/255, rc[3]/255, .3) , border = "black")
  
}

addStAnnsMPA <- function() {
 x = NULL
  # <name>StAnns</name>
  x$Y = c(46.1667,	46.1667,	46.2667,	46.4167,	46.4167,	46.2333,	46.0667,	46.0667,	45.9333,	45.7833)
  x$X = c(-59.65,	-59.3333,	-59.3333,	-59,	-58.6667,	-58.3667,	-58.5333,	-58.6667,	-58.6667,	-59.65)
  x$PID = c(1)
  x$POS = c(1:10)
  x = as.PolySet(data.frame(x), projection = "LL")
  rc = col2rgb("red")
  addPolys(x, col = rgb(rc[1]/255, rc[2]/255, rc[3]/255, .3) , border = "black")
}


 addEmera = function(direct){
	emera=importShapefile(file.path(direct,"mapping","maps","Emera Line","ENL_SubseaCable_2km_StudyArea.shp"))
	  rc = col2rgb("red")

        addPolys(emera,col = rgb(rc[1]/255, rc[2]/255, rc[3]/255, .3) , border = "black")

}

 addFisheryFootprint = function(direct,n=7){
	ff=importShapefile(file.path(direct,"mapping","crq0610_weight_2minGrid.shp"))
	cols 			<- colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"), space = "Lab")
	fp <- attr(ff,'PolyData')[,c('PID','ZDENSITY','CLASS','GMEAN')]
	fp$Z <- log(fp$ZDENSITY+1)
	br <- seq(min(fp$Z),max(fp$Z),length.out=n)
	a <- col2rgb(cols(n))/255
	coll <- apply(a,2,function(x) rgb(blue=x[3],green=x[2],red=x[1],0.3))
	fp <- makeProps(fp,breaks=br,propName='col',propVals=coll)
	addPolys(ff,polyProps=fp)
	}


addlinesSCAREA = function(){
  x = NULL
  #<name>NENS-SENS</name>
  x$X = c(-59.85247480316924,-57.78560987011954)
  x$Y = c(46.00,46.00106076110973)
  x$PID = c(1, 1)
  x$POS = c(1, 2)
  x = as.PolySet(data.frame(x), projection = "LL")
  addLines(x, col = "yellow", lwd = 10)
  
  #<name>CFA23 Inshore-offshore</name>
  
  x$X = c(-59.92853837122961,-58.41005287016962)
  x$Y = c(44.69774735070251,46.00038380222262)
  x$PID = c(1, 1)
  x$POS = c(1, 2)
  x = as.PolySet(data.frame(x), projection = "LL")
  addLines(x, col = "yellow", lwd = 10)
  
  # <name>CFA 23 - CFA 24</name>
  x$X = c(-60.66040620990439,-59.11881785180857)
  x$Y = c(45.58083805201212,43.67610276909335)
  x$PID = c(1, 1)
  x$POS = c(1, 2)
  x = as.PolySet(data.frame(x), projection = "LL")
  addLines(x, col = "yellow", lwd = 10)
  
  x = NULL
  # <name>4X-SENS</name>
  x$X = c(-63.52502801857509,-63.33296001561555, -63.33161397633095)
  x$Y = c(44.5005704612574,44.33343763428088, 42.50186912534272 )
  x$PID = c(1, 1, 1)
  x$POS = c(1, 2, 3)
  x = as.PolySet(data.frame(x), projection = "LL")
  addLines(x, col = "yellow", lwd = 10)
  
  
}
makechartSENS = function(){
  direct = "C://snowcrab"
  jpeg(filename = file.path(direct, "Survey","sample_senssurvey_2013.jpeg"), width = 16000, height = 7500, pointsize = 120)
  xlim = c(-66, -57)
  ylim = c(43, 46.1)
  plotRaster(file.path(direct,"mapping", "maps","Charts", "clip_WGS84_SENS4001.tif"),
             xlab="Longitude", main = "2013 Snow Crab Survey SENS Sample Points", ylab="Latitude", outer = T, axes=T, tck=0,
             tckLab=F, xlim = xlim, ylim = ylim, quality = 1, cellcount = NULL)
  
  #title(main="2013 Snow Crab Survey SENS Sample Points", line=1, cex.main = 15)
  
  addRaster(file.path(direct,"mapping", "maps","Charts", "clip_WGS84_4013.tif"), transpar = "white",quality = 1)
  addRaster(file.path(direct,"mapping", "maps","Charts", "clip_WGS84_4045.tif"), quality = 1)
  addRaster(file.path(direct,"mapping", "maps","Charts", "clip_WGS84_4015.tif"), quality = 1)
  addRaster(file.path(direct,"mapping", "maps","Charts", "clip_WGS84_4003.tif"), semitranspar = T, transpar = "green", quality = 1)
  
  x = NULL
  
  addlinesSCAREA()
  addMPA()
  
  surveydata = read.csv(file.path(direct,"Survey","surveypoints.csv"))
  surveydata = surveydata[-which(is.na(surveydata$Station)),]
  names(surveydata) = c("PID", "Y", "X", "Y2", "X2", "ID", "col" )

  
  surveydata$col[which(surveydata$col == 1)] = "green"
  surveydata$col[which(surveydata$col == 2)] = "yellow"
  surveydata$col[which(surveydata$col == 3)] = "red"
  
  
  
  surveydata$X = surveydata$X*-1
  surveydata$X2 = surveydata$X2*-1
  
  ind = which(surveydata$X < min(xlim) )
  
  if(length(ind)>0) surveydata = surveydata[-ind,]
  
  ind = which(surveydata$X > max(xlim)) 
  
  if(length(ind)>0) surveydata = surveydata[-ind,]
  
  ind = which(surveydata$Y > max(ylim))
  
  if(length(ind)>0) surveydata = surveydata[-ind,]
  
  ind = which(surveydata$Y < min(ylim) )
  
  if(length(ind)>0) surveydata = surveydata[-ind,]
  
  sd = as.PolyData(data.frame(surveydata), projection = "LL")
  

  addPoints(sd, cex = .6, lwd = 30, col = "black")
  addPoints(sd, cex = .5, lwd = 30, col = sd$col)
  addPoints(sd, cex = .05, lwd = 4, col = "black")
  
  sdt = sd
  
  ind = which(is.na(sdt$X2))
  
  if(length(ind)>0) sdt = sdt[-ind,]
  
  bea = bearing(matrix(sdt$X, sdt$Y, ncol = 2, nrow = length(sdt$X)), matrix(sdt$X2, sdt$Y2, ncol = 2, nrow = length(sdt$X2)))
  
  dest = destPoint(as.matrix(data.frame(sdt$X, sdt$Y)), bea, 800, r = 6378137 )
  dest = as.data.frame(dest)
  ind = which(is.na(dest$lon))
  if(length(ind)>0){ 
    dest = dest[-ind,]
    sdt = sdt[-ind,]                  
  }
  new = NULL
  j = 1
  for(i in 1:nrow(dest)){
    new$PID[j] = i
    new$X[j] = sdt$X[i]
    new$Y[j] = sdt$Y[i]
    new$POS[j] = 1
    j = j + 1
    
    new$PID[j] = i
    new$X[j] = dest$lon[i]
    new$Y[j] = dest$lat[i]
    new$POS[j] = 2
    j = j + 1
  }

  dire = as.PolySet(data.frame(new), projection = "LL")
  addLines(dire, lwd = 3, col = "blue")
  
  
  sd$label = as.character(sd$PID)
  
  for(i in 1:nrow(sd)){
    if(nchar(sd$label[i]) == 1) sd$label[i] = paste("00", sd$label[i], sep = "")
    if(nchar(sd$label[i]) == 2) sd$label[i] = paste("0", sd$label[i], sep = "")
  }
  
  sd$X = sd$X+.04
  
  addLabels(sd, font = 2, cex = .4, col = "white")
  sd$X = sd$X+.001
  addLabels(sd, font = 2, cex = .4, col = "white")
  sd$X = sd$X-.002
  addLabels(sd, font = 2, cex = .4, col = "white")
  sd$X = sd$X+.001
  sd$Y = sd$Y+.001
  addLabels(sd, font = 2, cex = .4, col = "white")
  sd$Y = sd$Y-.002
  addLabels(sd, font = 2, cex = .4, col = "white")
  sd$Y = sd$Y+.001
  addLabels(sd, font = 1, cex = .4, col = "black")
  
  scalebar(37.04, xy=c(-58.25, 46.0), type = 'bar', divs = 4, lonlat = T, below = "Nautical miles", label = c("0", "10", "20"), cex = 1, lwd = 3)
  scalebar(37.04, xy=c(-59.0, 43.25), type = 'bar', divs = 4, lonlat = T, below = "Nautical miles", label = c("0", "10", "20"), cex = 1, lwd = 3)
  scalebar(37.04, xy=c(-63.5, 43.5), type = 'bar', divs = 4, lonlat = T, below = "Nautical miles", label = c("0", "10", "20"), cex = 1, lwd = 3)
  degAxis(1, lwd = 5)
  degAxis(2, lwd = 5)
  dev.off()
}
makechartNENS = function(){
  direct = "C://snowcrab"
  jpeg(filename = file.path(direct, "Survey","sample_nenssurvey_2013.jpeg"), width = 5550*2, height = 5550*2, pointsize = 90)
  xlim = c(-60.48, -58.3)
  ylim = c(45.9, 47.4)
  plotRaster(file.path(direct,"mapping", "maps","Charts", "clip_WGS84_NENS4015.tif"),
             xlab="Longitude", main = "2013 Snow Crab Survey NENS Sample Points", ylab="Latitude", outer = T, axes=T, tck=0,
             transpar = "green", semitranspar = T, tckLab=F, xlim = xlim, ylim = ylim, quality = 1, cellcount = NULL)
  
 
  
 

  
  addlinesSCAREA()
  addStAnnsMPA()
  
  surveydata = read.csv(file.path(direct,"Survey","surveypoints.csv"))
  surveydata = surveydata[-which(is.na(surveydata$Station)),]
  names(surveydata) = c("PID", "Y", "X", "Y2", "X2", "ID", "col" )
  surveydata$col[which(surveydata$col == 1)] = "green"
  surveydata$col[which(surveydata$col == 2)] = "yellow"
  surveydata$col[which(surveydata$col == 3)] = "red"
  
  surveydata$X = surveydata$X*-1
  surveydata$X2 = surveydata$X2*-1
  
  ind = which(surveydata$X < min(xlim) )

  if(length(ind)>0) surveydata = surveydata[-ind,]
  
  ind = which(surveydata$X > max(xlim)) 

  if(length(ind)>0) surveydata = surveydata[-ind,]
  
  ind = which(surveydata$Y > max(ylim))

  if(length(ind)>0) surveydata = surveydata[-ind,]
  
  ind = which(surveydata$Y < min(ylim) )

  if(length(ind)>0) surveydata = surveydata[-ind,]

  sd = as.PolyData(data.frame(surveydata), projection = "LL")
  addPoints(sd, cex = 1.1, lwd = 30, col = "black")
  addPoints(sd, cex = 1, lwd = 30, col = sd$col)
  addPoints(sd, cex = .1, lwd = 4, col = "black")
  
  sdt = sd

  ind = which(is.na(sdt$X2))
  
  if(length(ind)>0) sdt = sdt[-ind,]
  
  bea = bearing(matrix(sdt$X, sdt$Y, ncol = 2, nrow = length(sdt$X)), matrix(sdt$X2, sdt$Y2, ncol = 2, nrow = length(sdt$X2)))

dest = destPoint(as.matrix(data.frame(sdt$X, sdt$Y)), bea, 300, r = 6378137 )
dest = as.data.frame(dest)
  new = NULL
  j = 1
  for(i in 1:nrow(dest)){
    new$PID[j] = i
    new$X[j] = sdt$X[i]
    new$Y[j] = sdt$Y[i]
    new$POS[j] = 1
    j = j + 1
        
    new$PID[j] = i
    new$X[j] = dest$lon[i]
    new$Y[j] = dest$lat[i]
    new$POS[j] = 2
    j = j + 1
  }

  dire = as.PolySet(data.frame(new), projection = "LL")
  addLines(dire, lwd = 3, col = "blue")
  
  sd$label = as.character(sd$PID)
  
  
  for(i in 1:nrow(sd)){
    if(nchar(sd$label[i]) == 1) sd$label[i] = paste("00", sd$label[i], sep = "")
    if(nchar(sd$label[i]) == 2) sd$label[i] = paste("0", sd$label[i], sep = "")
  }

  sd$X = sd$X+.02
  
  addLabels(sd, font = 2, cex = 1, col = "white")
  sd$X = sd$X+.001
  addLabels(sd, font = 2, cex = 1, col = "white")
  sd$X = sd$X-.002
  addLabels(sd, font = 2, cex = 1, col = "white")
  sd$X = sd$X+.001
  sd$Y = sd$Y+.001
  addLabels(sd, font = 2, cex = 1, col = "white")
  sd$Y = sd$Y-.002
  addLabels(sd, font = 2, cex = 1, col = "white")
  sd$Y = sd$Y+.001
  addLabels(sd, font = 1, cex = 1, col = "black")
  
  scalebar(37.04, xy=c(-59.25, 46.6), type = 'bar', divs = 4, lonlat = T, below = "Nautical miles", label = c("0", "10", "20"), cex = 3, lwd = 3)

  degAxis(1, lwd = 5)
  degAxis(2, lwd = 5)
 dev.off()
}

############____
#AMC tagging

makechartNENStag = function(){
  direct = "C://snowcrab"
 jpeg(filename = file.path(direct, "tracking2013.jpeg"), width = 5550*2, height = 5550*2, pointsize = 90)
  xlim = c(-60.48, -59)
  ylim = c(46, 47)
  plotRaster(file.path(direct,"mapping", "maps","Charts", "clip_WGS84_NENS4015.tif"),
             xlab="Longitude", main = "Fishery Footprint", ylab="Latitude", outer = T, axes=T, tck=0,
             transpar = "green", semitranspar = T, tckLab=F, xlim = xlim, ylim = ylim, quality = 1, cellcount = NULL)
  
  
  
  addFisheryFootprint(direct=direct)
  addEmera(direct=direct)
  addDec13Track(direct=direct)
  l1 <- data.frame(Y=c(46.55246,46.5833),X=c(-60.11458,-60.25783),EID=1:2)
  sd = as.EventData(l1, projection = "LL")
  addPoints(sd, cex = 2, col = "red",pch=16)
 # scalebar(37.04, xy=c(-59.5, 46.1), type = 'bar', divs = 4, lonlat = T, below = "Nautical miles", label = c("0", "10", "20"), cex = 3, lwd = 3)
  degAxis(1, lwd = 2)
  degAxis(2, lwd = 2)
 dev.off()
}

addDec13Track <- function(direct=direction) {
	emera=importShapefile(file.path(direct,"mapping","OTNTrackingPolys","Dec2013","TagHunt.shp"))
	  rc = col2rgb("red")

        addLines(emera,col =  "black",lwd=10)



}


addVMSTrack <- function(dat) {
 	#from getVMS.R
 	#AMCook June 14, 2013 01:59:26 PM 
 	names(dat)[3:4] <-  c('X','Y')
 	tr <- unique(dat$trip_id) 
 	vr <- data.frame(vrn=unique(dat$vrn),col=1:length(unique(dat$vrn)))
 	  for(i in 1:length(tr)) {
  		db <- dat[dat$trip_id==tr[i],]
  		db$PID <- i
  		db$POS <- 1:nrow(db)
  		attr(db,'projection') <- "LL"
  		addLines(db,col=vr[vr$vrn==unique(db$vrn),2])
          }
       }



