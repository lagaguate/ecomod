# script for background map with bathymetry


ClamMap<-function(area = 'custom', ylim = c(41,47), xlim = c(-68, -57), title = '',
                  banks = F, nafo = NULL, boundries = 'offshore', isobath = 'quick',
                  points.lst = NULL, lines.lst = NULL, poly.lst = NULL, 
                  image.lst = NULL, color.fun = cm.colors, color.adj = c(1,100),
                  zlim = NA, res = 'high', bathcol = rgb(0, 0, 1, 0.5), grid = NULL){
  
  
  require(PBSmapping)|| stop("Install PBSmapping Package")
  require(fields)
#MMM Sept 2014 - modified path for ecomod 
wd<-file.path(project.datadirectory('polygons'),'data')
  
  # Mapping
  if(area=='custom')	{ ylim=ylim; 			 xlim=xlim			}
  if(area=='SS')		{ ylim=c(41, 47); 	 xlim=c(-68,    -57)		}
  if(area=='ESS')		{ ylim=c(43, 45.4);  xlim=c(-62.5,  -57.4)	}
  if(area=='WSS')		{ ylim=c(41, 44); 	 xlim=c(-67,    -64)		}
  if(area=='BANQ')  { ylim=c(44, 45.25); xlim=c(-60.083, -57) }
 #MMM Sept 2014 - modified path for ecomod  
 if(res=='high')land <- read.table(find.ecomod.gis("martimesHIGH.ll"), header=T)
  attr(land, "projection") <- "LL"
  
  plotMap(land, col='wheat', xlim=xlim, ylim=ylim)
  
  # Image
  if(!is.null(image.lst)){
    a <- color.adj[1]
    b <- color.adj[2]
    zmin <- 0
    if(image.lst=='bathymetry'){
      #source("common/src/_Rfunctions/bathymetry/get.bathy.r", local=T)
      cat("after2")
      get.bathy('custom', xl=c(xlim[1] - 0.5, xlim[2] + 0.5),
                yl=c(ylim[1] - 0.5, ylim[2] + 0.5))
      #			bathy.lst <- makeTopography(bathy.dat)
      bathy.lst <- makeTopography(bathy)
      image(bathy.lst, add=T, col=rainbow(1000)[1:a], zlim=c(1, 500))
      image(bathy.lst, add=T, col=rainbow(1000)[a+1:1000], zlim=c(500, 10000))
      
    }
    else {
      if(is.na(zlim))zlim <- range(image.lst$z, na.rm=T)
      
      image(image.lst$x, image.lst$y, image.lst$z, add=T, col=color.fun(100)[a:b], zlim=zlim)
    }
  }
  
  # plot polygons
  if(!is.null(poly.lst)){
    addPolys(poly.lst[[1]], polyProps=poly.lst[[2]])
  }
  
  
  
  # Bathymetry
  
  if(isobath[1]!='none'&&isobath[1]!='quick'){
    
   # source("common/src/_Rfunctions/bathymetry/get.bathy.r", local=T)
    get.bathy('custom', xl=c(xlim[1]-0.5, xlim[2]+0.5), yl=c(ylim[1]-0.5, ylim[2]+0.5))
    bathy.lst <- makeTopography(bathy)
    bathy.cl <- contourLines(bathy.lst, levels=isobath)
    bathy.cp <- convCP(bathy.cl)
    bathy.poly <- bathy.cp$PolySet
    attr(bathy.poly,"projection") <- "LL"
    addLines(bathy.poly, col=bathcol)
  }
  
  
  if(isobath[1]=='quick'){
    bathcol <- rep(bathcol, length = 4)
    if (as.numeric(xlim[2]) < -50.){        ## Scotian Shelf
      d50.ll<- read.table(find.ecomod.gis("d50.ll"), header=T)
      d50.ll<-na.omit(d50.ll)
      d100.ll<- read.table(find.ecomod.gis("d100.ll"), header=T)
      d100.ll<-na.omit(d100.ll)
      d150.ll<- read.table(find.ecomod.gis("d150.ll"), header=T)
      d150.ll<-na.omit(d150.ll)
      d200.ll<- read.table(find.ecomod.gis("d200.ll"), header=T)
      d200.ll<-na.omit(d200.ll)
      
      
      attr(d50.ll,"projection") <- "LL"
      attr(d100.ll,"projection") <- "LL"
      attr(d150.ll,"projection") <- "LL"
      attr(d200.ll,"projection") <- "LL"
      
      bathcol<-rep(bathcol,length=4)
      addLines(d50.ll, col=bathcol[1])
      addLines(d100.ll, col=bathcol[2])
      addLines(d150.ll, col=bathcol[3])
      addLines(d200.ll, col=bathcol[4])
    }else{     
      chs50.ll<- read.table(find.ecomod.gis("CHS50.ll"), header=T)
      chs50.ll<-na.omit(chs50.ll)
      chs100.ll<- read.table(find.ecomod.gis("CHS100.ll"), header=T)
      chs100.ll<-na.omit(chs100.ll)
      chs200.ll<- read.table(find.ecomod.gis("CHS200.ll"), header=T)
      chs200.ll<-na.omit(chs200.ll)
      
      attr(chs50.ll,"projection") <- "LL"
      attr(chs100.ll,"projection") <- "LL"
      attr(chs200.ll,"projection") <- "LL"
      
      addLines(chs50.ll, col=bathcol[1])
      addLines(chs100.ll, col=bathcol[2])
      addLines(chs200.ll, col=bathcol[4])
    }
  }
  
  # NAFO
  if(!is.null(nafo)){
    
    
    nafo.xy <- read.csv(find.ecomod.gis("nafo.csv"), header=T)
    if(nafo=='all')nafo <- unique(nafo.xy$label)
    nafo.dat <- calcCentroid(subset(nafo.xy, label%in%nafo))
    nafo.dat$label <- unique(nafo.xy$label)
    
    addPolys(nafo.xy,border='grey')
    addLabels(nafo.dat, col=rgb(0.5,0.5,0.5,0.5), cex=2)
  }
  
  # Bank Names
  if(banks){
    
    bankNames.xy <- read.csv(find.ecomod.gis("bankNames.csv"))
    addLabels(bankNames.xy, col=rgb(0.5,0.5,0.5,0.85), cex=2)
  }
  
  
  # Boundaries
  #MMM Sept 2013 - these removed, as I don't have the polygons
#  if(boundries=='offshore'){
#    
#    
#    icj <- read.table("Y:/Maps/data/icj.ll", header=T)
#    GBab <- read.table("Y:/Maps/data/GB/GBab.ll", header=T)
#    GBa5zejm <- read.table("Y:/Maps/data/GB/GBa5zejm.ll", header=T)
#    inoffdiv <- read.table("Y:/Maps/data/inoffdiv.ll", header=T)
#    BBns <- data.frame(lon=c(-65.62486,-65.975), lat=c(43,42.308333))
#    
#    lines(Y~X, data=inoffdiv, lwd=2) 
#    lines(lat~lon, data=GBab)
#    lines(lat~lon, data=BBns) 
#    lines(lat~lon, data=icj, lty=4, lwd=3)
#    
#  }
#  
#  if(boundries=='inshore'){
#    
#    
#    icj<-read.table("Y:/Maps/data/icj.ll", header=T)
#    inoffdiv<-read.table("Y:/Maps/data/inoffdiv.ll", header=T)
#    
#    lines(Y~X,data=inoffdiv, lwd=2) 
#    lines(lat~lon, data=icj, lty=4, lwd=3)
#    
#  }
  
  addPolys(land, col='wheat')
  
  
  
  # plot points
  if(!is.null(points.lst)){
    addPoints(points.lst[[1]], polyProps=points.lst[[2]])
  }
  
  # plot lines
  if(!is.null(lines.lst)){
    addLines(lines.lst[[1]], polyProps=lines.lst[[2]])
  }
  
  # add grid lines
  if(!is.null(grid)){
    x <- seq(floor(xlim[1]), ceiling(xlim[2]),grid)
    y <- seq(floor(ylim[1]), ceiling(ylim[2]),grid)
    gridlines<-makeGrid(x, y, byrow=TRUE, addSID=TRUE, projection="LL", zone=NULL)
    addLines(gridlines, col='grey80', lwd=0.5)
  }
  
  box(lwd=2)
  
  
  title(main=title)
  
  
}
