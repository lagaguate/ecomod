#	source("fn/LobsterMap.r")

### MAPS Lobster FISHING AREAS IN R!

# ARGUMENTS
# area = 'custom' where xlim & ylim are specified or select from area list below
# mapRes = coastline detail ('LR' = low resolution, 'MR' = medium resolution, 'HR' = high resolution, 'UR' = ultra resolution)
# title = plot title
# boundaries = for ploting specific management boundaries
# isobath = plots bathymetry lines for specified depths from topex data 
# bathcol = isobath line color, default is transparent blue
# topolines = plots topographic lines for specified elevations from topex data 
# bathcol = topolines line color, default is transparent brown
# points.lst = points to overlay on map in PBSmapping format - list with 2 elements: 1st element is eventSet (EID, POS, X, Y), 2nd element is eventData (EID, pch, col, etc.) 
# lines.lst = lines to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, lty, col, etc.) 
# poly.lst = polygons to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, border, col, etc.) 
# contours = plots overlaping polygons as contours (same format as poly.lst)
# image.lst = image to overlay on map - list with 3 elements (x, y, z), 'bathymetry' produces image from bathymetry data 
# color.fun = color function for image
# zlim = zlim for image
# grid = size of grid in degrees, default is no grid
# stippling = adds stippling to land (purely for visual effect)
# lol = adds water colored border to coastline (purely for visual effect)

LobsterMap<-function(area='custom',ylim=c(42.5,48),xlim=c(-67.4,-57.8),mapRes='HR',land.col='wheat',title='',nafo=NULL,boundaries='LFAs', bathy.source='topex', isobaths=c(seq(10,100,10),seq(100,1000,100)),bathcol=rgb(0,0,1,0.1),topolines=NULL,topocol=rgb(0.8,0.5,0,0.2),points.lst=NULL,lines.lst=NULL,poly.lst=NULL,contours=NULL,image.lst=NULL,color.fun=tim.colors,zlim,grid=NULL,stippling=F,lol=F,gridlab=F,...){

		
	require(PBSmapping)|| stop("Install PBSmapping Package")
	require(fields)|| stop("Install fields Package")
	
	# Custom area
	if(area=='custom')	{ ylim=ylim; 			xlim=xlim			}
	
	## Area List
	if(area=='all')		{ ylim=c(42.5,48); 		xlim=c(-67.4,-57.8)	}
	if(area=='west')	{ ylim=c(42.5,46); 		xlim=c(-67.8,-64)	}
	if(area=='27')		{ ylim=c(44.9,47.9); 	xlim=c(-61,-57.8)	}
	if(area=='28')		{ ylim=c(45.3,46);	 	xlim=c(-61.6,-60.3)	}
	if(area=='29')		{ ylim=c(45.3,46); 		xlim=c(-61.6,-60.3)	}
	if(area=='30')		{ ylim=c(44.6,45.9); 	xlim=c(-60.8,-59.6)	}
	if(area=='31a')		{ ylim=c(44.4,45.7); 	xlim=c(-61.8,-60)	}
	if(area=='31b')		{ ylim=c(44.1,45.3); 	xlim=c(-62.2,-60.5)	}
	if(area=='32')		{ ylim=c(43.8,45);	 	xlim=c(-63.5,-61.5)	}
	if(area=='33')		{ ylim=c(42.5,44.8); 	xlim=c(-65.8,-62.2)	}
	if(area=='34')		{ ylim=c(42.5,45);	 	xlim=c(-67.8,-65)	}
	if(area=='35')		{ ylim=c(44.5,46);	 	xlim=c(-66,-63.2)	}
	if(area=='36')		{ ylim=c(44.5,45.7); 	xlim=c(-67.2,-65)	}
	if(area=='37')		{ ylim=c(44,45);		xlim=c(-67.3,-66.4) }
	if(area=='38')		{ ylim=c(44,45);		xlim=c(-67.3,-66.4) }
	if(area=='40')		{ ylim=c(43.7,45.2); 	xlim=c(-60.5,-57)	}
	if(area=='41')		{ ylim=c(44.5,47.5);	xlim=c(-58,-55)		}
	

	coast<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","gshhs",paste0("shoreline",mapRes,".csv")))
	rivers<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","gshhs",paste0("rivers",mapRes,".csv")))
	attr(coast,"projection")<-"LL"


	#par(...)
	plotMap(coast,xlim=xlim,ylim=ylim,...)
	#addLines(rivers)
	
	if(lol)addPolys(coast,border=bathcol,lwd=6)
	
	# Image
	if(!is.null(image.lst)){
		if(missing(zlim))zlim<-range(image.lst$z,na.rm=T)
		image(image.lst,add=T,col=color.fun(100),zlim=zlim)
	}

	# plot polygons
	if(!is.null(contours)){
		contours[[2]]<-subset(contours[[2]],PID%in%contours[[1]]$PID)
		junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))
		for(i in unique(contours[[2]]$PID)){
			addPolys(joinPolys(subset(contours[[1]],PID==i),junk,operation="DIFF"),polyProps=contours[[2]])
		}
	}
	if(!is.null(poly.lst)){
		addPolys(poly.lst[[1]],polyProps=poly.lst[[2]])
	}
	
	
	# Bathymetry
	
		if(!is.null(isobaths)){
			bath.lst<-list()
			for(i in unique(ceiling(isobaths/1000))){
	 			load(file.path( project.datadirectory("lobster"), "data","maps", bathy.source, paste0("bathyPoly",i,".rdata")))
	 			bath.lst[[i]]<-bathy.poly
	 		}
 			bathy.poly<-do.call(rbind,bath.lst)
 			bathy.poly<-subset(bathy.poly,Z%in%isobaths)
			attr(bathy.poly,"projection") <- "LL"
			addLines(bathy.poly,polyProps=data.frame(PID=unique(bathy.poly$PID),col=bathcol))
			#browser()
		}
	
	# NAFO
	if(!is.null(nafo)){
		
        nafo.xy<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","nafo.csv"))
        if(nafo[1]=='all')nafo<-unique(nafo.xy$label)
        nafo.sel<-subset(nafo.xy,label%in%nafo)
        nafo.dat<-merge(calcCentroid(nafo.sel),nafo.sel[c("PID","label")])[!duplicated(nafo.sel[c("PID","label")]),]
        nafo.dat$label[nafo.dat$label=="5ZC"]<-"5ZEM"
        
		addPolys(nafo.xy,border='grey')
		addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=2)
	}
	
	
	
	# Boundries
	if(boundaries=='LFAs'){
		
		LFAgrid<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFAgridPolys.csv"))
		if(area=='31a')area<-31.1
		if(area=='31b')area<-31.2

		if(!is.na(as.numeric(area))){
			lfa<-as.numeric(area)
			grids<-subset(LFAgrid,PID==lfa)
			addPolys(grids)
			if(gridlab){
				grids$label<-grids$SID
        		grids.dat<-merge(calcCentroid(grids),grids[c("PID","SID","label")])[!duplicated(grids[c("PID","SID","label")]),]
				addLabels(grids.dat,col=rgb(0.5,0.5,0.5,0.8),cex=1)
			}
		}
		else {
			addPolys(LFAgrid,border=rgb(0,0,0,0.2))
		}
	}
	if(boundaries=='scallop'){
		
		SFA<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","SFA.csv"))
		addLines(SFA)

		SPA<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","SPA.csv"))
		addPolys(SPA)
	}
		
	EEZ<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","EEZ.csv"))
	addLines(EEZ,lty=4,lwd=2)
	
	# plots land
	addPolys(coast,col=land.col)
	addLines(rivers)
	
	if(stippling)addStipples (coast, pch='.')
	
	# Topography
	
		if(!is.null(topolines)){
			topo.lst<-list()
			for(i in unique(ceiling(topolines/1000))){
	 			load(file.path( project.datadirectory("lobster"), "data", "topex",paste0("topoPoly",i,".rdata")))
	 			topo.lst[[i]]<-topo.poly
	 		}
 			topo.poly<-do.call(rbind,topo.lst)
 			topo.poly<-subset(topo.poly,Z%in%topolines)
			attr(topo.poly,"projection") <- "LL"
			addLines(topo.poly,polyProps=data.frame(PID=unique(topo.poly$PID),col=topocol))
		}
	


	# plot points
	if(!is.null(points.lst)){
		addPoints(points.lst[[1]],polyProps=points.lst[[2]])
	}

	# plot lines
	if(!is.null(lines.lst)){
		addLines(lines.lst[[1]],polyProps=lines.lst[[2]])
	}
	
	# add grid lines
	if(!is.null(grid)){
		x<-seq(floor(xlim[1]),ceiling(xlim[2]),grid)
		y<-seq(floor(ylim[1]),ceiling(ylim[2]),grid)
		gridlines<-makeGrid(x,y,byrow=TRUE,addSID=TRUE,projection="LL",zone=NULL)
		addLines(gridlines,col='grey80',lwd=1)
	}

	box(lwd=2)
	

	title(main=title)
	

}

