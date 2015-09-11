gridPlot <- function(Data,domain.poly,lvls,bcol="YlGnBu",border=1,FUN=mean,grid.size=1/60) {  

	require(PBSmapping)
	require(RColorBrewer)
	names(Data)[1:4]<-c("EID","X","Y","Z")
	
	Data <- subset(Data,EID%in%findPolys(Data,domain.poly)$EID)
#	grid   <- makeGrid(x=seq(floor(min(Data$X))-0.5/60,ceiling(max(Data$X))+0.5/60,1/60),y=seq(floor(min(Data$Y))-0.5/60,ceiling(max(Data$Y))+0.5/60,1/60),projection="LL")
	grid   <- makeGrid(x=seq(floor(min(Data$X)),ceiling(max(Data$X)),grid.size),y=seq(floor(min(Data$Y)),ceiling(max(Data$Y)),grid.size),projection="LL")
	
	# locate EventData in grid 
	locData<- findCells(Data, grid) 
	pdata  <- combineEvents(Data, locData, FUN=FUN)
	#browser()
	lvls<-c(lvls,max(lvls)*100)
	cols   <- brewer.pal(length(lvls),bcol) 
	pdata  <- makeProps(pdata, lvls, "col", cols) 
	pdata$border<-border
	
	list(grid, pdata, cols)
	
}