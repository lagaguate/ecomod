gridData <- function(Data,domain.poly,lvls,bcol="YlGnBu",border=1,FUN=mean,grid.size=1/60,projection="LL") {  

	require(PBSmapping)
	require(RColorBrewer)
	names(Data)[1:4]<-c("EID","X","Y","Z")


	if(projection=="UTM")	{
		attr(Data,"projection")="LL"
		Data <- convUL(Data)
	}
	if(!missing(domain.poly))Data <- subset(Data,EID%in%findPolys(Data,domain.poly)$EID)
	grid   <- makeGrid(x=seq(floor(min(Data$X)),ceiling(max(Data$X)),grid.size),y=seq(floor(min(Data$Y)),ceiling(max(Data$Y)),grid.size),projection=projection)
	
	if(projection=="UTM")	{
		grid <- convUL(grid)
		Data <- convUL(Data)
	}
	
	# locate EventData in grid 
	locData<- findCells(Data, grid) 
	pdata  <- combineEvents(Data, locData, FUN=FUN)
	#browser()
	cols   <- brewer.pal(length(lvls),bcol) 
	pdata  <- makeProps(pdata, c(lvls,max(lvls)*100), "col", cols) 
	pdata$border<-border
	
	return(list(polys=grid, polyData=pdata, lvls=lvls, col=cols))
	
}