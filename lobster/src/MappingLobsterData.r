
loadfunctions("lobster")


##### From Scallop Survey

# get lobster data from scallop survey
lobdat<-LAFSS(SPA=c("6A","6B","6C"))

#
ScallopAreas<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","NewAreaDefsforISAREADEFS2013.csv"))


pdf(file.path( project.datadirectory("lobster"), "R","SPA6LobsterDensity.pdf"),8,11)

for(i in 2005:2014){
	
	# interpolate abundance
	lob.contours<-interpolation(subset(lobdat,YEAR==i,c('TOW_SEQ','lon','lat','NLobsStd')),ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.03)

	# define contour lines
	print(lob.contours$str.def)
	lvls=c(1, 2, 5, 10, 20, 50)

	# generate contour lines
	cont.lst<-contour.gen(lob.contours$image.dat,lvls,col="YlGn",colorAdj=1)

	# plot Map
	LobsterMap(ylim=c(44.4,45.2),xlim=c(-67.2,-66.3),mapRes="UR",contours=cont.lst,title=paste("SPA 6 Lobster Density",i),isobath=seq(10,500,10),bathcol=rgb(0,0,1,0.2),bathy.source='bathy',boundaries='scallop',poly.lst=list(ScallopAreas,data.frame(PID=c(16,18))))
	points(lat~lon,lobdat,subset=YEAR==i,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
	ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bty='n')
}
dev.off()


	LobsterMap(ylim=c(44.4,45.2),xlim=c(-67.2,-66.3),mapRes="UR",isobath=seq(10,500,10),bathcol=rgb(0,0,1,0.2),bathy.source='bathy',boundaries='scallop',poly.lst=list(ScallopAreas,data.frame(PID=c(16,18))))
