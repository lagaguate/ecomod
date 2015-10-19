

### season timing

pdf(file.path( project.datadirectory("lobster"), "R","SPA6SurveyTiming.pdf"),8,11)
par(mfrow=c(5,2),mar=c(0,0,0,0))
for(i in 2005:2014){
	fishing.season(subset(lobdat,YEAR==i,c('TOW_DATE','NLobsStd')),smooth=0.05,title='')
}
mtext("Relative catch of lobsters in SPA 6 scallop survey",3,-2,cex=1.2,outer=T)	

dev.off()

### poster

loadfunctions('lobster')
logsInSeason<-read.csv(file.path( project.datadirectory("lobster"), "data","logsInSeason.csv"))
logsInSeason$WEIGHT_BUMP<-logsInSeason$WEIGHT_KG*logsInSeason$BUMPUP

catchgrids<-lobGridPlot(subset(logsInSeason,SYEAR==2014,c("LFA","GRID_NUM","WEIGHT_BUMP")),lvls=c(100,50000,100000,200000,400000,600000,800000,1000000),FUN=sum,border=NA)
	
pdf(file.path( project.datadirectory("lobster"), "R","SpatialLandings2014.pdf"),11,9)

LobsterMap(poly.lst=catchgrids[1:2],title="2014 Lobster Catch LFA 27-38")
ContLegend("bottomright",lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (t)",inset=0.02,cex=0.8,bg='white')

dev.off()
lfas<-c("27", "28", "29", "30", "31A", "31B", "32", "33", "34", "35", "36", "38")
#daily.dat<-CPUEplot(logsInSeason,lfa=lfas,yrs=2001:2014)

pdf(file.path( project.datadirectory("lobster"), "R","CPUE.pdf"),9,11)
daily.dat<-CPUEplot(logsInSeason,lfa=lfas[1:6],yrs=2001:2014)


daily.dat<-CPUEplot(logsInSeason,lfa=lfas[7:12],yrs=2001:2014)
dev.off()

