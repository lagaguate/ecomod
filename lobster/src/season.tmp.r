

pdf(file.path( project.datadirectory("lobster"), "R","SPA6SurveyTiming.pdf"),8,11)
par(mfrow=c(5,2),mar=c(0,0,0,0))
for(i in 2005:2014){
	fishing.season(subset(lobdat,YEAR==i,c('TOW_DATE','NLobsStd')),smooth=0.05,title='')
}
mtext("Relative catch of lobsters in SPA 6 scallop survey",3,-2,cex=1.2,outer=T)	

dev.off()
