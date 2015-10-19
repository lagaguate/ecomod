RLibrary("lubridate","dplyr","ggplot2")

loadfunctions(c('lobster','groundfish','BIOsurvey'))

##______________________##
##                      ##
##   at Sea sampling    ##
##______________________##
##                      ##

	lobster.db('atSea')

	atSea41<-subset(atSea,LFA==41&SPECIESCODE==2550)
	atSea41$YEAR<-year(atSea41$STARTDATE)
	atSea41$EID<-1:nrow(atSea41)
		
		# Look at the data!
		atSeaCLF<-CLF(subset(atSea41,!is.na(YEAR),c("YEAR","CARLENGTH")))
		BubblePlotCLF(atSeaCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),prop=T,filen="SeaSamplingLFA41",yrs=atSeaCLF$yrs)
		BarPlotCLF(atSeaCLF$CLF,,yrs=atSeaCLF$yrs,col='grey',filen="SeaSamplingLFA41",rel=T,LS=83,rows=9,sample.size=rowSums(atSeaCLF$CLF$CLF))

	### Map Areas 
	LobsterMap('41')		
	LFA41areas<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas.csv"))
	LFA41areasEXT<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas_ext.csv"))
	addPolys(LFA41areas,border='red')
	addPolys(LFA41areasEXT,border='grey')



	### Add Areas 
	events<-na.omit(with(atSea41,data.frame(EID=EID,X=LONGITUDE,Y=LATITUDE)))
	key<-findPolys(events,LFA41areasEXT,maxRows=1e+06)
	atSea41<-merge(atSea41,merge(key[,-3],subset(LFA41areas,!duplicated(PID),c("PID","OFFAREA"))),all=T)

	### Add Season
	atSea41$season<-getSeason(atSea41$STARTDATE)
	atSea41$AreaSeason<-paste(atSea41$OFFAREA,atSea41$season,sep='.')

	### Select females
	atSea41F<-subset(atSea41,SEX>1)

	### calculate average size
	sapply(unique(atSea41$AreaSeason),function(a){with(subset(atSea41F,AreaSeason==a),tapply(CARLENGTH,YEAR,mean,na.rm=T))}) # mean
	sapply(unique(atSea41$AreaSeason),function(a){with(subset(atSea41F,AreaSeason==a),tapply(CARLENGTH,YEAR,median,na.rm=T))}) # median
	medians<-with(subset(atSea41,YEAR%in%(1977:2012)),tapply(CARLENGTH,AreaSeason,median,na.rm=T))
	medians-(medians-95)/2


	medians<-with(atSea41,tapply(CARLENGTH,YEAR,median))
	plot(as.numeric(names(medians)),medians,type='b')
	with(atSea41,tapply(CAPTAIN,YEAR,unique))

	### Map Areas 
	LobsterMap('41',poly.lst=list(LFA41areas,data.frame(PID=1:5,border='red')))		



##______________________##
##                      ##
##       RV Survey      ##
##______________________##
##                      ##


	# Summer Survey index for 4X
	RVS4X.lst<-GroundfishSurveyProcess(Strata=c(477,478,480,481,482,483,484),Years=1980:2015) 
	RVS4Xlgf.lst<-GroundfishSurveyProcess(size.range=c(140,240),Sex=2:3,Strata=c(480:481),Years=2010:2015) 

	mavg(RVS4Xlgf.lst$index)
	
	# Survey index for 4X
	RVS5Z.lst<-GroundfishSurveyProcess(Strata=c('5Z1','5Z2','5Z3','5Z4'),Series=c('georges'),Years=1987:2015) 

	### Map Areas 
	LobsterMap('41')		
	LFA41areas<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas.csv"))
	RVstrata<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","summerstrata.csv"))
	RVstrata$POS<-1:nrow(RVstrata)
	RVstrataLabs<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","summer_strata_labels.csv"))
	LobsterMap('41')		
	addPolys(LFA41areas)
	addPolys(RVstrata,border='red')
	addLabels(RVstrataLabs,col=rgb(1,0,0,0.3),cex=1)

	points(LATITUDE~LONGITUDE,atSea41,pch='.')
	
	RVS4X.lst<-GroundfishSurveyProcess(Strata=c(477,478,480,481,482,483,484),Years=1980:2015) 
	#RVS4X.lst<-GroundfishSurveyProcess(Strata=c(477,478,482,483,484),Years=1980:2015) 

	DougsNumbers<-read.csv("DougsNumbers.csv")

	RVindex<-data.frame(YEAR=1980:2015,RV4X=RVS4X.lst$index,RV4Xse=RVS4X.lst$se)
	RVindex$RV4X[RVindex$YEAR<1999]<-DougsNumbers$Stratified.Mean[DougsNumbers$Year<1999]
	RVindex$RV4Xse[RVindex$YEAR<1999]<-DougsNumbers$Standard.Error[DougsNumbers$Year<1999]
	RVindex$MVAvg3<-mavg(RVindex$RV4X)

	bgcol<-rep('darkblue',nrow(RVindex))
	bgcol[which(RVindex$YEAR%in%(1995:1997))]<-'grey'
	bgcol[which(RVindex$YEAR%in%c(2004,2007))]<-'red'

	pdf("LFA41updateFig2.pdf",8,6)
	with(RVindex,plot(YEAR,RV4X,pch=21,col='lightblue',bg=bgcol,xlab='',ylab='Mean # / Tow',las=1,ylim=c(0,max(RV4X+RV4Xse,na.rm=T))))
	with(RVindex,arrows(YEAR, RV4X+RV4Xse, YEAR, RV4X-RV4Xse ,code=3,angle=90,length=0.03))
	with(RVindex,points(YEAR,RV4X,pch=21,col='lightblue',bg=bgcol))
	with(RVindex,lines(YEAR[-1],MVAvg3[-length(YEAR)],lty=3,col='blue'))
	lines(1995:2015,rep(1.48,length(1995:2015)),lty=3,col='green')
	lines(1995:2015,rep(0.16,length(1995:2015)),lty=3,col='red')
	legend('topleft',c("Mean #/Tow","3yr Moving Average (Mean #/Tow)","50% Median 1995-09","40% Median 1983-94"),pch=c(21,NA,NA,NA),pt.bg='darkblue',col=c('lightblue','blue','green','red'),lty=c(NA,3,3,3),bty='n',inset=0.02)
	dev.off()




