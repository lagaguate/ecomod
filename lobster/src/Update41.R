RLibrary("lubridate","dplyr","ggplot2")

loadfunctions(c('lobster','groundfish','BIOsurvey'))

##______________________##
##                      ##
##   at Sea sampling    ##
##______________________##
##                      ##

	lobster.db('atSea')

	atSea41<-subset(atSea,LFA==41)
	atSea41$YEAR<-year(atSea41$STARTDATE)
	atSea41$EID<-1:nrow(atSea41)

	### Map Areas 
	LobsterMap('41')		
	LFA41areas<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas.csv"))
	LFA41areasEXT<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas_ext.csv"))
	addPolys(LFA41areas)
	addPolys(LFA41areasEXT,border='red')

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


##______________________##
##                      ##
##       RV Survey      ##
##______________________##
##                      ##y


	# Summer Survey index for 4X
	RVS4X.lst<-GroundfishSurveyProcess(Strata=c(477,478,480,481,482,483,484),Years=1980:2015) 

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



