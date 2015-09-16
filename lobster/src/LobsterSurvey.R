

	loadfunctions('lobster')
	lobster.db("survey")

	RLibrary("CircStats","PBSmapping")


	# add column for tow length
	lat1<-surveyCatch$SET_LAT
	lat2<-surveyCatch$HAUL_LAT
	lon1<-surveyCatch$SET_LONG
	lon2<-surveyCatch$HAUL_LONG
	surveyCatch$LENGTH<-6371.3*acos(cos(rad(90-lat1))*cos(rad(90-lat2))+sin(rad(90-lat1))*sin(rad(90-lat2))*cos(rad(lon1-lon2)))

	# add column for LFA
	LFAs<-read.csv(file.path(project.datadirectory('lobster'),'data','maps','Polygons_LFA.csv'))
	SurvLocs<-subset(surveyCatch,select=c("SET_ID","SET_LONG","SET_LAT"))
	names(SurvLocs)[2:3]<-c("X","Y")
	SurvLocs$EID<-1:nrow(SurvLocs)
	key<-findPolys(SurvLocs,LFAs)
	SurvLFAs<-merge(subset(SurvLocs,select=c("EID","SET_ID")),merge(key,subset(LFAs,!duplicated(PID),c("PID","LFA"))))
	surveyCatch<-merge(surveyCatch,subset(SurvLFAs,!duplicated(SET_ID),c("SET_ID","LFA")),all=T)

	# select for Lobsters
	setNames<-c("SET_ID", "TRIP_ID", "TRIPCD_ID", "SURVEY_TYPE", "CFV", "VESSEL_NAME", "LICENSE_NO", "BOARD_DATE", "LANDING_DATE","HAULCCD_ID", "SET_NO", "FISHSET_ID",
	        "STATION", "STRATUM_ID", "SET_LAT", "SET_LONG", "SET_DEPTH", "SET_TIME", "SET_DATE", "HAUL_LAT", "HAUL_LONG", "HAUL_DEPTH", "HAUL_TIME", 
	        "HAUL_DATE", "YEAR", "LENGTH", "LFA")           
	surveyLobsters<-merge(subset(surveyCatch,SPECCD_ID==2550),subset(surveyCatch,!duplicated(SET_ID),setNames),all=T)

	# number of lobsters with detailed data
	NLM<-with(surveyMeasurements,tapply(SET_ID,SET_ID,length))
	surveyLobsters<-merge(surveyLobsters,data.frame(SET_ID=names(NLM),LOBSTERS_MEASURED=NLM),all=T)

	# save list of tows with length outliers
	write.csv(subset(surveyLobsters,(LENGTH<0.67|LENGTH>3.5)&LFA==34&HAULCCD_ID==1&YEAR>1995),file.path(project.datadirectory('lobster'),'data',"longTows34.csv"),row.names=F)

	# add columns  NUM_STANDARDIZED and MONTH
	surveyLobsters$NUM_CORRECTED[is.na(surveyLobsters$NUM_CORRECTED)]<-0
	surveyLobsters$LENGTH[surveyLobsters$LENGTH<0.67|surveyLobsters$LENGTH>3.5]<-NA
	surveyLobsters$NUM_STANDARDIZED<-surveyLobsters$NUM_CORRECTED/surveyLobsters$LENGTH
	surveyLobsters$MONTH<-as.character(month(surveyLobsters$BOARD_DATE,T))
	surveyLobsters$AREA_SWEPT<-surveyLobsters$LENGTH*1000*17
	surveyLobsters$LobDen<-surveyLobsters$NUM_CORRECTED/surveyLobsters$AREA_SWEPT*1000

	# add columns for length bins
	bins<-seq(0,220,5)
	sets<-unique(surveyMeasurements$SET_ID)
	CLF<-data.frame(SET_ID=sets,t(sapply(sets,function(s){with(subset(surveyMeasurements,SET_ID==s&FISH_LENGTH>=min(bins)&FISH_LENGTH<max(bins)),hist(FISH_LENGTH,breaks=bins,plot=F)$count)})))
	names(CLF)[-1]<-paste0("CL",bins[-1])
	surveyLobsters<-merge(surveyLobsters,CLF,all=T)
	surveyLobsters[,which(names(surveyLobsters)%in%names(CLF)[-1])]<-sweep(surveyLobsters[,which(names(surveyLobsters)%in%names(CLF)[-1])],1,FUN="/", surveyLobsters$LENGTH)

	## berried females
	with(subset(surveyMeasurements,SEX==3),tapply(SEX,SET_ID,length))->bfs
	with(subset(surveyMeasurements,SEX==2),tapply(SEX,SET_ID,length))->fs
	with(subset(surveyMeasurements,SEX==1),tapply(SEX,SET_ID,length))->ms
	with(subset(surveyMeasurements,SEX>0),tapply(SEX,SET_ID,length))->all
	sets<-merge(data.frame(SET_ID=names(all),all),merge(data.frame(SET_ID=names(ms),ms),merge(data.frame(SET_ID=names(fs),fs),data.frame(SET_ID=names(bfs),bfs),all=T),all=T),all=T)
	sets$bfs[is.na(sets$bfs)]<-0
	sets$fs[is.na(sets$fs)]<-0
	sets$ms[is.na(sets$ms)]<-0
	surveyLobsters<-merge(surveyLobsters,sets,all=T)
	surveyLobsters$BERRIED_FEMALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$bfs/surveyLobsters$all)
	surveyLobsters$FEMALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$fs/surveyLobsters$all)
	surveyLobsters$MALES<-surveyLobsters$NUM_STANDARDIZED*(surveyLobsters$ms/surveyLobsters$all)

	write.csv(surveyLobsters,file.path(project.datadirectory('lobster'),'data',"surveyLobsters.csv"),row.names=F) # Save data as csv
	
