loadfunctions("lobster")

RLibrary("plyr","lattice")

loadfunctions("lobster")

#LATEST DATA EXPORT FROM FSRS DATABASE:
#lobster.db("fsrs.redo")
lobster.db("fsrs")

FSRS.dat<-fsrs
FSRS.dat$VES_DATE<-paste(FSRS.dat$VESSEL_CD,FSRS.dat$HAUL_DATE,sep='.')

FSRS.dat<-subset(FSRS.dat,SOAK_DAYS<6)	# Remove soak days greater than 5
FSRS.dat$HAUL_DATE<-as.Date(FSRS.dat$HAUL_DATE)

## Aggregate by unique vessal and day, summerizing total traps, legals and shorts

trap.lst<-lapply(with(FSRS.dat,tapply(TRAP_NO,VES_DATE,unique)),length)
trap.dat<-data.frame(VES_DATE=names(trap.lst),TOTAL_TRAPS=unlist(trap.lst))

short.lst<-with(subset(FSRS.dat,SHORT==1&SEX%in%(1:2)),tapply(TRAP_NO,VES_DATE,length)) # do not iclude berried females
short.dat<-data.frame(VES_DATE=names(short.lst),SHORTS=short.lst)
legal.lst<-with(subset(FSRS.dat,SHORT==0&SEX%in%(1:2)),tapply(TRAP_NO,VES_DATE,length)) # do not iclude berried females
legal.dat<-data.frame(VES_DATE=names(legal.lst),LEGALS=legal.lst)

FSRS_1.dat<-subset(FSRS.dat,!duplicated(VES_DATE),c("VES_DATE", "VESSEL_CD", "SOAK_DAYS", "HAUL_DATE", "DEPTH", "LFA", "LFA_GRID", "GAUGE", "LATITUDE", "LONGITUDE", "TEMP", "BAIT", "WIND_DIRECTION", "WIND_SPEED", "HAUL_TIME", "LAT_DD", "LONG_DD", "HAUL_YEAR", "S_LABEL"))
FSRS_2.dat<-merge(trap.dat,merge(short.dat,legal.dat,all=T),all=T)
FSRS_2.dat$SHORTS[is.na(FSRS_2.dat$SHORTS)]<-0
FSRS_2.dat$LEGALS[is.na(FSRS_2.dat$LEGALS)]<-0
FSRSvesday.dat<-merge(FSRS_1.dat,FSRS_2.dat,all=T)

str(FSRSvesday.dat)


subareas<-read.csv(file.path( project.datadirectory("lobster"), "data","LFA2733subarea.csv"))
FSRSvesday.dat<-merge(FSRSvesday.dat,subareas,all.x=T)
FSRSvesday.dat$SYEAR<-as.numeric(substr(FSRSvesday.dat$S_LABEL,1,4))

pdf(file.path( project.directory("lobster"), "R","FSRScpue.pdf"))

#---------------------------------------------------------------------------LFA27

	FSRS.LFA27.dat<-subset(FSRSvesday.dat,LFA==27)


	# Create column for week of season (WOS)
	season<-unique(FSRS.LFA27.dat$S_LABEL)
	for(i in 1:length(season)){
		FSRS.LFA27.dat$WOS[FSRS.LFA27.dat$S_LABEL==season[i]]<-floor((FSRS.LFA27.dat$HAUL_DATE[FSRS.LFA27.dat$S_LABEL==season[i]]-min(FSRS.LFA27.dat$HAUL_DATE[FSRS.LFA27.dat$S_LABEL==season[i]]))/7)+1
	}

	#----------------------------------------------------------------------North
	LFA27north.week<-ddply(subset(FSRS.LFA27.dat,subarea=='27 North'),.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

		LFA27north.week$s.cpue=LFA27north.week$SHORTS/LFA27north.week$TRAPS
		LFA27north.week$l.cpue=LFA27north.week$LEGALS/LFA27north.week$TRAPS
		LFA27north.week$logTRAPS=log(LFA27north.week$TRAPS)

		LFA27north.year<-ddply(subset(FSRS.LFA27.dat,subarea=='27 North'),.(LFA,subarea,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

		LFA27north.year$s.cpue=LFA27north.year$SHORTS/LFA27north.year$TRAPS
		LFA27north.year$l.cpue=LFA27north.year$LEGALS/LFA27north.year$TRAPS

		# Generalized linear model for sub-legals
		LFA27north.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA27north.week, family=poisson(link="log"))
		summary(LFA27north.glm1)

			# determine most representative vessel for predictions
			vessels<-unique(LFA27north.week$VESSEL_CD)
			vesselCoefs<-c(0,LFA27north.glm1$coef[2:length(vessels)])
			v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
			
			# predicted number
			newdata<-data.frame(WOS=mean(LFA27north.week$WOS),SYEAR=sort(unique(LFA27north.week$SYEAR)),logTRAPS=mean(LFA27north.week$logTRAPS),VESSEL_CD=vessels[v])
			pred<-predict(LFA27north.glm1,newdata=newdata,se.fit=T)
			pred.total.short<-exp(pred$fit)
			pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
			LFA27north.year$pred.s.cpue<-pred.short.cpue


		# Generalized linear model for legals
		LFA27north.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA27north.week, family=poisson(link="log"))
		summary(LFA27north.glm2)

			# determine most representative vessel for predictions
			vessels<-unique(LFA27north.week$VESSEL_CD)
			vesselCoefs<-c(0,LFA27north.glm2$coef[2:length(vessels)])
			v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
			
			# predicted number
			newdata<-data.frame(WOS=mean(LFA27north.week$WOS),SYEAR=sort(unique(LFA27north.week$SYEAR)),logTRAPS=mean(LFA27north.week$logTRAPS),VESSEL_CD=vessels[v])
			pred<-predict(LFA27north.glm2,newdata=newdata,se.fit=T)
			pred.total.legal<-exp(pred$fit)
			pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
			LFA27north.year$pred.l.cpue<-pred.legal.cpue


		par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
		plot(s.cpue~SYEAR, LFA27north.year,ylim=c(0,4.5),xaxt='n')
		axis(1,lab=F)
		lines(pred.s.cpue~SYEAR, LFA27north.year)
		legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
		plot(l.cpue~SYEAR, LFA27north.year,ylim=c(0,4.5))
		lines(pred.l.cpue~SYEAR, LFA27north.year)
		legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
		mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
		mtext("LFA 27 north",3,1,outer=T,las=0)



	#----------------------------------------------------------------------South
	LFA27south.week<-ddply(subset(FSRS.LFA27.dat,subarea=='27 South'),.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))


		LFA27south.week$s.cpue=LFA27south.week$SHORTS/LFA27south.week$TRAPS
		LFA27south.week$l.cpue=LFA27south.week$LEGALS/LFA27south.week$TRAPS
		LFA27south.week$logTRAPS=log(LFA27south.week$TRAPS)

		LFA27south.year<-ddply(subset(FSRS.LFA27.dat,subarea=='27 South'),.(LFA,subarea,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

		LFA27south.year$s.cpue=LFA27south.year$SHORTS/LFA27south.year$TRAPS
		LFA27south.year$l.cpue=LFA27south.year$LEGALS/LFA27south.year$TRAPS

		# Generalized linear model for sub-legals
		LFA27south.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA27south.week, family=poisson(link="log"))
		summary(LFA27south.glm1)

			# determine most representative vessel for predictions
			vessels<-unique(LFA27south.week$VESSEL_CD)
			vesselCoefs<-c(0,LFA27south.glm1$coef[2:length(vessels)])
			v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
			
			# predicted number
			newdata<-data.frame(WOS=mean(LFA27south.week$WOS),SYEAR=sort(unique(LFA27south.week$SYEAR)),logTRAPS=mean(LFA27south.week$logTRAPS),VESSEL_CD=vessels[v])
			pred<-predict(LFA27south.glm1,newdata=newdata,se.fit=T)
			pred.total.short<-exp(pred$fit)
			pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
			LFA27south.year$pred.s.cpue<-pred.short.cpue


		# Generalized linear model for legals
		LFA27south.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA27south.week, family=poisson(link="log"))
		summary(LFA27south.glm2)

			# determine most representative vessel for predictions
			vessels<-unique(LFA27south.week$VESSEL_CD)
			vesselCoefs<-c(0,LFA27south.glm2$coef[2:length(vessels)])
			v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
			
			# predicted number
			newdata<-data.frame(WOS=mean(LFA27south.week$WOS),SYEAR=sort(unique(LFA27south.week$SYEAR)),logTRAPS=mean(LFA27south.week$logTRAPS),VESSEL_CD=vessels[v])
			pred<-predict(LFA27south.glm2,newdata=newdata,se.fit=T)
			pred.total.legal<-exp(pred$fit)
			pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
			LFA27south.year$pred.l.cpue<-pred.legal.cpue


		par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
		plot(s.cpue~SYEAR, LFA27south.year,ylim=c(0,4.5),xaxt='n')
		axis(1,lab=F)
		lines(pred.s.cpue~SYEAR, LFA27south.year)
		legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
		plot(l.cpue~SYEAR, LFA27south.year,ylim=c(0,4.5))
		lines(pred.l.cpue~SYEAR, LFA27south.year)
		legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
		mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
		mtext("LFA 27 south",3,1,outer=T,las=0)


#---------------------------------------------------------------------------LFA28

	FSRS.LFA28.dat<-subset(FSRSvesday.dat,LFA==28)

	# Create column for week of season (WOS)
	season<-unique(FSRS.LFA28.dat$S_LABEL)
	for(i in 1:length(season)){
		FSRS.LFA28.dat$WOS[FSRS.LFA28.dat$S_LABEL==season[i]]<-floor((FSRS.LFA28.dat$HAUL_DATE[FSRS.LFA28.dat$S_LABEL==season[i]]-min(FSRS.LFA28.dat$HAUL_DATE[FSRS.LFA28.dat$S_LABEL==season[i]]))/7)+1
	}

	LFA28.week<-ddply(FSRS.LFA28.dat,.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA28.week$s.cpue=LFA28.week$SHORTS/LFA28.week$TRAPS
	LFA28.week$l.cpue=LFA28.week$LEGALS/LFA28.week$TRAPS
	LFA28.week$logTRAPS=log(LFA28.week$TRAPS)

	LFA28.year<-ddply(FSRS.LFA28.dat,.(LFA,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA28.year$s.cpue=LFA28.year$SHORTS/LFA28.year$TRAPS
	LFA28.year$l.cpue=LFA28.year$LEGALS/LFA28.year$TRAPS

	# Generalized linear model for sub-legals
	LFA28.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA28.week, family=poisson(link="log"))
	summary(LFA28.glm1)

		# determine most representative vessel for predictions
		vessels<-unique(LFA28.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA28.glm1$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))[1]
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA28.week$WOS),SYEAR=sort(unique(LFA28.week$SYEAR)),logTRAPS=mean(LFA28.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA28.glm1,newdata=newdata,se.fit=T)
		pred.total.short<-exp(pred$fit)
		pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA28.year$pred.s.cpue<-pred.short.cpue


	# Generalized linear model for legals
	LFA28.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA28.week, family=poisson(link="log"))
	summary(LFA28.glm2)

		# determine most representative vessel for predictions
		vessels<-unique(LFA28.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA28.glm2$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))[1]
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA28.week$WOS),SYEAR=sort(unique(LFA28.week$SYEAR)),logTRAPS=mean(LFA28.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA28.glm2,newdata=newdata,se.fit=T)
		pred.total.legal<-exp(pred$fit)
		pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA28.year$pred.l.cpue<-pred.legal.cpue


	par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
	plot(s.cpue~SYEAR, LFA28.year,ylim=c(0,3.2),xaxt='n')
	axis(1,lab=F)
	lines(pred.s.cpue~SYEAR, LFA28.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
	plot(l.cpue~SYEAR, LFA28.year,ylim=c(0,3.2))
	lines(pred.l.cpue~SYEAR, LFA28.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
	mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
	mtext("LFA 28",3,1,outer=T,las=0)


#---------------------------------------------------------------------------LFA29

	FSRS.LFA29.dat<-subset(FSRSvesday.dat,LFA==29)

	# Create column for week of season (WOS)
	season<-unique(FSRS.LFA29.dat$S_LABEL)
	for(i in 1:length(season)){
		FSRS.LFA29.dat$WOS[FSRS.LFA29.dat$S_LABEL==season[i]]<-floor((FSRS.LFA29.dat$HAUL_DATE[FSRS.LFA29.dat$S_LABEL==season[i]]-min(FSRS.LFA29.dat$HAUL_DATE[FSRS.LFA29.dat$S_LABEL==season[i]]))/7)+1
	}

	LFA29.week<-ddply(FSRS.LFA29.dat,.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA29.week$s.cpue=LFA29.week$SHORTS/LFA29.week$TRAPS
	LFA29.week$l.cpue=LFA29.week$LEGALS/LFA29.week$TRAPS
	LFA29.week$logTRAPS=log(LFA29.week$TRAPS)

	LFA29.year<-ddply(FSRS.LFA29.dat,.(LFA,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA29.year$s.cpue=LFA29.year$SHORTS/LFA29.year$TRAPS
	LFA29.year$l.cpue=LFA29.year$LEGALS/LFA29.year$TRAPS

	# Generalized linear model for sub-legals
	LFA29.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA29.week, family=poisson(link="log"))
	summary(LFA29.glm1)

		# determine most representative vessel for predictions
		vessels<-unique(LFA29.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA29.glm1$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA29.week$WOS),SYEAR=sort(unique(LFA29.week$SYEAR)),logTRAPS=mean(LFA29.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA29.glm1,newdata=newdata,se.fit=T)
		pred.total.short<-exp(pred$fit)
		pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA29.year$pred.s.cpue<-pred.short.cpue


	# Generalized linear model for legals
	LFA29.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA29.week, family=poisson(link="log"))
	summary(LFA29.glm2)

		# determine most representative vessel for predictions
		vessels<-unique(LFA29.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA29.glm2$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA29.week$WOS),SYEAR=sort(unique(LFA29.week$SYEAR)),logTRAPS=mean(LFA29.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA29.glm2,newdata=newdata,se.fit=T)
		pred.total.legal<-exp(pred$fit)
		pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA29.year$pred.l.cpue<-pred.legal.cpue


	par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
	plot(s.cpue~SYEAR, LFA29.year,ylim=c(0,3.2),xaxt='n')
	axis(1,lab=F)
	lines(pred.s.cpue~SYEAR, LFA29.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
	plot(l.cpue~SYEAR, LFA29.year,ylim=c(0,3.2))
	lines(pred.l.cpue~SYEAR, LFA29.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
	mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
	mtext("LFA 29",3,1,outer=T,las=0)


#---------------------------------------------------------------------------LFA30

	FSRS.LFA30.dat<-subset(FSRSvesday.dat,LFA==30)

	# Create column for week of season (WOS)
	season<-unique(FSRS.LFA30.dat$S_LABEL)
	for(i in 1:length(season)){
		FSRS.LFA30.dat$WOS[FSRS.LFA30.dat$S_LABEL==season[i]]<-floor((FSRS.LFA30.dat$HAUL_DATE[FSRS.LFA30.dat$S_LABEL==season[i]]-min(FSRS.LFA30.dat$HAUL_DATE[FSRS.LFA30.dat$S_LABEL==season[i]]))/7)+1
	}


	LFA30.week<-ddply(FSRS.LFA30.dat,.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA30.week$s.cpue=LFA30.week$SHORTS/LFA30.week$TRAPS
	LFA30.week$l.cpue=LFA30.week$LEGALS/LFA30.week$TRAPS
	LFA30.week$logTRAPS=log(LFA30.week$TRAPS)

	LFA30.year<-ddply(FSRS.LFA30.dat,.(LFA,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA30.year$s.cpue=LFA30.year$SHORTS/LFA30.year$TRAPS
	LFA30.year$l.cpue=LFA30.year$LEGALS/LFA30.year$TRAPS

	# Generalized linear model for sub-legals
	LFA30.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA30.week, family=poisson(link="log"))
	summary(LFA30.glm1)

		# determine most representative vessel for predictions
		vessels<-unique(LFA30.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA30.glm1$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA30.week$WOS),SYEAR=sort(unique(LFA30.week$SYEAR)),logTRAPS=mean(LFA30.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA30.glm1,newdata=newdata,se.fit=T)
		pred.total.short<-exp(pred$fit)
		pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA30.year$pred.s.cpue<-pred.short.cpue


	# Generalized linear model for legals
	LFA30.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA30.week, family=poisson(link="log"))
	summary(LFA30.glm2)

		# determine most representative vessel for predictions
		vessels<-unique(LFA30.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA30.glm2$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA30.week$WOS),SYEAR=sort(unique(LFA30.week$SYEAR)),logTRAPS=mean(LFA30.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA30.glm2,newdata=newdata,se.fit=T)
		pred.total.legal<-exp(pred$fit)
		pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA30.year$pred.l.cpue<-pred.legal.cpue


	par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
	plot(s.cpue~SYEAR, LFA30.year,ylim=c(0,3.2),xaxt='n')
	axis(1,lab=F)
	lines(pred.s.cpue~SYEAR, LFA30.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
	plot(l.cpue~SYEAR, LFA30.year,ylim=c(0,3.2))
	lines(pred.l.cpue~SYEAR, LFA30.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
	mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
	mtext("LFA 30",3,1,outer=T,las=0)


#---------------------------------------------------------------------------LFA31A

	FSRS.LFA31A.dat<-subset(FSRSvesday.dat,LFA==31.1)

	# Create column for week of season (WOS)
	season<-unique(FSRS.LFA31A.dat$S_LABEL)
	for(i in 1:length(season)){
		FSRS.LFA31A.dat$WOS[FSRS.LFA31A.dat$S_LABEL==season[i]]<-floor((FSRS.LFA31A.dat$HAUL_DATE[FSRS.LFA31A.dat$S_LABEL==season[i]]-min(FSRS.LFA31A.dat$HAUL_DATE[FSRS.LFA31A.dat$S_LABEL==season[i]]))/7)+1
	}


	LFA31A.week<-ddply(FSRS.LFA31A.dat,.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA31A.week$s.cpue=LFA31A.week$SHORTS/LFA31A.week$TRAPS
	LFA31A.week$l.cpue=LFA31A.week$LEGALS/LFA31A.week$TRAPS
	LFA31A.week$logTRAPS=log(LFA31A.week$TRAPS)

	LFA31A.year<-ddply(FSRS.LFA31A.dat,.(LFA,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))
	LFA31A.year$LFA<-'31A'
	LFA31A.year$s.cpue=LFA31A.year$SHORTS/LFA31A.year$TRAPS
	LFA31A.year$l.cpue=LFA31A.year$LEGALS/LFA31A.year$TRAPS

	# Generalized linear model for sub-legals
	LFA31A.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA31A.week, family=poisson(link="log"))
	summary(LFA31A.glm1)

		# determine most representative vessel for predictions
		vessels<-unique(LFA31A.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA31A.glm1$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA31A.week$WOS),SYEAR=sort(unique(LFA31A.week$SYEAR)),logTRAPS=mean(LFA31A.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA31A.glm1,newdata=newdata,se.fit=T)
		pred.total.short<-exp(pred$fit)
		pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA31A.year$pred.s.cpue<-pred.short.cpue


	# Generalized linear model for legals
	LFA31A.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA31A.week, family=poisson(link="log"))
	summary(LFA31A.glm2)

		# determine most representative vessel for predictions
		vessels<-unique(LFA31A.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA31A.glm2$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA31A.week$WOS),SYEAR=sort(unique(LFA31A.week$SYEAR)),logTRAPS=mean(LFA31A.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA31A.glm2,newdata=newdata,se.fit=T)
		pred.total.legal<-exp(pred$fit)
		pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA31A.year$pred.l.cpue<-pred.legal.cpue


	par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
	plot(s.cpue~SYEAR, LFA31A.year,ylim=c(0,3.2),xaxt='n')
	axis(1,lab=F)
	lines(pred.s.cpue~SYEAR, LFA31A.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
	plot(l.cpue~SYEAR, LFA31A.year,ylim=c(0,3.2))
	lines(pred.l.cpue~SYEAR, LFA31A.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
	mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
	mtext("LFA 31A",3,1,outer=T,las=0)


#---------------------------------------------------------------------------LFA31B

	FSRS.LFA31B.dat<-subset(FSRSvesday.dat,LFA==31.2)

	# Create column for week of season (WOS)
	season<-unique(FSRS.LFA31B.dat$S_LABEL)
	for(i in 1:length(season)){
		FSRS.LFA31B.dat$WOS[FSRS.LFA31B.dat$S_LABEL==season[i]]<-floor((FSRS.LFA31B.dat$HAUL_DATE[FSRS.LFA31B.dat$S_LABEL==season[i]]-min(FSRS.LFA31B.dat$HAUL_DATE[FSRS.LFA31B.dat$S_LABEL==season[i]]))/7)+1
	}


	LFA31B.week<-ddply(FSRS.LFA31B.dat,.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA31B.week$s.cpue=LFA31B.week$SHORTS/LFA31B.week$TRAPS
	LFA31B.week$l.cpue=LFA31B.week$LEGALS/LFA31B.week$TRAPS
	LFA31B.week$logTRAPS=log(LFA31B.week$TRAPS)

	LFA31B.year<-ddply(FSRS.LFA31B.dat,.(LFA,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))
	LFA31B.year$LFA<-'31B'

	LFA31B.year$s.cpue=LFA31B.year$SHORTS/LFA31B.year$TRAPS
	LFA31B.year$l.cpue=LFA31B.year$LEGALS/LFA31B.year$TRAPS

	# Generalized linear model for sub-legals
	LFA31B.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA31B.week, family=poisson(link="log"))
	summary(LFA31B.glm1)

		# determine most representative vessel for predictions
		vessels<-unique(LFA31B.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA31B.glm1$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA31B.week$WOS),SYEAR=sort(unique(LFA31B.week$SYEAR)),logTRAPS=mean(LFA31B.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA31B.glm1,newdata=newdata,se.fit=T)
		pred.total.short<-exp(pred$fit)
		pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA31B.year$pred.s.cpue<-pred.short.cpue


	# Generalized linear model for legals
	LFA31B.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA31B.week, family=poisson(link="log"))
	summary(LFA31B.glm2)

		# determine most representative vessel for predictions
		vessels<-unique(LFA31B.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA31B.glm2$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA31B.week$WOS),SYEAR=sort(unique(LFA31B.week$SYEAR)),logTRAPS=mean(LFA31B.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA31B.glm2,newdata=newdata,se.fit=T)
		pred.total.legal<-exp(pred$fit)
		pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA31B.year$pred.l.cpue<-pred.legal.cpue


	par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
	plot(s.cpue~SYEAR, LFA31B.year,ylim=c(0,3.2),xaxt='n')
	axis(1,lab=F)
	lines(pred.s.cpue~SYEAR, LFA31B.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
	plot(l.cpue~SYEAR, LFA31B.year,ylim=c(0,3.2))
	lines(pred.l.cpue~SYEAR, LFA31B.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
	mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
	mtext("LFA 31B",3,1,outer=T,las=0)


#---------------------------------------------------------------------------LFA32

	FSRS.LFA32.dat<-subset(FSRSvesday.dat,LFA==32)

	# Create column for week of season (WOS)
	season<-unique(FSRS.LFA32.dat$S_LABEL)
	for(i in 1:length(season)){
		FSRS.LFA32.dat$WOS[FSRS.LFA32.dat$S_LABEL==season[i]]<-floor((FSRS.LFA32.dat$HAUL_DATE[FSRS.LFA32.dat$S_LABEL==season[i]]-min(FSRS.LFA32.dat$HAUL_DATE[FSRS.LFA32.dat$S_LABEL==season[i]]))/7)+1
	}


	LFA32.week<-ddply(FSRS.LFA32.dat,.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA32.week$s.cpue=LFA32.week$SHORTS/LFA32.week$TRAPS
	LFA32.week$l.cpue=LFA32.week$LEGALS/LFA32.week$TRAPS
	LFA32.week$logTRAPS=log(LFA32.week$TRAPS)

	LFA32.year<-ddply(FSRS.LFA32.dat,.(LFA,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

	LFA32.year$s.cpue=LFA32.year$SHORTS/LFA32.year$TRAPS
	LFA32.year$l.cpue=LFA32.year$LEGALS/LFA32.year$TRAPS

	# Generalized linear model for sub-legals
	LFA32.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA32.week, family=poisson(link="log"))
	summary(LFA32.glm1)

		# determine most representative vessel for predictions
		vessels<-unique(LFA32.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA32.glm1$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA32.week$WOS),SYEAR=sort(unique(LFA32.week$SYEAR)),logTRAPS=mean(LFA32.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA32.glm1,newdata=newdata,se.fit=T)
		pred.total.short<-exp(pred$fit)
		pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA32.year$pred.s.cpue<-pred.short.cpue


	# Generalized linear model for legals
	LFA32.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA32.week, family=poisson(link="log"))
	summary(LFA32.glm2)

		# determine most representative vessel for predictions
		vessels<-unique(LFA32.week$VESSEL_CD)
		vesselCoefs<-c(0,LFA32.glm2$coef[2:length(vessels)])
		v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
		
		# predicted number
		newdata<-data.frame(WOS=mean(LFA32.week$WOS),SYEAR=sort(unique(LFA32.week$SYEAR)),logTRAPS=mean(LFA32.week$logTRAPS),VESSEL_CD=vessels[v])
		pred<-predict(LFA32.glm2,newdata=newdata,se.fit=T)
		pred.total.legal<-exp(pred$fit)
		pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
		LFA32.year$pred.l.cpue<-pred.legal.cpue


	par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
	plot(s.cpue~SYEAR, LFA32.year,ylim=c(0,3.2),xaxt='n')
	axis(1,lab=F)
	lines(pred.s.cpue~SYEAR, LFA32.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
	plot(l.cpue~SYEAR, LFA32.year,ylim=c(0,3.2))
	lines(pred.l.cpue~SYEAR, LFA32.year)
	legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
	mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
	mtext("LFA 32",3,1,outer=T,las=0)


#---------------------------------------------------------------------------LFA33

	FSRS.LFA33.dat<-subset(FSRSvesday.dat,LFA==33)

	# Create column for week of season (WOS)
	season<-unique(FSRS.LFA33.dat$S_LABEL)
	for(i in 1:length(season)){
		FSRS.LFA33.dat$WOS[FSRS.LFA33.dat$S_LABEL==season[i]]<-floor((FSRS.LFA33.dat$HAUL_DATE[FSRS.LFA33.dat$S_LABEL==season[i]]-min(FSRS.LFA33.dat$HAUL_DATE[FSRS.LFA33.dat$S_LABEL==season[i]]))/7)+1
	}


	#----------------------------------------------------------------------East
	LFA33east.week<-ddply(subset(FSRS.LFA33.dat,subarea=='33 East'),.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

		LFA33east.week$s.cpue=LFA33east.week$SHORTS/LFA33east.week$TRAPS
		LFA33east.week$l.cpue=LFA33east.week$LEGALS/LFA33east.week$TRAPS
		LFA33east.week$logTRAPS=log(LFA33east.week$TRAPS)

		LFA33east.year<-ddply(subset(FSRS.LFA33.dat,subarea=='33 East'),.(LFA,subarea,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

		LFA33east.year$s.cpue=LFA33east.year$SHORTS/LFA33east.year$TRAPS
		LFA33east.year$l.cpue=LFA33east.year$LEGALS/LFA33east.year$TRAPS

		# Generalized linear model for sub-legals
		LFA33east.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA33east.week, family=poisson(link="log"))
		summary(LFA33east.glm1)

			# determine most representative vessel for predictions
			vessels<-unique(LFA33east.week$VESSEL_CD)
			vesselCoefs<-c(0,LFA33east.glm1$coef[2:length(vessels)])
			v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
			
			# predicted number
			newdata<-data.frame(WOS=mean(LFA33east.week$WOS),SYEAR=sort(unique(LFA33east.week$SYEAR)),logTRAPS=mean(LFA33east.week$logTRAPS),VESSEL_CD=vessels[v])
			pred<-predict(LFA33east.glm1,newdata=newdata,se.fit=T)
			pred.total.short<-exp(pred$fit)
			pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
			LFA33east.year$pred.s.cpue<-pred.short.cpue


		# Generalized linear model for legals
		LFA33east.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA33east.week, family=poisson(link="log"))
		summary(LFA33east.glm2)

			# determine most representative vessel for predictions
			vessels<-unique(LFA33east.week$VESSEL_CD)
			vesselCoefs<-c(0,LFA33east.glm2$coef[2:length(vessels)])
			v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
			
			# predicted number
			newdata<-data.frame(WOS=mean(LFA33east.week$WOS),SYEAR=sort(unique(LFA33east.week$SYEAR)),logTRAPS=mean(LFA33east.week$logTRAPS),VESSEL_CD=vessels[v])
			pred<-predict(LFA33east.glm2,newdata=newdata,se.fit=T)
			pred.total.legal<-exp(pred$fit)
			pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
			LFA33east.year$pred.l.cpue<-pred.legal.cpue


		par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
		plot(s.cpue~SYEAR, LFA33east.year,ylim=c(0,4.5),xaxt='n')
		axis(1,lab=F)
		lines(pred.s.cpue~SYEAR, LFA33east.year)
		legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
		plot(l.cpue~SYEAR, LFA33east.year,ylim=c(0,4.5))
		lines(pred.l.cpue~SYEAR, LFA33east.year)
		legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
		mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
		mtext("LFA 33 east",3,1,outer=T,las=0)

	#----------------------------------------------------------------------West
	LFA33west.week<-ddply(subset(FSRS.LFA33.dat,subarea=='33 West'),.(SYEAR, VESSEL_CD,WOS),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))

		LFA33west.week$s.cpue=LFA33west.week$SHORTS/LFA33west.week$TRAPS
		LFA33west.week$l.cpue=LFA33west.week$LEGALS/LFA33west.week$TRAPS
		LFA33west.week$logTRAPS=log(LFA33west.week$TRAPS)

		LFA33west.year<-ddply(subset(FSRS.LFA33.dat,subarea=='33 West'),.(LFA,subarea,SYEAR),summarize,SHORTS=sum(SHORTS),LEGALS=sum(LEGALS),TRAPS=sum(TOTAL_TRAPS))
		
		LFA33west.year$s.cpue=LFA33west.year$SHORTS/LFA33west.year$TRAPS
		LFA33west.year$l.cpue=LFA33west.year$LEGALS/LFA33west.year$TRAPS

		# Generalized linear model for sub-legals
		LFA33west.glm1=glm(SHORTS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA33west.week, family=poisson(link="log"))
		summary(LFA33west.glm1)

			# determine most representative vessel for predictions
			vessels<-unique(LFA33west.week$VESSEL_CD)
			vesselCoefs<-c(0,LFA33west.glm1$coef[2:length(vessels)])
			v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
			
			# predicted number
			newdata<-data.frame(WOS=mean(LFA33west.week$WOS),SYEAR=sort(unique(LFA33west.week$SYEAR)),logTRAPS=mean(LFA33west.week$logTRAPS),VESSEL_CD=vessels[v])
			pred<-predict(LFA33west.glm1,newdata=newdata,se.fit=T)
			pred.total.short<-exp(pred$fit)
			pred.short.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
			LFA33west.year$pred.s.cpue<-pred.short.cpue


		# Generalized linear model for legals
		LFA33west.glm2=glm(LEGALS~as.factor(VESSEL_CD) + as.factor(SYEAR) + WOS + offset(logTRAPS) ,data=LFA33west.week, family=poisson(link="log"))
		summary(LFA33west.glm2)

			# determine most representative vessel for predictions
			vessels<-unique(LFA33west.week$VESSEL_CD)
			vesselCoefs<-c(0,LFA33west.glm2$coef[2:length(vessels)])
			v<-which(abs(vesselCoefs-median(vesselCoefs))==min(abs(vesselCoefs-median(vesselCoefs))))
			
			# predicted number
			newdata<-data.frame(WOS=mean(LFA33west.week$WOS),SYEAR=sort(unique(LFA33west.week$SYEAR)),logTRAPS=mean(LFA33west.week$logTRAPS),VESSEL_CD=vessels[v])
			pred<-predict(LFA33west.glm2,newdata=newdata,se.fit=T)
			pred.total.legal<-exp(pred$fit)
			pred.legal.cpue<-exp(pred$fit)/exp(newdata$logTRAPS[1])
			LFA33west.year$pred.l.cpue<-pred.legal.cpue


		par(mfrow=c(2,1),mar=c(0,1,0,1),omi=c(0.5,0.8,0.5,0.2),las=1)
		plot(s.cpue~SYEAR, LFA33west.year,ylim=c(0,4.5),xaxt='n')
		axis(1,lab=F)
		lines(pred.s.cpue~SYEAR, LFA33west.year)
		legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Sub-Legal",bty='n',inset=c(0,0.05),cex=0.8)
		plot(l.cpue~SYEAR, LFA33west.year,ylim=c(0,4.5))
		lines(pred.l.cpue~SYEAR, LFA33east.year)
		legend('topleft',c('unstandardized','predicted'),pch=c(1,NA),lty=c(NA,1),title="Legal",bty='n',inset=c(0,0.05),cex=0.8)
		mtext("CPUE (No. Lobsters/Trap)",2,2,outer=T,las=0)
		mtext("LFA 33 west",3,1,outer=T,las=0)

dev.off()

FSRScpue.dat<-merge(rbind(LFA27north.year,LFA27south.year,LFA33east.year,LFA33west.year),rbind(LFA28.year,LFA29.year,LFA30.year,LFA31A.year,LFA31B.year,LFA32.year),all=T)
write.csv(FSRScpue.dat,file.path( project.datadirectory("lobster"), "data","FSRScpue.csv"),row.names=F)


#---------------------------------------------------------------------------Plots for Update
	
	FSRScpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","FSRScpue.csv"))
	require(lattice)

	wd=9
	ht=6
	wd.r=0.7
	ht.r=0.62

	windows(wd,ht)
	xyplot(pred.s.cpue~SYEAR|LFA, data=subset(FSRScpue.dat,LFA%in%c('28','29','30','31A','31B','32')), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", main="", as.table=T,type='b',ylim=c(0,4.5))
	windows(wd*wd.r,ht*ht.r)
	xyplot(pred.s.cpue~SYEAR|subarea, data=subset(FSRScpue.dat,LFA=='27'), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,4.5))
	windows(wd*wd.r,ht*ht.r)
	xyplot(pred.s.cpue~SYEAR|subarea, data=subset(FSRScpue.dat,LFA=='33'), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,4.5))



