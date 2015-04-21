


loadfunctions("lobster")


#SUMMARY OF LANDINGS AND EFFORT TABLE:
Fish.Date<-read.csv(file.path( project.datadirectory("lobster"), "data","FishingSeasonDates.csv"))
lfa <- unique(Fish.Date$LFA)
max_trap<-c(825,750,750,750,750,750,750,750,1126,1126,1126,1226)
max_lbs<-c(2750,2750,2750,2750,2750,2750,2750,10000,30000,30000,30000,30000)
Fish.Date$START_DATE<-as.Date(Fish.Date$START_DATE,"%d/%m/%Y")
Fish.Date$END_DATE<-as.Date(Fish.Date$END_DATE,"%d/%m/%Y")


# import logs from marfis
lobster.db('logs')

logs$TOTAL_NUM_TRAPS<-rowSums(logs[c('NUM_OF_TRAPS','NUM_OF_TRAPS_B','NUM_OF_TRAPS_C')],na.rm=T)
logs$TOTAL_WEIGHT_LBS<-rowSums(logs[c('WEIGHT_LBS','WEIGHT_LBS_B','WEIGHT_LBS_C')],na.rm=T)
logs$TOTAL_WEIGHT_KG<-logs$TOTAL_WEIGHT_LBS*0.4536

# select for records within season
logs$SYEAR<-NA
logs$DATE_FISHED<-as.Date(logs$DATE_FISHED)
for(i in 1:length(lfa)) {
	h <- Fish.Date[Fish.Date$LFA==lfa[i],]	
	for(j in 1:nrow(h)) {
		logs$SYEAR[logs$LFA==lfa[i]&logs$DATE_FISHED>=h[j,'START_DATE']&logs$DATE_FISHED<=h[j,'END_DATE']]<-h[j,'SYEAR']
	}

}
logs<-subset(logs,!is.na(SYEAR))

# add week of season (WOS) variable
logs$WOS<-NA
for(i in 1:length(lfa)) {
	h <- Fish.Date[Fish.Date$LFA==lfa[i],]	
	for(j in unique(logs$SYEAR[logs$LFA==lfa[i]])){
		logs$WOS[logs$LFA==lfa[i]&logs$SYEAR==j]<-floor(as.numeric(logs$DATE_FISHED[logs$LFA==lfa[i]&logs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
	}
}


commonCols<-c("SUM_DOC_ID", "VR_NUMBER", "VESSEL_NAME", "SUBMITTER_NAME", "LICENCE_ID", "LFA", "SD_LOG_ID", "DATE_FISHED","SYEAR","WOS","TOTAL_NUM_TRAPS","TOTAL_WEIGHT_KG")

logsInSeasonA<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS)&!is.na(NUM_OF_TRAPS),c(commonCols,"GRID_NUM", "WEIGHT_LBS", "NUM_OF_TRAPS"))
logsInSeasonB<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS_B)&!is.na(NUM_OF_TRAPS_B),c(commonCols,"GRID_NUM_B", "WEIGHT_LBS_B", "NUM_OF_TRAPS_B"))
logsInSeasonC<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS_C)&!is.na(NUM_OF_TRAPS_C),c(commonCols,"GRID_NUM_C", "WEIGHT_LBS_C", "NUM_OF_TRAPS_C"))
names(logsInSeasonB)<-names(logsInSeasonA)
names(logsInSeasonC)<-names(logsInSeasonA)
logsInSeason<-rbind(logsInSeasonA,logsInSeasonB,logsInSeasonC)
logsInSeason$WEIGHT_KG<-logsInSeason$WEIGHT_LBS*0.4536

centgrid<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","lfa27_38_centgrid.csv"))
grid.key<-with(centgrid,paste(LFA,GRID_NUM,sep='.'))
logsInSeason<-subset(logsInSeason,!is.na(GRID_NUM)&paste(LFA,GRID_NUM,sep='.')%in%grid.key)
logsInSeason$CPUE<-logsInSeason$WEIGHT_KG/logsInSeason$NUM_OF_TRAPS
logsInSeason<-subset(logsInSeason,CPUE<20)

subareas<-read.csv(file.path( project.datadirectory("lobster"), "data","LFA2733subarea.csv"))
names(subareas)[2]<-"GRID_NUM"
logsInSeason<-merge(logsInSeason,subareas,all.x=T)


### by subarea for 27 & 33, nogrid excluded
cpue.lst<-list()
cpue1.lst<-list()
for(i in 1:length(lfa)) {
	dat<-subset(logsInSeason,LFA==lfa[i]&TOTAL_NUM_TRAPS<max_trap[i],c('subarea','SYEAR','WEIGHT_KG','NUM_OF_TRAPS'))
	if(lfa[i]%in%c('27','33')){
		subs<-unique(dat$subarea)
		for(j in 1:length(subs)){
			sdat<-na.omit(subset(dat,subarea==subs[j],c('SYEAR','WEIGHT_KG','NUM_OF_TRAPS')))
			catch<-with(sdat,tapply(WEIGHT_KG,SYEAR,sum))
			effort<-with(sdat,tapply(NUM_OF_TRAPS,SYEAR,sum))
			n<-with(sdat,tapply(NUM_OF_TRAPS,SYEAR,length))

			cpue1.lst[[j]]<-data.frame(lfa=lfa[i],subarea=subs[j],year=sort(unique(sdat$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)

		}
		cpue.lst[[i]]<-do.call("rbind",cpue1.lst)
	}
	if(!lfa[i]%in%c('27','33')){
		sdat<-na.omit(subset(dat,select=c('SYEAR','WEIGHT_KG','NUM_OF_TRAPS')))
		catch<-with(sdat,tapply(WEIGHT_KG,SYEAR,sum))
		effort<-with(sdat,tapply(NUM_OF_TRAPS,SYEAR,sum))
		n<-with(sdat,tapply(NUM_OF_TRAPS,SYEAR,length))

		cpue.lst[[i]]<-data.frame(lfa=lfa[i],subarea=unique(dat$subarea),year=sort(unique(sdat$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)
	}
}

names(cpue.lst)<-lfa
cpue.dat<-do.call("rbind",cpue.lst)

write.csv(cpue.dat,file.path( project.datadirectory("lobster"), "data","CommercialCPUE.csv"),row.names=F)


#### Plotting
cpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","CommercialCPUE.csv"))

require(lattice)

	wd=9
	ht=6
	wd.r=0.7
	ht.r=0.62


	windows(wd,ht)
	xyplot(cpue~year|lfa, data=subset(cpue.dat,lfa%in%c('28','29','30','31A','31B','32')), ylab="CPUE (Kg / Trap Haul)",xlab= "Year", main="", as.table=T,type='b',ylim=c(0,2.2))
	windows(wd*wd.r,ht*ht.r)
	xyplot(cpue~year|subarea, data=subset(cpue.dat,lfa=='27'), ylab="CPUE (Kg / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,2.2))
	windows(wd*wd.r,ht*ht.r)
	xyplot(cpue~year|subarea, data=subset(cpue.dat,lfa=='33'), ylab="CPUE (Kg / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,2.2))


for(i in 1:length(lfa)) {
	for(j in 2008:2014){
		pdf(file.path( project.datadirectory("lobster"),"R",paste0("CPUEhist",lfa[i],j,".pdf")),8,11)
		par(mfrow=c(3,2))
		d<-subset(logsInSeason,LFA==lfa[i]&SYEAR==j)
		brks=seq(0,max(d$CPUE)+1,0.1)
		for(k in sort(unique(d$WOS))){
			hist(d$CPUE[d$WOS==k],breaks=brks,main=k,xlab="CPUE")
		}
		dev.off()
	}
}


aggDat99<-aggregate(CPUE ~ LFA + SYEAR + WOS, data = logsInSeason, quantile,0.99)
aggDat999<-aggregate(CPUE ~ LFA + SYEAR + WOS, data = logsInSeason, quantile,0.999)
par(mfcol=c(3,2))
plot(CPUE~WOS,subset(aggDat99,LFA=='34'),main='34 99th',ylim=c(0,17))
plot(CPUE~WOS,subset(aggDat99,LFA=='33'),main='33 99th',ylim=c(0,17))
plot(CPUE~WOS,subset(aggDat99,LFA=='27'),main='27 99th',ylim=c(0,17))

plot(CPUE~WOS,subset(aggDat999,LFA=='34'),main='34 99.9th')#,ylim=c(0,max()))
plot(CPUE~WOS,subset(aggDat999,LFA=='33'),main='33 99.9th')#ylim=c(0,17))
plot(CPUE~WOS,subset(aggDat999,LFA=='27'),main='27 99.9th')#,ylim=c(0,17))




LandingsUpdate<-read.table(file.path( project.datadirectory("lobster"), "data","Landings.27-33.1947.2014.txt"),header=T)

windows(6.5,8)
bwd<-8
par(mfrow=c(3,1),las=1,mar=c(2,2,2,2),omi=c(0.2,0.8,0.2,0.2))
plot(Landings.tons~YR,subset(LandingsUpdate,LFA==27),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 27",ylim=c(0,6000))
axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)
plot(Landings.tons~YR,subset(LandingsUpdate,LFA==28.32),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 28-32",ylim=c(0,6000))
axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)
plot(Landings.tons~YR,subset(LandingsUpdate,LFA==33),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 33",ylim=c(0,6000))
axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)

mtext("Landings (mt)",2,3,outer=T,las=0)




############### voluntary logs ####################


lobster.db('vlog')

