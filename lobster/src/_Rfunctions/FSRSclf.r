FSRSclf<-function(lfa= c("27", "28", "29", "30", "31.1", "31.2", "32", "33"), yrs=2004:2015, bins=seq(0,140,10),fn="FSRS",sex=1:2,maxsoak=10){

	loadfunctions("lobster")


	#LATEST DATA EXPORT FROM FSRS DATABASE:
	#lobster.db("fsrs.redo")
	lobster.db("fsrs")
	#recruitment.trap.db('raw.redo',p=p)

	fsrs$SYEAR<-fsrs$HAUL_YEAR
	fsrs$SYEAR[fsrs$LFA%in%c("33","34")]<-as.numeric(substr(fsrs$S_LABEL[fsrs$LFA%in%c("33","34")],6,9))

	fsrs<-subset(fsrs,SOAK_DAYS<maxsoak&SEX%in%sex)	# Remove soak days greater than 5,  do not iclude berried females
	fsrs$HAUL_DATE<-as.Date(fsrs$HAUL_DATE)

	CLF<-list()
	for(i in 1:length(lfa)){
		print(lfa[i])
		CLF[[i]]<-t(sapply(yrs,function(y){with(subset(fsrs,LFA==lfa[i]&SYEAR==y),hist(SIZE_CD,breaks=0.5:15.5,plot=F)$counts)}))

	}
	names(CLF)<-paste("LFA",lfa)		
	BarPlotCLF(CLF,yrs=yrs,bins=bins,col='grey',filen=fn,rel=T,LS=83,wd=9)
}