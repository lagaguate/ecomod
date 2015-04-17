CLF<-function(LFdat,bins=seq(53,228,5),yrs=1982:2014){

	require("lubridate")
	names(LFdat)[1:2]<-c('YEAR','LENGTH')

	LFdat<-na.omit(LFdat)
	nc<-ncol(LFdat)
	LFdat$ID<-"CLF"
	if(nc>2){
		LFdat$ID<-paste0(names(LFdat)[3],LFdat[,3])
		if(nc>3){
			for(i in 4:nc) LFdat$ID<-paste(LFdat$ID,paste0(names(LFdat)[i],LFdat[,i]),sep='.')	
		}
	}		

	CLF<-list()
	IDs<-unique(LFdat$ID)
	for(i in 1:length(IDs)){
		CLF[[i]]<-t(sapply(yrs,function(y){with(subset(LFdat,YEAR==y&ID==IDs[i]&LENGTH>=min(bins)&LENGTH<max(bins)),hist(LENGTH,breaks=bins,plot=F)$count)}))
	}
	names(CLF)<-IDs

	return(CLF)
}


