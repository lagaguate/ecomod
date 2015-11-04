
dataOverlap<-function(data1,data2,distance.Theshold=0.1,dnames=c("EID","X","Y","date","salinity","temperature","depth","source")){

	data1$EID<-1:nrow(data1)
	data2$EID<-(1:nrow(data2))+nrow(data1)

	data1$source<-1
	data2$source<-2

	days<-sort(unique(data1$date))

	Add.lst<-list()
	Reject.lst<-list()

	for(i in 1:length(days)){

		x<-subset(data1,date==days[i],dnames)
		y<-subset(data2,date==days[i],dnames)
		z<-rbind(x,y)
		z$Overlap<-NA
		attr(z,'projection')<-"LL"
		z<-convUL(z)
		W<-owin(range(z$X),range(z$Y))
		ppp<-as.ppp(subset(z,select=c('X','Y')),W)
		z$nndist<-nndist(ppp)
		z<-convUL(z)
		z$Overlap[z$nndist>=distance.Theshold)]<-F
		z$Overlap[z$nndist<distance.Theshold)]<-T
		Add.lst[[i]]<-subset(z,source==1)
		}

	Data1<-do.call("rbind",Add.lst)
	return(Data)
}
