	# source("Y:/Assessment/2009/r/fn/design functions/design.anal.r")
	
	
# Analysis Efficiency Against Simple Random with "BIOsurvey" package 

design.anal<-function(restrat.dat,restrat.areas,original.dat=NULL,original.areas=NULL,years=2007:1981,plot=T,domain=T,biomass=F){
	
	print("design.anal start")
	print(Sys.time())
	
	
	require(PEDstrata)
	names(restrat.dat)<-c('year','tow','STRATA.ID','resp')
	restrat.dat$orig.strata<-original.dat$new.stratum
	
	strat.anal<-list(NULL)
	
	mean.str<-c()
	mean.ran<-c()
	se.str<-c()
	se.ran<-c()
	var.str<-c()
	var.ran<-c()
	effic.str<-c()
	effic.alloc<-c()
	effic.tot<-c()
	effic.pot<-c()
	ci.low<-c()
	ci.up<-c()
	min.var<-c()
	dom.var.str<-c()
	dom.se.str<-c()
	dom.mean.str<-c()
	dom.min.var<-c()
	if(is.vector(restrat.areas))restrat.areas<-matrix(restrat.areas,length(years),length(restrat.areas),byrow=T)
	
	for(i in 1:length(years)){
		
		annual.dat<-subset(restrat.dat,year==years[i])
		annual.dat$STRATA.ID[is.na(annual.dat$STRATA.ID)]<-1
		annual.dat<-na.omit(annual.dat)
		Scallop.strata<-data.frame(Strata=sort(unique(annual.dat$STRATA.ID)),NH=restrat.areas[i,])
		print(years[i])
	#browser()	
		Scallop.numbers<-PEDstrata(annual.dat, Scallop.strata,'STRATA.ID', annual.dat$resp)
		strat.anal[[i]]<-summary(Scallop.numbers,effic=T)
		se.str[i]<-strat.anal[[i]]$se.yst
		var.str[i]<-strat.anal[[i]]$se.yst^2
		var.ran[i]<-strat.anal[[i]]$var.ran
		se.ran[i]<-sqrt(var.ran[i])
		#min.var[i]<-strat.anal[[i]]$min.var
		mean.str[i]<-strat.anal[[i]]$yst
		mean.ran[i]<-mean(annual.dat$resp)
		effic.str[i]<-strat.anal[[i]]$effic.str
		effic.alloc[i]<-strat.anal[[i]]$effic.alloc
		effic.tot[i]<-effic.str[i]+effic.alloc[i]
		effic.pot[i]<-strat.anal[[i]]$max.eff
		#ci.low[i]<-strat.anal[[i]]$ci.yst[1]
		#ci.up[i]<-strat.anal[[i]]$ci.yst[2]
		
		if(domain==T){
			old.area<-data.frame(Strata=1:5,NH=as.vector(unlist(original.areas[i,])),strata.id=1:5)
			new.area<-data.frame(Scallop.strata,strata.id=Scallop.strata$Strata)
			test<-Domain.estimates(annual.dat$resp,annual.dat$orig.strata,annual.dat$STRATA.ID,old.area,new.area)
			dom.min.var[i]<-summary(test)[[2]]$min.var
			dom.var.str[i]<-summary(test)[[2]]$var.yst
			dom.se.str[i]<-sqrt(summary(test)[[2]]$var.yst)
			dom.mean.str[i]<-summary(test)[[2]]$yst

			annual.dat$STRATA.ID<-annual.dat$orig.strata
			Scallop.stratadata<-Prepare.strata.data(annual.dat)
			Scallop.numbers<-Stratify(Scallop.stratadata, old.area, species=resp)
			strat.anal[[i]]<-summary(Scallop.numbers,effic=T)
			se.str[i]<-strat.anal[[i]]$se.yst
			var.str[i]<-strat.anal[[i]]$se.yst^2
			var.ran[i]<-strat.anal[[i]]$var.ran
			se.ran[i]<-sqrt(var.ran[i])
			min.var[i]<-strat.anal[[i]]$min.var
			mean.str[i]<-strat.anal[[i]]$yst
			mean.ran[i]<-mean(annual.dat$resp)
			effic.tot[i]<-(var.str[i]-dom.var.str[i])/var.str[i]*100
			effic.pot[i]<-(var.str[i]-dom.min.var[i])/var.str[i]*100
			ci.low[i]<-strat.anal[[i]]$ci.yst[1]
			ci.up[i]<-strat.anal[[i]]$ci.yst[2]
			
		}
	
	}

	if(domain==F)stratadata<-data.frame(years,mean.str,mean.ran,var.str,var.ran,effic.str,effic.alloc,effic.tot,effic.pot,se.str,se.ran,cv.str=se.str/mean.str,cv.ran=se.ran/mean.ran)
	if(domain==T)stratadata<-data.frame(years,mean.str,mean.ran,var.str,var.ran,effic.tot,effic.pot,ci.low,ci.up,se.str,se.ran,cv.str=se.str/mean.str,cv.ran=se.ran/mean.ran,min.var=min.var,dom.min.var,dom.mean.str,dom.var.str,dom.se.str,dom.cv.str=dom.se.str/dom.mean.str)

	measure<-ifelse(biomass,'kg/tow','mean number/tow')
	
	if(plot==T){
		windows(7,9)
		par(mfrow=c(3,1), mar=c(0,5,0,1), omi=c(0.7,0,0.3,0.2))
		# simple mean vs stratified mean
		plot(mean.str~years,data=stratadata,type='b',pch=16,xlab='',ylab=measure,xaxt='n',ylim=c(0,max(c(ci.up),mean.ran)),xlim=range(years))
		#lines(ci.low~years,data=stratadata,type='l',col='grey60',lty=2)
		#lines(ci.up~years,data=stratadata,type='l',col='grey60',lty=2)
		lines(mean.ran~years,data=stratadata,type='b',col='red')
		if(domain==T)lines(dom.mean.str~years,data=stratadata,type='b',col='blue',lwd=1,pch=3)
		axis(1,lab=F)
		axis(4,lab=F)
		
#		legend("topleft", c("stratafied","simple random","stratafied 95% CI"), pch=c(16,1,1),col=c('black','red','grey60'),lty=c(1,1,2),inset=.05)
		
		# simple variance vs stratified variance
		plot(se.str~years,data=stratadata,type='b',pch=16,xlab='',ylab='standard error',xaxt='n',xlim=range(years),ylim=c(0,max(c(se.str,se.ran))*1.2))
		lines(se.ran~years,data=stratadata,type='b',col='red')
		if(domain==T)lines(dom.se.str~years,data=stratadata,type='b',col='blue',lwd=1,pch=3)
#		legend("topleft", c("stratafied","simple random"), pch=c(16,1),col=c('black','red'),lty=c(1,1),inset=.05)
		axis(1,lab=F)
		axis(4,lab=F)

		plot(cv.str~years,data=stratadata,type='b',pch=16,xlab='',ylab='relative error',xlim=range(years),ylim=c(0,max(c(cv.str,cv.ran))*1.2))
		lines(cv.ran~years,data=stratadata,type='b',col='red')
		if(domain==T)lines(dom.cv.str~years,data=stratadata,type='b',col='blue',lwd=1,pch=3)
		axis(4,lab=F)

	
		# total and stratified relative efficiency
		windows(8,7)
		par(mfrow=c(2,1), mar=c(0,5,0,1), omi=c(1,0.1,0.5,0.5))
		if(domain==T){
			plot(effic.pot~years,data=stratadata,type='h',ylim=c(-110,100),col='grey80',lwd=15,lend=3,xlab='',xlim=range(years),ylab='',xaxt='n')
			lines(years,effic.tot,type='h',lwd=15,lend=3)
			abline(h=0)
			axis(1,lab=F)
			axis(4,lab=F)
			
			
			tot<-(var.ran-dom.var.str)/var.ran*100
			pot<-(var.ran-dom.min.var)/var.ran*100

			plot(years,pot,type='h',ylim=c(-100,110),col='grey80',lwd=15,lend=3,xlab='Year',xlim=range(years),ylab='')
			lines(years,tot,type='h',lwd=15,lend=3)
			abline(h=0)
			axis(4,lab=F)
		}
		
		if(domain==F){
			plot(effic.pot~years,data=stratadata,type='h',ylim=c(-110,100),col='grey75',lwd=15,lend=3,xlab='',xlim=range(years),ylab='',xaxt='n')
			lines(years,effic.tot,type='h',lwd=15,lend=3)
			abline(h=0)
			axis(1,lab=F)
			axis(4,lab=F)
			
			plot(years-0.22,effic.str,type='h',ylim=c(-100,110),lwd=9,lend=3,xlab='',xlim=range(years),ylab='')
			lines(years+0.22,effic.alloc,type='h',col='grey75',lwd=9,lend=3)
			abline(h=0)
			axis(4,lab=F)
		}
		mtext('Relative efficiency (%)', 2, 3.5, adj =2.5,cex = 1.25)
		mtext('Year', 1, 2.5, cex = 1.25)
	
#		legend("topleft", c("strata","allocation"), pch=c(15,15),col=c('black','grey60'),inset=.05,bty='n')
	}
	print("design.anal end")
	print(Sys.time())
	
	return(stratadata=stratadata)
	
}



