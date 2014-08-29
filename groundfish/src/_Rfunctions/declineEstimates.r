declineEstimates<-function(dd,syear,eyear) {
#dd comes from trawlMensurationComparison
	
	dd<-subset(dd,dd$Year>=syear & dd$Year<=eyear)
	a<-coef(lm(log(MeanCorrected)~Year,data=dd))
	b<-coef(lm(log(MeanTraditional)~Year,data=dd))
	dg<-c(Rate.of.Change=-1*round(100*(1-exp(a[2]*nrow(dd))),digits=1),Years=paste(syear,eyear,sep="-"))
	df<-c(Rate.of.Change=-1*round(100*(1-exp(b[2]*nrow(dd))),digits=1),Years=paste(syear,eyear,sep="-"))
		with(dd,plot(Year,log(MeanCorrected),type='b',col='red',pch=16))
		with(dd,lines(Year,log(MeanTraditional),type='b',col='black',pch=16))
		legend('bottomleft',lty=c(1,1),pch=c(16,16),col=c('red','black'),c('Depth Corrected SA','Traditional SA'),cex=0.75,bty='n')

	abline(a=a[1],b=a[2],lty=1,col=1,lwd=2)
	abline(a=b[1],b=b[2],lty=1,col=2,lwd=2)
	return(list(Corrected=dg,Traditional=df))
	}