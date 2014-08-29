trawlMensurationComparison <- function(species,metric='TOTWGT',species.name,strat1=440,strat2=495) {
		a =	sqlQuery(channel,paste("select i.mission,i.setno,(dmin+dmax)/2 depth, strat, sdate, dist,totno,totwgt from groundfish.gsinf i, groundfish.gscat c where i.mission=c.mission and i.setno=c.setno and spec=",species," and to_char(sdate,'mm') in ('06','07','08') and strat between '",strat1,"' and '",strat2,"'  and type=1",sep=""))
		b =	sqlQuery(channel,paste("select i.mission,i.setno,(dmin+dmax)/2 depth, strat, sdate,dist, 0 totno, 0totwgt from groundfish.gsinf i where to_char(sdate,'mm') in ('06','07','08') and strat between '",strat2,"' and '",strat2,"' and type=1"))
		d = sqlQuery(channel,paste("select strat,area from groundfish.gsstratum"))
		
		d$areakm2 <- d$AREA*3.429904
		
		e = rbind(a,b)
		e = e[!duplicated(e[,c('MISSION','SETNO')]),]
		
		e$DEPTH = e$DEPTH*1.8288
		
		e <- merge(e,d,by='STRAT')
		e <- e[as.numeric(substr(e$MISSION,4,7))>1983,]	
		#sa trawled==5.10*log10(depth)+6.51
		
		e$wspread = (5.10*log10(e$DEPTH)+6.51)/1000
		e$distkm = e$DIST*1.852
		
		e$sacorr = e$wspread * e$distkm
		e$satra  =	41*0.0003048 * e$distkm #ft to km
		e$yr = substr(e$MISSION,4,7)
		
		e$metcor = e[,metric]/e$sacorr
		e$mettra = e[,metric]/e$satra
		
		f = aggregate(cbind(metcor,mettra)~yr+STRAT+areakm2,data=e,FUN=mean)
		
		g = merge(f,setNames(aggregate(areakm2~yr,data=f,FUN=sum),c('yr','sumarea')),by='yr')
		g$wt = g$areakm2/g$sumarea
		
		h = setNames(aggregate(cbind(metcor*wt,mettra*wt)~yr,data=g,FUN=sum),c('Year','MeanCorrected','MeanTraditional'))
		h$Year = as.numeric(h$Year)
		if(metric=='TOTNO') yl= expression(N / km^2)
		if(metric=='TOTWGT') yl= expression(kg / km^2)
		
		ylim=max(h[,c('MeanCorrected','MeanTraditional')])
		
		with(h,plot(Year,MeanCorrected,type='b',col='red',ylim=c(0,ylim),ylab=yl,pch=16))
		with(h,lines(Year,MeanTraditional,type='b',col='black',pch=16))
		legend('bottomleft',lty=c(1,1),pch=c(16,16),col=c('red','black'),c('Depth Corrected SA','Traditional SA'),cex=0.75,bty='n')
		title(species.name)
		
return(h)
}

