#delury estimate
delury.leslie <-	function(x,estimate=c('delury','leslie'),day.or.week='week') {
if(nrow(x)>10) {
	x$lncpue <- log(x$cpue)
	
		#if sum across week before average cpue
		
			if(day.or.week=='day')  y <- merge(aggregate(landings~date.fished,data=x,FUN=sum), aggregate(effort~date.fished,data=x,FUN=sum),by='date.fished')
			if(day.or.week=='week') y <- merge(aggregate(landings~weekno,data=x,FUN=sum), aggregate(effort~weekno,data=x,FUN=sum),by='weekno')
			y$cpue <- y$landings/y$effort
			y$lncpue <- log(y$cpue)
			y$sumef <- cumsum(y$effort)
			y$sumlan <- cumsum(y$landings)

		#if average across week before average cpue
			#if(day.or.week=='day' & estimate=='delury')  y <- merge(aggregate(lncpue~date.fished,data=x,FUN=mean), aggregate(effort~date.fished,data=x,FUN=sum),by='date.fished')
			#if(day.or.week=='week'& estimate=='delury')  y <- merge(aggregate(lncpue~weekno,data=x,FUN=mean), aggregate(effort~weekno,data=x,FUN=sum),by='weekno')
			                                                                  
			#if(day.or.week=='day' & estimate=='leslie')  y <- merge(aggregate(lncpue~date.fished,data=x,FUN=mean), aggregate(landings~date.fished,data=x,FUN=sum),by='date.fished')
			#if(day.or.week=='week'& estimate=='leslie')  y <- merge(aggregate(lncpue~weekno,data=x,FUN=mean), aggregate(landings~weekno,data=x,FUN=sum),by='weekno')
			
			
		yi <- max(x[which(x$date.fished<as.Date(paste(unique(x$yr),06,30,sep="-"))),'weekno'])
		
		if(estimate=='delury') {
		y$sumef <- cumsum(y$effort)
		delu <- summary(lm(lncpue~sumef,data=y))$coefficients
		with(y,plot(sumef,lncpue,main=paste('Delury',unique(x$yr),sep="-"),xlab='Effort',ylab='ln(CPUE)'))
		abline(a=delu[1,1],b=delu[2,1],col='red')
		abline(v=y[which(y$weekno==yi),'sumef'],col='red')
		delu <- round(delu, 5) # extract coefficients
	
	mtext(bquote(lncpue == .(delu[2,1])*sumU + .(delu[1,1])-- pslope*.(delu[2,4])), side=1,line=4,cex=0.7) # display equation
		
		return(delu)
		}
		if(estimate=='leslie') {
		y$sumlan <- cumsum(y$landings)
		lesl <- summary(lm(lncpue~sumlan,data=y))$coefficients
		with(y,plot(sumlan,lncpue,main=paste('Leslie',unique(x$yr),sep="-"),xlab='Landings',ylab='ln(CPUE)'))
		abline(v=y[which(y$weekno==yi),'sumlan'],col='red')
		return(lesl)
		}
	}
	}