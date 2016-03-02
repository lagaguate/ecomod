####### Update 27-33: Feb 2015



#---------------------------------------------------------------------------Plots for Update
	require(lattice)
	require(plyr)

    # run script for modelling catch rates in FSRS traps
    loadfunctions( "lobster", functionname="FSRScpue.r") 

	
	# FSRS CPUE
	shorts<-read.csv(file.path( project.datadirectory("lobster"), "data","products","FSRSmodelresultsSHORT.csv"))
	legals<-read.csv(file.path( project.datadirectory("lobster"), "data","products","FSRSmodelresultsLEGAL.csv"))



	# shorts
	pdf(file.path( project.datadirectory("lobster"),"figures","FSRSshorts.pdf"),8, 10)


	p <- ggplot()
	p <- p + geom_point(data = shorts, aes(y = mu, x = YEAR), shape = 16, size = 3)
	p <- p + xlab("Year") + ylab("Lobsters / Trap")
	p <- p + theme(text = element_text(size=15)) + theme_bw()
	p <- p + geom_line(data = shorts, aes(x = YEAR, y = mu), colour = "black")
	p <- p + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	p <- p + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	p

	dev.off()

	# legals

	pdf(file.path( project.datadirectory("lobster"),"figures","FSRSlegals.pdf"),8, 10)

	p <- ggplot()
	p <- p + geom_point(data = legals, aes(y = mu, x = YEAR), shape = 16, size = 3)
	p <- p + xlab("Year") + ylab("Lobsters / Trap")
	p <- p + theme(text = element_text(size=15)) + theme_bw()
	p <- p + geom_line(data = legals, aes(x = YEAR, y = mu), colour = "black")
	p <- p + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	p <- p + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	p

	dev.off()

	wd=9
	ht=6
	wd.r=0.7
	ht.r=0.62


	# Logs CPUE
	LOGcpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","CommercialCPUE_LFA.csv"))
	cpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","CommercialCPUE.csv"))
	vollog.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","VolLogsCPUE_LFA.csv"))

	#Manon's Plot version
	#Plot in update displays all commercial and voluntary log CPUEs. Long term mean CPUE from commercial logs (2008-2014) is also included.
	long.term.mean<-ddply(subset(cpue.dat,year<2015),c("lfa","subarea"),summarize,lt.mean=mean(cpue))
	require(ggplot2)

  pdf('Commercial.CPUE.LFA27-33.pdf',width=8,height=10)
	ggplot(subset(cpue.dat,lfa<34),aes(x=as.factor(year),y=cpue, group=lfa)) + geom_point() + geom_line () + facet_wrap(~subarea, ncol=2) +
	  geom_hline(aes(yintercept=lt.mean), colour="blue4", data=subset(long.term.mean,lfa<"34")) + geom_line(vollog.dat,aes(x=year,y=cpue)) +
	  scale_y_continuous(limits=c(0,5.2), breaks=c(0,6,2)) + theme_bw() + theme(axis.text.x=element_text(size=10.0),strip.text=element_text(size=12.0),legend.position="none") +
	  xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
	dev.off()

	pdf('Commercial.CPUE.Vollog.LFA27-33.pdf',width=8,height=10)
	ggplot(vollog.dat,aes(x=as.factor(year),y=cpue, group=lfa, colour="red")) + geom_line () + facet_wrap(~subarea, ncol=2) +
	  geom_segment(aes(x='2008', y=lt.mean,xend='2014',yend=lt.mean), colour="cornflowerblue", data=subset(long.term.mean,lfa<"34")) + geom_line(data=subset(cpue.dat,lfa<34),colour="black") + 
	  geom_point(data=subset(cpue.dat,lfa<34),colour="black") +scale_y_continuous(limits=c(0,2.2), breaks=c(0,1,2)) + scale_x_discrete(breaks=seq(1981,2015,4),
	  labels=c('1981','1985','1989','1993','1997','2001','2005','2009','2013')) + 
	  theme_bw() + theme(axis.text.x=element_text(size=10.0),strip.text=element_text(size=12.0),legend.position="none") + xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
	dev.off()

	
	x11(wd,ht)
	xyplot(cpue~year|lfa, data=subset(cpue.dat,lfa%in%c('28','29','30','31A','31B','32')), ylab="CPUE (Kg / Trap Haul)",xlab= "Year", main="", as.table=T,type='b',ylim=c(0,2.2))
	x11(wd*wd.r,ht*ht.r)
	xyplot(cpue~year|subarea, data=subset(cpue.dat,lfa=='27'), ylab="CPUE (Kg / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,2.2))
	x11(wd*wd.r,ht*ht.r)
	xyplot(cpue~year|subarea, data=subset(cpue.dat,lfa=='33'), ylab="CPUE (Kg / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,2.2))


	# Landings
	LandingsUpdate<-read.table(file.path( project.datadirectory("lobster"), "data","Landings.27-33.1947.2014.txt"),header=T)

	x11(6.5,8)
	bwd<-8
	par(mfrow=c(3,1),las=1,mar=c(2,2,2,2),omi=c(0.2,0.8,0.2,0.2))
	plot(Landings.tons~YR,subset(LandingsUpdate,LFA==27),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 27",ylim=c(0,6000))
	axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)
	plot(Landings.tons~YR,subset(LandingsUpdate,LFA==28.32),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 28-32",ylim=c(0,6000))
	axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)
	plot(Landings.tons~YR,subset(LandingsUpdate,LFA==33),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 33",ylim=c(0,6000))
	axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)

	mtext("Landings (mt)",2,3,outer=T,las=0)

	# for LFA33 presentation
	pdf(file.path( project.datadirectory("lobster"),"R","FSRScpueLFA33.pdf"),8, 5)
	xyplot(cpue~year|subarea, data=subset(cpue.dat,lfa=='33'), ylab="CPUE (Kg / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,1))
	xyplot(pred.s.cpue~SYEAR|subarea, data=subset(FSRScpue.dat,LFA=='33'), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,4.5))
	xyplot(pred.l.cpue~SYEAR|subarea, data=subset(FSRScpue.dat,LFA=='33'), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,1.2))
	dev.off()

	subareas<-read.csv(file.path( project.datadirectory("lobster"), "data","LFA2733subarea.csv"))
	polyprop33<-merge(subset(subareas,LFA==33),data.frame(LFA=33,subarea=c("33 West","33 East"),col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5))),all=T)
	names(polyprop33)[1:3]<-c("PID","label","SID")
	LFAgrid<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFAgridPolys.csv"))

	pdf(file.path( project.datadirectory("lobster"),"R","LFA33map.pdf"),8, 8)
	LobsterMap('33',poly.lst=list(subset(LFAgrid,PID==33),polyprop33),boundaries='none',isobaths=c(seq(50,450,50),seq(500,1000,100)),bathcol=rgb(0,0,1,0.3))
	legend('bottomright',c("West","East"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
	dev.off()
