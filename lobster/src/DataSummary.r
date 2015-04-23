	


	# Growth parameters [1=male, 2=female, 3=berried]
	
	# length-weight 
	a=c(0.000608,0.001413,0.00482)
	b=c(3.0583,2.8746,2.638)

	# VB
	Linf=c(281,207)
	k=c(0.065,0.089)
	t0=c(0.76,0.42)
	

	FSRScpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","FSRScpue.csv"))
	SlipLand.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","AnnualSlipLand.csv"))
	LOGScpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","CommercialCPUE.csv"))
	

	xyplot(pred.s.cpue~SYEAR|LFA, data=subset(FSRScpue.dat,LFA%in%c('28','29','30','31A','31B','32')), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", main="", as.table=T,type='b',ylim=c(0,4.5))
	xyplot(pred.s.cpue~SYEAR|subarea, data=subset(FSRScpue.dat,LFA=='27'), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,4.5))
	xyplot(pred.s.cpue~SYEAR|subarea, data=subset(FSRScpue.dat,LFA=='33'), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,4.5))

	lobster.db('atSea')
	atSea.LFA29.dat<-subset(atSea,LFA==29)
