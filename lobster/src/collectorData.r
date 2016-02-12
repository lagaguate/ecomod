loadfunctions(c('lobster','utility','growth'))
options(stringsAsFactors=F)
switch.ecomod.data.directory(2)

FigLoc = file.path(project.figuredirectory('lobster'),'collectors')
dir.create(FigLoc, showWarnings=F)
fp = file.path(project.datadirectory('lobster'),'data')

x = read.csv(file.path(fp,'CollectorData2016.csv'))

x$Study.Area = trimws(x$Study.Area,'right') #lobster bay has space in name sometimes
x = rename.df(x,"Size.CL..CW.or.TL",'Size')                   
#set up counter for individuals
x$n = 1

#distinct number of collectors sampled per year

		a = data.frame(unique(cbind(x$Year,x$Collector.Number,x$Study.Area)))
		names(a) = c('Year','Collector.Count','Study.Area')
		a = aggregate(Collector.Count~Year+Study.Area,data=a,FUN=length)
		x = merge(x,a,by=c('Year','Study.Area'))
#cpue by area by year
	ax = aggregate(n~Study.Area+Year+Collector.Count+Common.Name,data=x,FUN=sum)
#ax$Density = ax$n / 0.56 #m2 of the traps to get to density per m2
	ax$Density = ax$n/ax$Collector.Count

	axL = ax[which(ax$Common.Name== 'American Lobster'),]
	axL = axL[order(axL$Study.Area,axL$Year),]

	
#size frequencies
	y = x[which(x$Common.Name=='American Lobster'),]

	si = unique(y$Study.Area)

for(s in si) {
	pdf(file.path(FigLoc,paste(s,'sizeFrequency.pdf',sep="")))
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s)
	dev.off()
	}

#lobster bay suction
xs = read.csv(file.path(fp,'Suction2005-2013.csv')) #lobster Bay only
xs = rename.df(xs,"Size.CL..CW.or.TL",'Size')                   
ys = xs[which(xs$Species=='Homarus americanus'),]
yss = ys[which(ys$Size<=100),'Size']
a = hist(yss,breaks = seq(1,100,2),main='Lobster Bay Suction')

require(mixtools)
	sC = normalmixEM(yss,k=4, mu=c(8,18,22,40),sigma=c(0.5,1,1,1) )
		histAllMixtures(sC,breaks=seq(1,100,2),main='LobsterBaySuction',prob=T,xlab='CL')	
		savePlot(file.path(getwd(),'LobsterBaySuction.png'),'png')


#mixture modelling for Lobster Bay, Port Latour, St Mary's Bay and Cape Breton

require(mixtools)
	s = 'Lobster Bay'
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s)
	sC = normalmixEM(yy,k=3, mu=c(8,22,38),sigma=c(0.5,1,3) )
	histAllMixtures(sC,breaks=seq(1,100,2),main=s,prob=T,xlab='CL')	
	savePlot(file.path(getwd(),'LobsterBay.png'),'png')


	s = "St Mary's Bay"
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s)
	sC = normalmixEM(yy,k=3, mu=c(8,22,38),sigma=c(0.5,1,3) )
	histAllMixtures(sC,breaks=seq(1,100,2),main=s,prob=T,xlab='CL')	
	savePlot(file.path(getwd(),'St Marys.png'),'png')


	s = 'Cape Breton'
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s)
	sC = normalmixEM(yy,k=3, mu=c(8,22,38),sigma=c(0.5,1,3) )
	histAllMixtures(sC,breaks=seq(1,100,2),main=s,prob=T,,xlab='CL')	
	savePlot(file.path(getwd(),'CapeBreton.png'),'png')


	s = "Port LaTour"
	yy = as.numeric(y[which(y$Study.Area==s),'Size'])
	hist(yy,breaks = seq(1,100,2),main=s)

	sC = normalmixEM(yy,k=3, mu=c(8,22,38),sigma=c(0.5,1,3) )
	histAllMixtures(sC,breaks=seq(1,100,2),main=s,prob=T)	
	savePlot(file.path(getwd(),'PortLatourLobster.png'),'png')

#Beaver Harbour suction sampling
	require(chron)
	ws = read.csv(file.path(fp,'beaverHarbourSuction1991-2009.csv')) #lobster Bay only
	ws = ws[which(ws$Species==1),]
	ws$Date = as.Date(ws$Date,"%d-%b-%y")
	ws$Year = factor2number(years(ws$Date))
	ws$Months = factor2character(months(ws$Date))
	
	a = unique(ws$Year)
	for(y in a) {
		x11()
	 	with(ws[which(ws$Year==y & ws$Month=='October'),],hist(Size,breaks=seq(1,100,2),main=y))
		}


