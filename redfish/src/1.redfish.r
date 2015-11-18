	  p = list()
	  p$init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
	  p$init.files = c(p$init.files,loadfunctions(c("BIOsurvey",'polygons','lobster') ))
	  p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp" )  )
	        
	  fp = file.path(project.datadirectory('redfish'),"analysis")
	  dir.create(fp, recursive = TRUE, showWarnings = FALSE )


	 rebuild.groundfish.data = F
	 if(rebuild.groundfish.data) { 
			  odbc.data.yrs=1970:2015
			  groundfish.db( DS="odbc.redo", datayrs=odbc.data.yrs )  
			  groundfish.db( DS="gsgears.redo" )
			  groundfish.db( DS="gscat.redo" )
			  groundfish.db( DS="gsdet.redo" )
			  groundfish.db( DS="gsinf.redo" )
			  groundfish.db( DS="gshyd.profiles.redo" )
			  groundfish.db( DS="gshyd.redo" )
			  groundfish.db( DS="gshyd.georef.redo" )  # not used here but used in temperature re-analysis
			   }

	#set the parameters for doing the stratified analysis
			p$strat=456:495  #unit III
			p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
			p$years.to.estimate = c(1970:2015)
			p$species = 23
			p$vessel.correction = T
			p$vessel.correction.fixed = 1.2
			p$length.based = T
			p$size.class= c(0,22)
			#p$size.class= c(23,70)
			p$by.sex = F
			p$sex = 1# male female berried c(1,2,3)
			p$functional.groups = F
			p$bootstrapped.ci = F
			p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
			p$runs = p$runs[order(p$runs$v),]


	#set up the frame to do the analysis
	aout= groundfish.analysis(DS='stratified.estimates.redo',p=p,out.dir= 'redfish')
	
	aout= groundfish.analysis(DS='stratified.estimates',p=p,out.dir= 'redfish') #returns all the redfish stratified data

#by cm

len = 1:65
oo = NULL
for(l in len) {
	p$size.class = c(l,l)
	aout= groundfish.analysis(DS='stratified.estimates.redo',p=p,out.dir= 'redfish')
	aout$len.group = l
	oo = rbind(oo,aout)
   }
   save(oo,file = file.path(project.datadirectory('redfish'),'analysis','stratified.at.length.redfish.rdata'))

		out = reshape(oo[,c('yr','len.group','n.Yst')], timevar= 'len.group',idvar='yr',direction='wide')
		pdf(file.path(project.datadirectory('redfish'),'figures','UnitIIIbubbles.pdf'))
		matrixBubbles(t(out[,2:46]),xr=1:46,yr=1:45,maxinch=0.2,xlab='Year',ylab='Length',yc.colors=T,ttl='Unit III Redfish')
		dev.off()

#vonB

		loadfunctions('model.fishery.general')
		dat = read.csv(file.path(project.datadirectory('redfish'),'data','LEN.AGE.csv'),header=T)
		vB = vonB(dat = dat, ODBC=F, conditional.bootstrap=T)
		savePlot(file.path(project.datadirectory('redfish'),'figures','vonB.png'),type='png')

#combining bubbles and vonBert

		matrixBubbles(t(out[1:12,2:46]),xr=1:46,yr=1:12,maxinch=0.2,xlab='Year',ylab='Length',yc.colors=T,ttl='Unit III Redfish')
		 v = vB[[1]]
		 A = 1:20
		 ht <- v[1,1]*(1-exp(-v[2,1]*(A-v[3,1])))     # von Bertalanffy equation #

#1970-1985
		pdf(file.path(project.datadirectory('redfish'),'figures','UnitIIIbubbles1970-1980.pdf'))
		matrixBubbles(t(out[1:15,2:46]),xr=1:15,yr=1:45,maxinch=0.2,xlab='Year',ylab='Length',yc.colors=T,ttl='Unit III Redfish')
		lines(1:12,ht[7:18],lwd=3,col='red')
		lines(2:13,ht[6:18],lwd=3,col='red')
		lines(5:15,ht[5:15],lwd=3,col='red')

		dev.off()

#1980-1995

		pdf(file.path(project.datadirectory('redfish'),'figures','UnitIIIbubbles1980-1995.pdf'))
		matrixBubbles(t(out[10:25,2:46]),xr=1:16,yr=1:45,maxinch=0.2,xlab='Year',ylab='Length',yc.colors=T,ttl='Unit III Redfish',xlabs=seq(1980,1995,5))
		lines(4:15,ht[9:20],lwd=3,col='red')
		lines(6:19,ht[4:17],lwd=3,col='red')

		dev.off()

#1990-2005

		pdf(file.path(project.datadirectory('redfish'),'figures','UnitIIIbubbles1990-2005.pdf'))
		matrixBubbles(t(out[20:35,2:46]),xr=1:16,yr=1:45,maxinch=0.2,xlab='Year',ylab='Length',yc.colors=T,ttl='Unit III Redfish',xlabs = seq(1990,2005,by=5))
		lines(6:19,ht[4:17],lwd=3,col='red')
		lines(10:19,ht[6:15],lwd=3,col='red')

		dev.off()


#2000-2015

		pdf(file.path(project.datadirectory('redfish'),'figures','UnitIIIbubbles2000-2015.pdf'))
		matrixBubbles(t(out[30:45,2:46]),xr=1:16,yr=1:45,maxinch=0.2,xlab='Year',ylab='Length',yc.colors=T,ttl='Unit III Redfish',xlabs = seq(2000,2015,by=5))
			lines(8:18,ht[5:15],lwd=3,col='red')
		dev.off()


#habitat associations
p$strata.files.return =T
p$plot.name = 'unit3redfish.habitat.associations.pdf'
aout = groundfish.analysis(DS = 'species.set.data',p=p,out.dir='redfish')
#figure.habitat.associations(aout,p=p,out.dir='redfish',f.name='unit3redfish.habitat.associations.22-70')
figure.habitat.associations(aout,p=p,out.dir='redfish',f.name='unit3redfish.habitat.associations.0-22')


#figure stratified analysis

			p$add.reference.lines = F
			p$time.series.start.year = 1970
			p$time.series.end.year = 2015
			p$reference.start.year = 1999
			p$reference.end.year = 2013
			p$add.primary.line = F # the center estimate for reference point
			p$metric =  'weights' #'numbers'#
			p$measure = 'stratified.mean' #'stratified.total'

			p$reference.measure = 'median' # mean, geomean 
			p$file.name = 'unit3redfish.weight<22cm.png'
			#p$file.name = 'unit3redfish.weight.>22cm.png'
			       
			#stock reference lines based on primary measure as above
			  p$add.upper.lower = F
			        p$upper.reference.line = 0.8
			        p$lower.reference.line = 0.4
			        p$figure.title = 'Unit III redfish >22cm'
			        #p$figure.title = 'Unit III redfish <22cm'
			        p$y.maximum = NULL # NULL # if ymax is too high for one year
			    	p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
			        p$legend = F
			        p$legend.placement = 'topright'
			        p$running.median = T
					p$running.length = 3
					p$running.mean = F #can only have rmedian or rmean
			        p$error.polygon=F
			        p$error.bars=T
					

			     ref.out=   figure.stratified.analysis(x=aout,p=p,out.dir='redfish')

			sfp = file.path(fp,'analysis','saved p files')
			dir.create(sfp,recursive=T,showWarnings=F)
			save(p,file=file.path(sfp,paste('pfile','unit3redfish>22cm',p$series,'strata',min(p$strat),max(p$strat),'rdata',sep=".")))
			save(p,file=file.path(sfp,paste('pfile','unit3redfish<22cm',p$series,'strata',min(p$strat),max(p$strat),'rdata',sep=".")))


#spatial pie plots

	aout= groundfish.analysis(DS='species.set.data',p=p,out.dir= 'redfish') #returns all the redfish stratified data
	
	aout$yr = substring(aout$mission,4,7)
	combined.yrs = 5
	yrs = seq(1970,2010,combined.yrs)
	vars.to.keep = c('slong','slat','totno.0-22','totno.23-70')
	
	for(y in yrs) {
	pdf(file.path(fp,paste('piecharts',y,y+combined.yrs,sep=".")))
		LobsterMap(ylim=c(42,46.5),xlim=c(-68,-58),boundaries='uu',labels='nn',addSummerStrata=F)
		n = aout[which(aout$yr %in% seq(y,y+combined.yrs)),vars.to.keep]
		n$slong = n$slong*-1
		z = as.matrix(n[,grep('totno',names(n))])
		
		draw.pie(x=n$slong,y=n$slat,z=z,radius=.2)
		dev.off()
	}