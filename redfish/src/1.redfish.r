	  loadfunctions( "redfish", functionname="initialise.local.environment") 
	

	        
	  fp = file.path(project.datadirectory('redfish'),"analysis")
	  dir.create(fp, recursive = TRUE, showWarnings = FALSE )

	#set the parameters for doing the stratified analysis
			p$length.based = T
			#p$size.class= c(0,22)
			p$size.class= c(23,70)
			p$by.sex = F
			p$sex = 1# male female berried c(1,2,3)
			p$bootstrapped.ci = T
			p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
			p$runs = p$runs[order(p$runs$v),]
			p$strata.files.return = F

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




	#set up the frame to do the analysis
	aout= groundfish.analysis(DS='stratified.estimates.redo',p=p,out.dir= 'redfish')
	
	aout= groundfish.analysis(DS='stratified.estimates',p=p,out.dir= 'redfish') #returns all the redfish stratified data

#survey numbers at length by cm
if(redo.numbers.at.length) {
				len = 1:50
				oo = NULL
				p$length.based=T
				p$bootstrapped.ci = F
				for(l in len) {
					p$size.class = c(l,l)
					aout= groundfish.analysis(DS='stratified.estimates.redo',p=p,out.dir= 'redfish')
					aout$len.group = l
					oo = rbind(oo,aout)
				   }
				   save(oo,file = file.path(project.datadirectory('redfish'),'analysis','stratified.at.length.redfish.rdata'))
}

#figure stratified analysis
if(redo.stratied.figure) {
				p$add.reference.lines = F
				p$time.series.start.year = 1982
				p$time.series.end.year = 2015
				p$reference.start.year = 1999
				p$reference.end.year = 2013
				p$add.primary.line = F # the center estimate for reference point
				p$metric =  'numbers' #'weights' #'numbers'#
				p$measure = 'stratified.mean' #'stratified.total'

				p$reference.measure = 'median' # mean, geomean 
				
				#p$file.name = 'unit3redfish.numbers.less.22cm.png'
				#a = aout[which(aout$group=='0-22'),]
				#p$file.name = 'unit3redfish.weights.greater.22cm.png'
				#a = aout[which(aout$group=='23-70'),]
				
				p$file.name = 'unit3redfish.weights.all.png'
				a = aout[which(aout$group=='all'),]
				
				p$file.name = 'unit3redfish.numbers.all.png'
				       
				#stock reference lines based on primary measure as above
				  p$add.upper.lower = F
				        p$upper.reference.line = 0.8
				        p$lower.reference.line = 0.4
				        p$figure.title = 'Unit III redfish '
				        #p$figure.title = 'Unit III redfish <22cm'
				        p$y.maximum = NULL # NULL # if ymax is too high for one year
				    	p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
				        p$legend = F
				        p$legend.placement = 'topright'
				        p$running.median = T
						p$running.length = 5
						p$running.mean = F #can only have rmedian or rmean
				        p$error.polygon=F
				        p$error.bars=T
						

				     ref.out=   figure.stratified.analysis(x=a,p=p,out.dir='redfish')

				sfp = file.path(fp,'analysis','saved p files')
			dir.create(sfp,recursive=T,showWarnings=F)
			save(p,file=file.path(sfp,paste('pfile','unit3redfish>22cm',p$series,'strata',min(p$strat),max(p$strat),'rdata',sep=".")))
			#save(p,file=file.path(sfp,paste('pfile','unit3redfish<22cm',p$series,'strata',min(p$strat),max(p$strat),'rdata',sep=".")))
}

if(redo.strata.efficiencies) {
	p$strata.efficiencies = T
	p$bootstrapped.ci = F
	aout= groundfish.analysis(DS='stratified.estimates.redo',p=p,out.dir= 'redfish')
	save(aout,file = file.path(project.datadirectory('redfish'),'analysis','survey.efficiency.rdata'))
	#these are currently for commercial size fish
	load(file = file.path(project.datadirectory('redfish'),'analysis','survey.efficiency.rdata'))
		pdf(file.path(project.datadirectory('redfish'),'figures','surveyefficiency.pdf'))
			with(aout[[1]],plot(yr-0.1,strat.effic.wt,type='h',col='black',xlab='Year', lwd=2,ylab='Efficiency (%)',ylim=c(-100,100)))
			with(aout[[1]],lines(yr+0.1,alloc.effic.wt,type='h',col='grey40',lwd=2,ylab='Efficiency (%)',ylim=c(-100,100)))
			legend('topright',lty=c(1,1),lwd=2,col=c('black','grey40'),c('Strata Efficiency','Allocation Efficiency'),bty='n',cex=0.9)
			dev.off()
}

if(abundance.distribution.plot) {
	aout= groundfish.analysis(DS='stratified.estimates',p=p,out.dir= 'redfish') #returns all the redfish stratified data
	a = aout[which(aout$group=='all'),]
	a$pc =  "."
	pdf(file.path(project.datadirectory('redfish'),'figures','abundance.area.relationship.pdf'))
	with(a, plot((dwao),log(n.Yst),type='b',pch=a$pc,xlab='DWAO',ylab='log(Abundance)'))
	b = a[which(a$yr %in% seq(1970,2015, by=15)),]
	d = a[-which(a$yr %in% seq(1970,2015, by=15)),]
	text(labels=b$yr,x=b$dwao,y=log(b$n.Yst))
	points(d$dwao,log(d$n.Yst),pch=16)
	dev.off()
}


#make the map of strata that need extra allocations, this comes from teh redo.strata.efficiencies above
if(make.map.allocation) {
	 loadfunctions('redfish',functionname='initialise.local.environment')
	 load(file = file.path(project.datadirectory('redfish'),'analysis','survey.efficiency.rdata'))
	a = aout[[2]]
	ll = NULL
	f=0
	for(i in 1:length(a)) {
		b = a[[c(i,2)]]
		b$yr = a[[c(i,1)]]
		if(any(!is.na(b$Optimal))) {
			f=f+1
		if(f==1) {
			ll = b[,c(1:3,7)]
			} else {
			b = b[,c(1:3,7)]
			ll = rbind(ll,b)
				}
			}
		}
		ll$Inc = ifelse(ll$Observed<ll$Optimal,1,0)
		ll$N = 1
			h = aggregate(cbind(Inc,N)~Strata,data=ll,FUN=sum)
			h$perc = h$Inc/h$N
		}	
		h$Z = h$perc*100
		h = h[-which(h$Strata=='Total'),]
		h$PID = as.numeric(h$Strata)
				loadfunctions('polygons')
			  b = find.ecomod.gis('strat.gf',return.one.match=F)
			  b = read.table(b)
			  names(b) <- c('X','Y','PID')
			  b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})
			  b = b[which(b$PID %in% p$strat),]
			  brks = c(.2,.4,.6,.8)*100
				lbrks = length(brks)
			  cols<-c(brewer.pal(lbrks+1,'Greens'))
			  bb = makeProps(h,brks,'col',cols)
		pdf(file.path(project.datadirectory('redfish'),'figures','improved.allocation.pdf'))
		 LobsterMap(ylim=c(42,46.5),xlim=c(-68,-58),boundaries='uu',labels='nn',addSummerStrata=F,poly.lst=list(b,bb))
 			addPolys(b)
 			ContLegend('bottomright',title='Percent of Years' ,bty='n',cex=0.8,lvls = brks,Cont.data = data.frame(col=cols))
	 dev.off()
}


#make the catch at length bubble plot
	if(redo.survey.bubbles) {
				load(file.path(project.datadirectory('redfish'),'analysis','stratified.at.length.redfish.rdata')) #This matches PComeau's #s
				out = reshape(oo[,c('yr','len.group','n.yst')], timevar= 'len.group',idvar='yr',direction='wide')
				pdf(file.path(project.datadirectory('redfish'),'figures','UnitIIIsurveybubbles.pdf'))
				matrixBubbles(t(out[,2:46]),xr=1:46,yr=1:45,maxinch=0.2,xlab='Year',ylab='Length',yc.colors=T,ttl='Unit III Redfish')
				lines(1:10,ht[7:16],lwd=3,col='red')
				lines(28:37,ht[7:16],lwd=3,col='red')
					dev.off()
		
			}
#commercial numbers at length by cm

if(redo.commercial.bubbles) {
				load(file.path(project.datadirectory('redfish'),'data','commercial.numbers.at.length.rdata')) #This is from pcomeau
				out=a
				pdf(file.path(project.datadirectory('redfish'),'figures','UnitIIIcommercialbubbles.pdf'))
				matrixBubbles(t(out[,2:46]),xr=1:45,yr=1:45,maxinch=0.2,xlab='Year',ylab='Length',yc.colors=T,ttl='Unit III Redfish')
				dev.off()	
			}
#need to make the histograms of the numbers at length by year and combined the two series

#make the numbers at length freqs for commercial and survey standardized to one for each year
if(make.length.freq.comps) {
	#commercial
			load(file.path(project.datadirectory('redfish'),'data','commercial.numbers.at.length.rdata')) #This is from pcomeau
				outc=a
	#survey
	load(file.path(project.datadirectory('redfish'),'analysis','stratified.at.length.redfish.rdata')) #This matches PComeau's #s
				outs = reshape(oo[,c('yr','len.group','n.yst')], timevar= 'len.group',idvar='yr',direction='wide')
	
	for(i in 1:nrow(outc)) {
		a = outc[i,2:51]
		s = outs[i,2:51]
		pdf(file.path(project.datadirectory('redfish'),'figures',paste('length.freqs',outc[i,1],'pdf',sep=".")))
		plot(1:50,a/sum(a),type='l',lwd=3,col='grey30',cex.axis=2,xlab=c('Length (cm)'),ylab='Proportion',cex.lab=1.8)
		lines(1:50,s/sum(s),type='l',lwd=3,col='black')
		legend('topleft',bty='n',legend=outc[i,1],cex=2.5)
		dev.off()		

	}				

}


#mean weight at length
if(redo.mean.wt.at.length) {
		sc = c(20,23,26)
		o = NULL
		for(i in 1:length(sc)) {
			p$size.class = c(sc[i],sc[i])
			a = groundfish.analysis(DS='mean.wt.at.length',p=p,out.dir= 'redfish')
			a$length = sc[i]
			o = rbind(o,a)
			}
		save(o,file= file.path(project.datadirectory('redfish'),'analysis','mean.wt.at.length.r'))

		plot(1,1,xlab='Year',xlim=c(min(o$yr,na.rm=T),max(o$yr,na.rm=T)),ylim=c(min(o$meanWt,na.rm=T),350),ylab='Mean Weight at Length (g)',type='n')
		
		with(o[which(o$length==sc[1]),],lines(yr,meanWt,col='red',lty=2,pch=16,type='b'))
		a =with(o[which(o$length==sc[1]),],summary(lm(meanWt~yr)))
		abline(a=coef(a)[1],b=coef(a)[2],col='red',lwd=2)
		
		with(o[which(o$length==sc[2]),],lines(yr,meanWt,col='blue',lty=2,pch=16,type='b'))
		a1 =with(o[which(o$length==sc[2]),],summary(lm(meanWt~yr)))
		abline(a=coef(a1)[1],b=coef(a1)[2],col='blue',lwd=2)
		
		with(o[which(o$length==sc[3]),],lines(yr,meanWt,col='black',lty=2,pch=16,type='b'))
		a2 =with(o[which(o$length==sc[3]),],summary(lm(meanWt~yr)))
		abline(a=coef(a2)[1],b=coef(a2)[2],col='black',lwd=2)
		
		legend('top',c('20 cm', '23 cm','26 cm'),lty=c(2,2,2),pch=16,col=c('red','blue','black'),bty='n',ncol=3)						
savePlot(file.path(project.datadirectory('redfish'),'figures','weight.at.length.png'),type='png')
		}
#vonB
if(redo.vonB){
		loadfunctions('model.fishery.general')
		dat = read.csv(file.path(project.datadirectory('redfish'),'data','LEN.AGE.csv'),header=T)
		vB = vonB(dat = dat, ODBC=F, conditional.bootstrap=T)
		v = vB[[1]]
		 A = 1:45
		vM = c(40.31,0.119,-1.05) #Sabiorido-Rey et al.
		vF = c(44.04,0.103,-1.19) #Sabiorido-Rey et al.
		 ht <- v[1,1]*(1-exp(-v[2,1]*(A-v[3,1])))     # von Bertalanffy equation #
		htM <- vM[1]*(1-exp(-vM[2]*(A-vM[3])))     # von Bertalanffy equation #
		htF <- vF[1]*(1-exp(-vF[2]*(A-vF[3])))     # von Bertalanffy equation #
		lines(A,htM,col='red',lwd=2)
		lines(A,htF,col='green',lwd=2)
		legend('bottomright',lty=c(1,1,1),col=c('blue','red','green'),c('Estimated','SR Male','SR Female'),bty='n',lwd=2)
		savePlot(file.path(project.datadirectory('redfish'),'figures','vonB.png'),type='png')
		}

		 
#habitat associations
if(redo.perry.smith.habitats) {
			p$strata.files.return =T
			p$plot.name = 'unit3redfish.habitat.associations.pdf'
			aout = groundfish.analysis(DS = 'species.set.data',p=p,out.dir='redfish')
			figure.habitat.associations(aout,p=p,out.dir='redfish',f.name='unit3redfish.habitat.associations.22-70')
			#figure.habitat.associations(aout,p=p,out.dir='redfish',f.name='unit3redfish.habitat.associations.0-22')
			}


#spatial pie plots
if(redo.spatial.pies) {
fp = file.path(project.datadirectory('redfish'),"figures")
	  
	p$strata.files.return=NULL
	aout= groundfish.analysis(DS='species.set.data',p=p,out.dir= 'redfish') #returns all the redfish stratified data
		aout$yr = substring(aout$mission,4,7)
		combined.yrs = 5
	yrs = seq(1970,2010,combined.yrs)
	vars.to.keep = c('slong','slat','totno.0-22','totno.23-70')
	
	for(y in yrs) {
			pdf(file.path(fp,paste('piecharts',y,y+combined.yrs,'pdf',sep=".")))
				LobsterMap(ylim=c(42,46.5),xlim=c(-68,-58),boundaries='uu',labels='nn',addSummerStrata=F)
				n = aout[which(aout$yr %in% seq(y,y+combined.yrs)),vars.to.keep]
				n$slong = n$slong*-1
				z = as.matrix(n[,grep('totno',names(n))])
				title(paste(y,y+combined.yrs,sep="-")	)
				draw.pie(x=n$slong,y=n$slat,z=z,radius=.2)
				dev.off()
	}
}


#logbook data
	if(logbook.redo){
		redfish.db('logbook.redo')
		}


#filter logbook data to unit3 and make fishery footprint plots
	red = redfish.db('filter.region.unit3')
	redfishFisheryFootprint(x=red)

#make fishery footprint plots for summer months only
	red.summer = red[which(red$month_fished %in% c(6,7,8)),]
	redfishFisheryFootprint(x=red.summer,fig.lab='Summer')

#adding in the summer survey plots over the fishery footprint
	
	p$strata.files.return = NULL
	aout= groundfish.analysis(DS='species.set.data',p=p,out.dir= 'redfish') #returns all the redfish stratified data
	aout$yr = substring(aout$mission,4,7)
	vars.to.keep = c('yr','slong','slat','totno.23-70')
	aout = aout[,vars.to.keep]
	red.summer = red[which(red$month_fished %in% c(6,7,8)),]
	
	redfishFisheryFootprint(x=red.summer,fig.lab='Summer.Survey',survey.data=aout)

	
	for(y in yrs) {
			pdf(file.path(fp,paste('footPrintSurveyPiecharts',y,sep=".")))
				LobsterMap(ylim=c(42,46.5),xlim=c(-68,-58),boundaries='uu',labels='nn',addSummerStrata=F)
				n = aout[which(aout$yr %in% seq(y,y+combined.yrs)),vars.to.keep]
				n$slong = n$slong*-1
				z = as.matrix(n[,grep('totno',names(n))])
				title(paste(y,y+combined.yrs,sep="-")	)
				draw.pie(x=n$slong,y=n$slat,z=z,radius=.2)
				dev.off()
	}
	