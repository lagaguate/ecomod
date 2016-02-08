#multiyear modal analysis 




# ----------------------
# using snowcrab data
 
#apply a filter
  source( file.path( project.directory("snowcrab"), "src", "initialise.local.environment.r" ) )
  loadfunctions(c('snowcrab','common'))
  det = snowcrab.db( DS ="det.georeferenced" ) 
	outdir <- file.path(project.directory('snowcrab',"growth"))

gridBased=F  
		if(gridBased) {
		#set up a grid
			  det <- gridSets(det)
			  gr <- det[[2]]
			  cn.gr <- calcCentroid(gr)
			  cn.gr$label <- cn.gr$PID
			  det <- det[[1]]
			ps <- unique(det$PID)	
		
		#plot the length frequency histograms
			outdir <- "C:/~/ecomod/snowcrab/growth"
			dir.create(outdir,recursive=T,showWarnings=F)
		plotHists=F
		if(plotHists) {
			for(i in 1:length(ps)) {
					pdf(file.path(outdir,"spatial.histograms",paste(ps[i],".pdf",sep="")))
					makeMap()
					addPolys(gr[gr$PID==ps[i],],col='red')
					lfHistMatrix(x=det[det$PID==ps[i],],title='All carapace conditions',logbin=F)
					dev.off()	
			}
		}
		
							#stepped through and identified the best data for modal analysis
				runModes = F 
				if(runModes) {	
					fi <- dir(file.path(outdir,"spatial.histograms"))
					good.ps <-	as.numeric(unlist((strsplit(fi,"\\ .pdf"))))
					sex <- c(0,1)	
					
					for(i in 1:length(good.ps)) {
						for(j in 1:length(sex)) {
								x	<- reshapeCW(det[det$PID==good.ps[i] & det$sex==sex[j] ,]) #need to remove the eyars with very sparce data October 25, 2013 03:05:44 PM 
							if(length(x[[1]])>0 & !is.vector(x[[1]])) {
								yrs 	<- x[[2]]
								x 		<- x[[1]]
								xi 		<- identifyModes(x,span=5)
								out 	<- annualMixBoot(x=x,init.mode=xi,ni=100)
								fname 	<- paste("PID",good.ps[i],"-",sex[j],".rdata",sep="")
							
							save(out,file=file.path(outdir,"R",fname))
							alarm()
							}
						}
					}
}
		}

		
		
		
radiusBased=T
if(radiusBased) {

	radius=20 
	a <- unique(det[,c('trip','set','lon','lat')])	
	a <- a[!is.na(a$lon),]
	a$PID <- 1:nrow(a)
	a$ID <- rownames(a)
	b <- a[substr(a$trip,6,9)=='2010',]
	det <- merge(det,a,by=c('trip','set','lon','lat'))
	groups <- list()
	for(i in 1:nrow(b)) {
			g <- whichEventsVincenty(b[i,c('lon','lat')],a[,c('lon','lat')],radius=radius)
			g$ID <- rownames(g)
		 groups[[i]] <- merge(a,g,by=c('ID','lon','lat'))
	}
	
		
	
	plotHists=T
		if(plotHists) {
			dir.create(file.path(outdir,"radius.histograms"),recursive=T,showWarnings=F)
			for(i in 1:length(groups)) {
				u <- a[which(a$PID %in% groups[[i]][,'PID']),]
				v <- groups[[i]][which.min(groups[[i]]$D),'PID']
			z <- det[which(det$PID %in% u$PID),]
			u$X <- u$lon
			u$Y <- u$lat
			u$EID <- 1:nrow(u)
			pdf(file.path(outdir,"radius.histograms",paste(v,"-",radius,".pdf",sep="")))
					makeMap()
					addPoints(u[,c('EID','X','Y')],col='red')
					lfHistMatrix(x=z,title='All carapace conditions',logbin=F)
					dev.off()	
			}
		}
runModes = T
if(runModes) {	
	fi <- dir(file.path(outdir,"radius.histograms"))
		
	good.ps <-	as.numeric(unlist((strsplit(fi,"-20.pdf"))))
	sex <- c(0,1)	
	gg <- unlist(lapply(groups,function(x) x[which.min(x$D),'PID']))
#require(foreach)
#require(doParallel)
#cl=makeCluster(detectCores())
#registerDoParallel(cl)
	for(i in 1:length(good.ps)) {
		for(j in 1:length(sex)) {
			
			dg <- which(gg==good.ps[i])	
				x	<- reshapeCW(det[which(det$PID %in% groups[[dg]][,'PID'] & det$sex==sex[j]) ,]) 
				
			if(length(x[[1]])>0 & !is.vector(x[[1]])) {
				yrs 	<- x[[2]]
				x 		<- x[[1]]
				lw <- apply(x,2,function(x) length(na.omit(x)))
			
			if(any(lw<75)) {
					po <- which(lw<75)	
					x <- x[,-po]
					yrs <- yrs[-po]
				}

				xi 		<- identifyModes(x,span=5)
				vi		<- identifyVariancesLambdas(x,xi,span=5)
				li		<- vi[[2]]
				vi		<- vi[[1]]
				
				

				out 	<- annualMixBoot(x=x,init.mode=xi,ni=5000,mode.thresh=6, init.lam=li,init.var=vi)
				fname 	<- paste("PID",good.ps[i],"-",sex[j],".rdata",sep="")
			
			save(out,file=file.path(outdir,"R",fname))
			alarm()
			}
		}
		
	}
}

	
	
}
		

#set up growth data for growth model
	
pullComp = T 
loadfunctions('snowcrab')
if(pullComp) {
	out 	<- pullModalComponents()
	ou.m1 	<- out$ou.m1
	ou.msd1 <- out$ou.msd1
	ou.m2 	<- out$ou.m2
	ou.msd2 <- out$ou.msd2
	ou.f1 	<- out$ou.f1  	#mean 1 of growth interval female
	ou.fsd1 <- out$ou.fsd1 	#sd 1 of growth interval
	ou.f2 	<- out$ou.f2  
	ou.fsd2 <- out$ou.fsd2
	pid.m 	<- out$pid.m #id the polygon where growth is from male
	pid.f 	<- out$pid.f
	}
	
pullFreqComp = F 
loadfunctions('snowcrab')
if(pullFreqComp) {
	out 	<- pullFrequencyComponents()
	ou.m1 	<- out$ou.m1
	ou.m2 	<- out$ou.m2
	ou.f1 	<- out$ou.f1  
	ou.f2 	<- out$ou.f2  
	pid.m 	<- out$pid.m #id the polygon where growth is from male
	pid.f 	<- out$pid.f
	}

#N-NENS
nens 	<- c(113,114,100,101,102,88:91)
sens 	<- c(5:13,18:26,31:39,44:52,59:65,74:78,88:91)
fx		<- c(1,15,16,29,30,17)
n.m.id 	<- which(pid.m %in% nens)
s.m.id 	<- which(pid.m %in% sens)
fx.m.id <- which(pid.m %in% fx)

n.f.id 	<- which(pid.f %in% nens)
s.f.id 	<- which(pid.f %in% sens)
fx.f.id <- which(pid.f %in% fx)

#x <- modalBreakPointRegression(x1=ou.m1,sd1=ou.msd1,x2=ou.m2,sd2=ou.msd2)
#x <- frequencyBreakPointRegression(x=ou.m1,y=ou.m2)

pdf('SegmentedGrowth.pdf')
x <- meanSegmented(x=ou.m1[n.m.id],y=ou.m2[n.m.id],sd.x=ou.msd1[n.m.id],sd.y=ou.msd2[n.m.id],weighted=F,title='NENS-Male')
x <- meanSegmented(x=ou.f1[n.f.id],y=ou.f2[n.f.id],sd.x=ou.fsd1[n.f.id],sd.y=ou.fsd2[n.f.id],weighted=F,title='NENS-Female')

x <- meanSegmented(x=ou.m1[s.m.id],y=ou.m2[s.m.id],sd.x=ou.msd1[s.m.id],sd.y=ou.msd2[s.m.id],weighted=F,title='NENS-Male')
x <- meanSegmented(x=ou.f1[s.f.id],y=ou.f2[s.f.id],sd.x=ou.fsd1[s.f.id],sd.y=ou.fsd2[s.f.id],weighted=F,title='NENS-Female')


x <- meanSegmented(x=ou.m1[fx.m.id],y=ou.m2[fx.m.id],sd.x=ou.msd1[fx.m.id],sd.y=ou.msd2[fx.m.id],weighted=F,title='4X-Male')
x <- meanSegmented(x=ou.f1[fx.f.id],y=ou.f2[fx.f.id],sd.x=ou.fsd1[fx.f.id],sd.y=ou.fsd2[fx.f.id],weighted=F,title='4X-Female')
dev.off()




#for a later date
#		#
#		#jags model
#		jags.growth <- 'model {
#			#priors
#			alpha 	~ dnorm(0,.5)
#			be  	~ dnorm(0.5,.005)
#			ga	 	~ dnorm(1,.05)
#			ta  	~ dunif(0.001,5)
#			
#			for(i in 1:length(ou.m1)) {
#				ou.p1[i] <- pow(ou.msd1[i],-2)
#				ou.p2[i] <- pow(ou.msd2[i],-2)
#				m1[i] ~ dnorm(ou.m1[i],ou.p1[i])
#				m2[i] ~ dnorm(ou.m2[i],ou.p2[i])	
#				
#				m[i] <- alpha + be * m1[i] ^ ga 
#				m2[i] ~ dnorm(m[i],ta)
#			}
#		}'
#		
#		n <- length(ou.m1)
#		
#		jags <- jags.model(file=jags.setup(jags.growth),data=list(ou.m1=ou.m1,ou.m2=ou.m2,ou.msd1=ou.msd1,ou.msd2=ou.msd2),n.adapt=100)
#		
#		
#		 params <- c('alpha','be','ga','ta')
#		 samps <- coda.samples(jags, params, n.iter = 200000)
#		 plot(samps)
#		 
#		 burn.in <- 1000
#		 summary(window(samps, start = burn.in))
#		 
#		 round(summary(window(samps, start = burn.in))$quantiles[, c(3, 1, 5)], 2)
#		


##--------------------
#simulated data
doSimualted=F
if(doSimulated) {
		loadfunctions('snowcrab') 
		require(mixtools)
		  
		#set up simulatoin data 
			y 		<- simulateMixtures(k=4,means=c(20,40,52,70),sigmas=c(4,3,4,5),prop=c(0.1,0.1,0.25,0.25),size=10000)
		
			multi.year=T
			if(multi.year) {
					y2 		<- simulateMixtures(k=4,means=c(10,18,35,42),sigmas=c(4,3,4,5),prop=c(0.08,0.04,0.15,0.25),size=10000)
					y3 		<- simulateMixtures(k=4,means=c(10,18,35,42),sigmas=c(4,3,4,5),prop=c(0.08,0.04,0.15,0.25),size=10000)
					y4 		<- simulateMixtures(k=4,means=c(10,18,35,42),sigmas=c(4,3,4,5),prop=c(0.08,0.04,0.15,0.25),size=10000)
		
					y <- cbindPad(cbindPad(cbindPad(y,y2),y3),y4)
			}
		}			
	



