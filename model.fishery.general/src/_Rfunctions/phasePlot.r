#phase Plot

phasePlot <- function(biomass, fishing.mortality=NA, landings,labels=0, BMSY, FMSY, USR=0.8, LRP=0.4){
#// phase plot for fisheries reference points, biomass is the vector of B, landings if the vector of L, BMSY and FMSY are point estimates
	b <- biomass	
	l <- landings
	if(is.na(fishing.mortality)) fishing.mortality <- -log(1-l/b)
	f <- fishing.mortality
	
	bp <- b/BMSY
	fp <- f/FMSY
	n <- length(bp)
	plot(bp,fp,xlim=c(0,max(bp)*1.05),ylim=c(0,max(fp)*1.05),type='n',cex=1.5,xlab=expression(over(B,BMSY)),ylab=expression(over(F,FMSY)))
	#plot(bp,fp,xlim=c(0,4),ylim=c(0,2),type='n',cex=1.5,xlab=expression(over(B,BMSY)),ylab=expression(over(F,FMSY)))
	polygon(c(.8,10,10,.8),c(-5,-5,1,1),col="#00FF0080",lty=3) #bottom right
	polygon(c(.4,-1,-1,.4),c(-1,-1,10,10),col="#FF000080",lty=3) #top left
	polygon(c(.8,.4,.4,.8),c(1,1,-5,-5),col="#FFFF0080",lty=0) #bottom center
	#polygon(c(-1,.4,.4,-1),c(1,1,-5,-5),col="#FF000080",lty=3) #bottom left
	polygon(c(.4,10,10,.4),c(1,1,10,10),col="#FFFF0080",lty=0) #topright
	lines(bp,fp,col='black')
	if(length(labels)>1) {text(labels=labels,bp,fp,font=2)
	}else{
	points(pch=15,bp[1],fp[1],col='blue',cex=1.5)
	points(pch=16,bp[2:n-1],fp[2:n-1],cex=0.5)
	points(pch=17,bp[n],fp[n],col='red',cex=1.5)
	}
	}
	

