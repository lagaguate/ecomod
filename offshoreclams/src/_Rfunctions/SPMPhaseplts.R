SPMPhaseplts = function(o,graphic="R",areas=1:5,nc=2,ht=9,wd=7,...){

	#phase plots
	o$median$B = sweep(o$median$P,MARGIN=2,o$median$K,"*")

	o$median$F = -log(1-o$data$C / o$median$B)

	loadfunctions('utility')
	if(graphic=='pdf')pdf(file.path( project.datadirectory("offshoreclams"),"figures",'PhasePlots.pdf'),height=ht,width=wd)
 	if(graphic=='R')x11(height=ht,width=wd)
	par(mfrow=c(ceiling(length(areas)/nc),nc), mar = c(0, 0, 0, 0), omi = c(0.5, 0.3, 0.3, 0.3))

	for(i in areas) {

		smB = sRefPoints('BMSY',r=o$median$r[i],K=o$median$K[i],err=o$median$sigma)
		smF = sRefPoints('FMSY',r=o$median$r[i],K=o$median$K[i],err=o$median$sigma)

		plot(1,1,type= 'n',xlab='',ylab='',xlim=c(0,4),ylim=c(0,3),...)
		plotArrows(X=o$median$B[,i] / smB, Y = o$median$F[,i] / smF,add=T)
		abline(h=1,lwd=3)
		abline(v=1,lwd=3)
		text(4,3,paste('Area',i,sep="-"))

	}
   	mtext( expression(over(B,B[MSY])), 1, 3,outer=T, cex = 1.25)
   	mtext( expression(over(F,F[MSY])), 2, 3,outer=T, cex = 1.25)

	if(graphic!='R')dev.off()
}

