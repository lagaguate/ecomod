meanSegmented <- function(x,y,sd.x,sd.y,weighted=T,breaks=50,title) {
		require(segmented)
	w <- 1/(sd.x^2+sd.y^2) #weighting factor
	if(weighted) a <- lm(y~x,weights=w)	
	if(!weighted) a <- lm(y~x)	
	
	gr <- segmented(a,seg.Z=~x,psi=list(x=breaks))
	print(summary(gr))
	plot(x,y,pch=1,cex=0.5,xlab=expression(bar(x)[t0]),ylab=expression(bar(x)[t1]),main=title)
	plot(gr,add=TRUE,lwd=2,col=1, lty=c(1,1),lwd=2)
	lines(gr,col=1,pch=19,bottom=FALSE,lwd=2)
	legend('bottomright',bty='n',c(paste('break point at ',round(summary(gr)$psi[2],1),'mm')))
	
}                                                      