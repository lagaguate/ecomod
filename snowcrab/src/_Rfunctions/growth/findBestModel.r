findBestModel <- function(x,modes,aic.thresh,mode.thresh) {

	#from findMinAIC.r
				x$d <- x[,'aic']-min(x$aic)
				y 	<- x[x$d<=aic.thresh,]
		 		w 	<- y[y$nmodes==min(y$nmodes),]
		 		p 	<- modes[w[,3]]
		 		#remove runs with modes closer than threshold value
		 		b <- c()
		 		
		 		for(i in 1:length(p)) {
			 		h <- unique(abs(c(outer(p[[i]],p[[i]],"-"))))
			 		h <- h[h>0]
			 		b[i] <- all(h>mode.thresh)
				}
				if(length(which(b))==0) { 
					findBestModel(x=x,modes=modes,aic.thresh=aic.thresh,mode.thresh=(mode.thresh-1))
					}else {
				w <- w[b,]
		 		nk 	<- w[w$d==min(w$d),3]
		 		if(length(nk)>1) nk <- nk[1]
		 		return(nk)
}
}