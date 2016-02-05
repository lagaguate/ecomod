findMinAIC <- function(x,thresh=2) {
		if(is.vector(x)) {
				N 	<- length(x)
				x 	<- data.frame(k=1:N,aic=x)
				x 	<- x[order(x$aic),]
				x$d <- c(x[1:N,'aic']-x[1,'aic'])
				u 	<- with(x[x$d<=thresh,],min(k))
			return(u)
		} else {
				x$d <- x[,'aic']-min(x$aic)
				y 	<- x[x$d<=thresh,]
		 		w 	<- y[y$nmodes==min(y$nmodes),]
		 		nk 	<- w[w$d==min(w$d),3]
		 		return(nk)
			}
		}
		