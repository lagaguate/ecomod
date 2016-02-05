identifyModes <- function(x,span=10) {
	#x comes from reshapeCW
	x <- rCW(x)

		#prune outliers
			ol<- quantile(x,c(0.005,0.995),na.rm=T)
	x[x<ol[1] | x>ol[2]] 	<- NA
	
	out <- list()
	
	for(i in 1:ncol(x)) {
		u 			<- as.data.frame(table(x[,i]))
		u 			<- factor2number(u,1)
		b 	<- u[positivePeaks(u[,2],span=span),]
		out[[i]] = b[which(b$Freq>3),'Var1']
			}
	return(out)	
 	}	
