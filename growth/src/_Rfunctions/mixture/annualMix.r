annualMix <- function(x,seqK=1:4,aic.threshold=10,mode.threshold=10) {
	#x is a matrix with columns representing the years
	ny <- ncol(x)
out <- list()
outg <- list()
best.model <- c()

	for(i in 1:ny) {
	
		out[[i]] <- nMix(x[,i],seqK=seqK,aic.threshold=aic.threshold,plot=F,mode.threshold=mode.threshold)
		outg[[i]] <- nMixgamma(x[,i],seqK=seqK,aic.threshold=aic.threshold,plot=F,mode.threshold=mode.threshold)
		best.model[i] <- which.min(c(aicc(lL=out[[i]]$loglik,k=length(out[[i]]$lambda)*3-1,n=length(out[[i]]$x)),aicc(lL=outg[[i]]$loglik,k=length(outg[[i]]$lambda)*3-1,n=length(outg[[i]]$x))))
		}
	
		oo <- list(out,outg,best.model)
		return(oo)
	}	