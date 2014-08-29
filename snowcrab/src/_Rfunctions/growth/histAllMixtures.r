histAllMixtures <- function(mix.obj,x=NA,xlims=c(0,80)) {
		if(is.na(x)) lfHist(mix.obj$x,xlims=xlims)
	
	for(i in 1:length(mix.obj$lambda)) {
			addMixtures(mix.obj,i)
	}

}