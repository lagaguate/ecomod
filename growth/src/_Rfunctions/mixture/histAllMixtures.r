histAllMixtures <- function(mix.obj,x=NA,...) {
		if(is.na(x)) hist(mix.obj$x,...)
	
	for(i in 1:length(mix.obj$lambda)) {
			addMixtures(mix.obj,i,...)
	}

}
