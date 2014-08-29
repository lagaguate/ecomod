addAllMixtures <- function(mix.obj) {
		hist(mix.obj$x,'FD')
	
	for(i in 1:length(mix.obj$mu)) {
			addMixtures(mix.obj,i)
	}

}