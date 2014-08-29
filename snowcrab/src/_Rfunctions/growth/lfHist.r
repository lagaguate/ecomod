lfHist <- function(x,xlims=c(0,150)) {
	hist(x,prob=T,breaks=seq(min(x)-3,max(x)+3,by=2),xlim=xlims)
	}