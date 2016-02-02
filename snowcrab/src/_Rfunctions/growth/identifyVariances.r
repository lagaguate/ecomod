identifyVariancesLambdas <- function(x,xi,span) {
		vars <- list()
		lambdas <- list()
		for(i in 1:ncol(x)) {
			
			w	<- xi[[i]]
			
			u	<- as.data.frame(table(rCW(x[,i])))
			u	<- factor2number(u,1)
		
			oo <- c()
			op <- c()
			
		for(j in 1:length(w)) {
			ii 	<- which(u[,1]==w[j])


			ji <- ii-span
			ij <- ii+span
			if(ji<0) ji<- 1
			if(ij>nrow(u)) ij <- nrow(u)
			v <- u[ji:ij,]
			v <- rep(v[,1],v[,2])
			oo[j] <- var(v)
			op[j] <- length(v) / sum(u$Freq)
		}
		vars[[i]] <- oo
		lambdas[[i]] <- op
	}
	return(list(vars,lambdas))
}