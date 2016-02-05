
movingMean<-function(x,w){
	
	# x is the vector
	# w is the vector of the number of data points and their relative weights w=c(1,2,1) is three year with the center year getting a higher weight
	
		t = c(x,rep(0,(length(w) - 1)))
		z = c(rep(1,length(x)),rep(0,(length(w) - 1)))
		tm = tcrossprod(w,t)
		zm = tcrossprod(w,z)
		for(i in 1:(length(w)-1)){
			tm[i+1,] =c(tm[i+1,(length(t)-i+1):length(t)],tm[i+1,1:(length(t)-i)])
			zm[i+1,] =c(zm[i+1,(length(t)-i+1):length(t)],zm[i+1,1:(length(t)-i)])
		}
		zt = colSums(tm)/colSums(zm)
		id = trunc(length(w)/2)
		return(zt[-c(1:id,(length(zt)-id+1):length(zt))])
}

