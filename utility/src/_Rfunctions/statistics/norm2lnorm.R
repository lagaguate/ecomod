norm.2.lnorm <-	function(x,v) {
	cv <- x/sqrt(v)
	l.v<-log(cv^2+1)
	l.x<-log(x)-log(l.v)/2
	
	return(c(mean=l.x,var=l.v)	)
	
	}