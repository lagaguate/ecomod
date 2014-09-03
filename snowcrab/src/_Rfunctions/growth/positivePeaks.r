    
positivePeaks<-function(series,span=3){
	      z <- embed(series, span)
		  s <- span %/% 2
		  v<- max.col(z) == 1 + s
		  result <- c(rep(FALSE,s),v)
		  result <- result[1:(length(result)-s)]
		   return(result)
	} 
