mavg<-function(x,n=3){filter(x,rep(1/n,n),sides=2)}
