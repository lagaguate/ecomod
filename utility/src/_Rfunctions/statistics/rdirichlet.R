rdirichlet<-function(mean.props=c(.1,.1,.8),sd.prop=0.2,index=2,n=100) {
	#//mean props must add to 1
	#index indicates which proportion's sd is represented
	#sd prop is the single standard deviation from the index
	#n=number of iters
	
	meanprops <- mean.props
    sdans <-index
     #what is that standard deviation
   sourcesd <- sd.prop
 	w <- (meanprops[sdans] * (1 - meanprops[sdans])/sourcesd^2) -   1
	#alphapars is the shape parameter for dirichlet distribution for your prior
 	alphapars <- meanprops * w
	#from ben bolker r help 2000
	l<-length(alphapars)
    x<-matrix(rgamma(l*n,alphapars),ncol=l,byrow=TRUE)
    sm<-x%*%rep(1,l)
    x<-x/as.vector(sm)
    return(x)
    }
	



