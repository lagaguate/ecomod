meanBreakPointRegression <- function(x,y,sd.x,sd.y,nsamples=5000,weighted=T) {
		
	w <- 1/(sd.x^2+sd.y^2) #weighting factor
		
		#break point intervals        
				intervals=1000                                                   
				coef1	<-c()                                                        
				coef2	<-c()                                                        
				coef3	<-c()                                                        
				aic		<-c()                                                           
		                                                                 
		    f <- function (Cx)                                 
		        {  
		       
			        lhs <- function(x) ifelse(x < Cx,Cx-x,0)       
		    	    rhs <- function(x) ifelse(x < Cx,0,x-Cx)       
		        	if(!weighted) fit <- lm(y ~ lhs(x) + rhs(x))                 
		        	if(weighted) fit <- lm(y ~ lhs(x) + rhs(x),weights=w)    
		        	browser()             
		        c(summary(fit)$r.squared,summary(fit)$coef[1], summary(fit)$coef[2], summary(fit)$coef[3])                      
		        }                                              
		                                                       
		    r2 <- function(x) -(f(x)[1])                       
		                                                       
		    res <- optimize(r2,interval=c(min(x),max(x)))      
		    res <- c(res$minimum,f(res$minimum))               

		    best_Cx <- res[1]                                  
		    coef1 <- res[3]                                    
		    coef2 <- res[4]                                    
		    coef3 <- res[5]                                    
		    plot(x,y)                                          
		    abline(coef1+best_Cx*coef2,-coef2) #lhs            
		    abline(coef1-best_Cx*coef3,coef3)  #rs             
		    return(c(coef1,coef2,coef3,best_Cx))
}                                                      