modalBreakPointRegression <- function(x1,sd1,x2,sd2,bp.range=c(30,60),nsamples=10000) {
		#sample from distributions
				x 	<- numeric()
				y 	<- numeric()
				n   <- length(x1)
					for(i in 1:n) {
							s.1 <- rnorm(nsamples,x1[i],sd1[i])
							s.2	<- rnorm(nsamples,x2[i],sd2[i])
							x <- c(x,s.1)
							y <- c(y,s.2)
				        }
		#break point intervals        
				intervals=1000                                                   
				coef1	<-c()                                                        
				coef2	<-c()                                                        
				coef3	<-c()                                                        
				aic		<-c()                                                           
		                                                                 
		    f <- function (Cx)                                 
		        {  
		        browser()                                            
			        lhs <- function(x) ifelse(x < Cx,Cx-x,0)       
		    	    rhs <- function(x) ifelse(x < Cx,0,x-Cx)       
		        	fit <- lm(y ~ lhs(x) + rhs(x))                 
		        c(summary(fit)$r.squared,summary(fit)$coef[1], summary(fit)$coef[2], summary(fit)$coef[3])                      
		        }                                              
		                                                       
		    r2 <- function(x) -(f(x)[1])                       
		                                                       
		    res <- optimize(r2,interval=c(min(x),max(x)))      
		    res <- c(res$minimum,f(res$minimum))               
		         browser()                                              
		    best_Cx <- res[1]                                  
		    coef1 <- res[3]                                    
		    coef2 <- res[4]                                    
		    coef3 <- res[5]                                    
		    plot(x,y)                                          
		    abline(coef1+best_Cx*coef2,-coef2) #lhs            
		    abline(coef1-best_Cx*coef3,coef3)  #rs             
}                                                      