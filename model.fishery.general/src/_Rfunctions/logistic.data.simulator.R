#	source('C:/Documents and Settings/cooka/Desktop/Scripts/Surplus Production dec 2011/logistic data simulator.R')
#set up with 
	#process error sd.proc, 
	#observation error sd.obs, 
	#autocorrelation on process error arp 
	# the amount of autocorrelation phi
	#will return simulation with both process and observation if process=T otherwise observation error only
	#catchs =T  simulates catches based on an exploitation rate from running mean of previous 3 years numbers
	#expl = exploitation rate
	#expl.range = adding some deviation to catches from runif distribution (using the ranges plus or minus this range)
	
sim.logistic.data = function(seed=1001,r=0.5,K=200,n0=100,t0=1,tot.t=200,sd.proc=1,sd.obs=1,arp=F,phi=0.9,process=T,catchs=T,expl=0.05,expl.range=0.02) {
				  if (!is.null(seed)) set.seed(seed)
				 tvec = seq(1,tot.t)
				 n=length(tvec)
				 y = numeric(n)
				 ydet=numeric(n)
				 y[1] = n0
				 ydet[1] = n0
				 e.proc = rnorm(n,mean=0,sd=sd.proc)
				  
				  if(arp) {
				  #set up for autocorrelated process errors
				  #phi is the amount of correlation between points
				 aut<-function(n,cors,sds) {
						a<-numeric(n)
						a[1]<-rnorm(1,mean=0,sd=sds)
							for(i in 2:n) {
								 a[i]<-a[i-1]*cors+rnorm(1,mean=0,sd=sds)
								 }
								 a
							}
 					e.proc = aut(n,phi,sds=sd.proc)  
				  }
				   e.obs = rnorm(n,mean=0,sd=sd.obs)
				   
				  if(catchs==F) {
				 	    for (i in 2:n) {
					    ydet[i] = ydet[i-1]+r*ydet[i-1]*(1-ydet[i-1]/K)
						y[i] = y[i-1]+(r*y[i-1]*(1-y[i-1]/K)+e.proc[i-1]) ## process only
		  				}
							  y.procobs = y+e.obs
							  y.obs = ydet+e.obs
						  	  			if(any(y.obs<=0)) { a = which(y.obs<=0); y.obs[c(seq(a,n))] <-0}
							  			if(any(y.procobs<=0)) { a = which(y.procobs<=0); y.procobs[c(seq(a,n))] <-0}
							 		if(process) return(cbind(tvec,y.procobs))
							 		else return(cbind(tvec,y.obs))
		 }
		 
		 if(catchs) {
					cats = rep(expl, times=n)		 		  
					cats = sapply(cats,FUN=function(x) x+runif(1,-expl.range,expl.range))
				  y = numeric(n)
				  ydet = numeric(n)
				  ct = numeric(n)
				  ct.det = numeric(n)
				  y[1] = n0
				  ydet[1] = n0
				  ct[1] = cats[1] * n0
				  
				  for (i in 2:n) {
					    ydet[i] = ydet[i-1]+(r*ydet[i-1]*(1-ydet[i-1]/K))-ct[i-1]
						    y[i] = y[i-1]+(r*y[i-1]*(1-y[i-1]/K)+e.proc[i-1])-ct[i-1] ## process only
						    ct[i] = y[i-1] * cats[i]
						    }
		  y.procobs = y+e.obs
		  y.obs = ydet+e.obs
		   			if(any(y.obs<=0| is.na(y.obs))) { a = which(y.obs<=0 | is.na(y.obs)); y.obs[c(min(a):n)] <-0; ct[c((min(a):n))] <-0}
					if(any(y.procobs<=0)) { a = which(y.procobs<=0); y.procobs[c(min(a):n)] <-0; ct[c(min(a):n)] <-0}
					if(process) return(cbind(tvec,y.procobs,ct))
					else return(cbind(tvec,y.obs,ct))
		 }
	}



	
	
#set up a series of lists of initial values
		perturb.params<- function (base, alt, which, mult = FALSE, use.base = TRUE) 
		{ #from emdbook package
		    if (!missing(alt)) {
		        chlist = mapply(function(name, vals) {
		            x = lapply(vals, function(z2) {
		                if (mult) {
		                  base[[name]] = base[[name]] * z2
		                }
		                else base[[name]] = z2
		                base
		            })
		            if (is.list(x)) 
		                x
		            else list(x)
		        }, names(alt), alt)
		        chlist = unlist(chlist, recursive = FALSE)
		    }
		    if (use.base) {
		        chlist = append(list(base), chlist)
		    }
		    chlist
		}
