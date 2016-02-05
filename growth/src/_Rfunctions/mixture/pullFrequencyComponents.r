pullFrequencyComponents <- function() {
		indir 	<- file.path("C:","~","ecomod","snowcrab","growth","R")
		h 		<- dir(indir) 	
		male	<- grep("-0",h)
		female	<- grep("-1",h)
		ou.m1 	<- list() ; m=0
		ou.m2 	<- list() 
		ou.f1 	<- list() ; f=0
		ou.f2	<- list()
		pid.m		<- numeric()
		pid.f		<- numeric()
		
		for(i in 1:length(h)) {
			load(file.path(indir,h[i]))	
			aa 	<- annualMeans(out,gaussian=T)
			if(i %in% male) {
					
					bb	<- identifyYearClasses(aa,male=T)
			if(length(bb)>0) {
				for(j in 1:length(bb)) {
					m	<- m+1	
						pid.m[m]		<- as.numeric(unlist(strsplit(unlist(strsplit(h[i],"PID")),"-"))[1])
					jk 	<- bb[[j]][1]
					#first freq
						in1 	<- out[[1]][jk][[1]]
						fg 		<- cbind(in1$x,in1$posterior[,bb[[j]][2]])
						ou.m1[[m]] 	<- fg[fg[,2]>0.025,1]
					#second freq
						in1 	<- out[[1]][jk+1][[1]]
						fg 		<- cbind(in1$x,in1$posterior[,bb[[j]][3]])
						ou.m2[[m]] 	<- fg[fg[,2]>0.025,1]
					}
				}
			}
			if(i %in% female) {
			bb	<- identifyYearClasses(aa,male=F)
			if(length(bb)>0) {
			for(j in 1:length(bb)) {
					f	<- f+1	
					pid.f[f] 		<- as.numeric(unlist(strsplit(unlist(strsplit(h[i],"PID")),"-"))[1])
					jk 	<- bb[[j]][1]
					#first freq
						in1 	<- out[[1]][jk][[1]]
						fg 		<- cbind(in1$x,in1$posterior[,bb[[j]][2]])
						ou.f1[[f]] 	<- fg[fg[,2]>0.025,1]
					#second freq
						in1 	<- out[[1]][jk+1][[1]]
						fg 		<- cbind(in1$x,in1$posterior[,bb[[j]][3]])
						ou.f2[[f]] 	<- fg[fg[,2]>0.025,1]
						}
					}
				}
			}
			return(list(ou.m1=ou.m1,ou.m2=ou.m2,ou.f1=ou.f1,ou.f2=ou.f2,pid.m=pid.m,pid.f=pid.f))
	}