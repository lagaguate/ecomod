pullModalComponents <- function() {
		indir 	<- file.path(project.directory("snowcrab"),"growth","R")
		h 		<- dir(indir) 	
		male	<- grep("-0",h)
		female	<- grep("-1",h)
		ou.m1 	<- numeric() ; m=0
		ou.m2 	<- numeric() 
		ou.msd1	<- numeric()
		ou.msd2	<- numeric()
		ou.f1 	<- numeric() ; f=0
		ou.f2	<- numeric()
		ou.fsd1	<- numeric()
		ou.fsd2	<- numeric() 
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
					jk <- bb[[j]][1]
					#first dist
					ins <- grep('index',names(aa))
					hj 	<- ins[which(aa[jk,ins]==bb[[j]][2])]
					hj 	<- as.numeric(unlist(strsplit(names(aa[hj]),".",fixed=T))[2])
					hy1	<- paste('par1.',hj,sep='')
					hy2 <- paste('par2.',hj,sep='')
					ou.m1[m]	<-	aa[jk,which(names(aa)==hy1)]
					ou.msd1[m]	<-	aa[jk,which(names(aa)==hy2)]
	
					#second dist
					ins <- grep('index',names(aa))
					hj 	<- ins[which(aa[jk+1,ins]==bb[[j]][3])]
					hj 	<- as.numeric(unlist(strsplit(names(aa[hj]),".",fixed=T))[2])
					hy1	<- paste('par1.',hj,sep='')
					hy2 <- paste('par2.',hj,sep='')
					ou.m2[m] 	<-	aa[jk+1,which(names(aa)==hy1)]
					ou.msd2[m]	<-	aa[jk+1,which(names(aa)==hy2)]
					}
				}
			}
			if(i %in% female) {
			bb	<- identifyYearClasses(aa,male=F)
			if(length(bb)>0) {
			for(j in 1:length(bb)) {
					f	<- f+1	
					pid.f[f] 		<- as.numeric(unlist(strsplit(unlist(strsplit(h[i],"PID")),"-"))[1])
					jk <- bb[[j]][1]
					#first dist
					ins <- grep('index',names(aa))
					hj 	<- ins[which(aa[jk,ins]==bb[[j]][2])]
					hj 	<- as.numeric(unlist(strsplit(names(aa[hj]),".",fixed=T))[2])
					hy1	<- paste('par1.',hj,sep='')
					hy2 <- paste('par2.',hj,sep='')
					ou.f1[f]	<-	aa[jk,which(names(aa)==hy1)]
					ou.fsd1[f]	<-	aa[jk,which(names(aa)==hy2)]
					#second dist
					ins <- grep('index',names(aa))
					hj 	<- ins[which(aa[jk+1,ins]==bb[[j]][3])]
					hj 	<- as.numeric(unlist(strsplit(names(aa[hj]),".",fixed=T))[2])
					hy1	<- paste('par1.',hj,sep='')
					hy2 <- paste('par2.',hj,sep='')
					ou.f2[f] 	<-	aa[jk+1,which(names(aa)==hy1)]
					ou.fsd2[f]	<-	aa[jk+1,which(names(aa)==hy2)]
						}
					}
				}
			}
			return(list(ou.m1=ou.m1,ou.msd1=ou.msd1,ou.m2=ou.m2,ou.msd2=ou.msd2,ou.f1=ou.f1,ou.fsd1=ou.fsd1,ou.f2=ou.f2,ou.fsd2=ou.fsd2,pid.m=pid.m,pid.f=pid.f))
	}
