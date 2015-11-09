#changing productivity simulator

	
	#delta K is the change in K between periods
	#delta R is the change in R between periods
	#corr is the change in Fmsy used to set exploitation in the second lower productivity period ie corr=2 lets exploitation to be at R/(2*2), decreasing below FMSY
	#change in R is much worse than change in K for reference points
	
prod.change<-function(R=1,k=100,delta.r=0.6,delta.k=1,corr=1){
	dat1<-sim.logistic.data(r=R,K=k,n0=50,sd.proc=1,catchs=T,tot.t=30,arp=T,expl=R/2,seed=NULL)
	dat2<-sim.logistic.data(r=R*delta.r,K=k*delta.k,n0=dat1[nrow(dat1),2],sd.proc=1,catchs=T,tot.t=30,arp=T,expl=R/(2*corr),seed=NULL)
	dat3<-rbind(dat1,dat2[-1,])
	lines(dat3[,2],type='l')
	return(dat3)
	}
	plot(1,1,xlim=c(0,60),ylim=c(0,60),type='n',xlab='Time',ylab='Biomass')
	prod.change()
	abline(v=30,lty=2,col='red')
	for(k in 1:1000) {
				a2<-prod.change(delta.r=0.6,delta.k=1)
				if(any(a2[,2]==0)) m=m+1
				}
	
	#prod.change()
	
	ab<-list()
	l=0
	delta.r.seq<-seq(1,0.25,by=-0.05)
	delta.k.seq<-seq(1,0.25,by=-0.05)
	for(i in 1:length(delta.r.seq)) {
		for(j in 1:length(delta.k.seq)){
				m=0
			for(k in 1:1000) {
				a2<-prod.change(delta.r=delta.r.seq[i],delta.k=delta.k.seq[j])
				if(any(a2[,2]==0)) m=m+1
				}
				l=l+1
				ab[[l]]<- c(delta.r.seq[i],delta.k.seq[j],m)
				}
				}

	abd<-as.data.frame(do.call('rbind',ab))
image(x=rev(delta.r.seq),y=rev(delta.k.seq),z=matrix(rev(abd[,3]/1000),length(delta.r.seq),length(delta.k.seq)),xlab='% Change in K',ylab='% Change in r')
contour(x=rev(delta.r.seq),y=rev(delta.k.seq),z=matrix(rev(abd[,3]/1000),length(delta.r.seq),length(delta.k.seq)),xlab='% Change in K',ylab='% Change in r',add=T)
title('Likelihood of Collapse')
