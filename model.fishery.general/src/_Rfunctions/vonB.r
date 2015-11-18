#running the function
#if you have an RODBC conncection open then call your connection channel and set ODBC=T other wise read your full data frame 
#with all observations (not mean at age) with column headers of FLEN, AGE 
#if doing the cohort vonB analysis otherwise you need to specify the year as well
#you can specifiy the initial parameter estimates (init.pars )  or let the function calculate them using walford analysis
#if you use means you can specify a weighting -- error.kimura.c--- which is the inverse variance of the length at age...seems most appropriate to me
# you can do a cohort analysis as well just something I have been playing with

#if you have an odbc connection
#LVB(species=11,syear=1985,eyear=2014,area=470:481,season='Summer',means=T,error.kimura.c=T)

#or if you have a data frame named gg and no odbc connection 
#LVB(ODBC=F,dat=gg,means=T,error.kimura.c=T)
vonB <- function(ODBC = T, dat=NULL, species=11,area='4X',syear=1985,eyear=2014,season='Summer', plot=T, init.pars=list(hinf=0, K=0, t0=0), cohort=F, 
	control = list(maxiter = 10000, minFactor = 1/2024, tol = 1e-05), means=F, error.kimura.c=T, conditional.bootstrap) {
if(ODBC) {

				mns<-ifelse(season=='Winter',paste('12','01','02','03',sep="','"),ifelse(season=='Spring',paste('04','05',sep="','"),ifelse(season=='Summer',paste('06','07','08',sep="','"),
				ifelse(season=='Autumn',paste('09','10','11','12',sep="','"),9999))))
				area<-paste(area,collapse="','")
	                
                if(!cohort) {
                   dat<-sqlQuery(channel,paste("select flen,age from groundfish.gsdet d, groundfish.gsinf i where i.mission=d.mission and i.setno=d.setno and 
                   					to_char(sdate,'yyyy') between ",syear," and ",eyear," and to_char(sdate,'mm') in ('",mns,"') and strat in (select distinct strat from mflib.gsmgt where unit in ('",area,"')) and spec in ",species," and age is not null;",sep=""))
                   }
                if(cohort) {
                   dat<-sqlQuery(channel,paste("select to_char(sdate,'yyyy') year, flen,age from groundfish.gsdet d, groundfish.gsinf i 
                   					where i.mission=d.mission and i.setno=d.setno and strat in (select distinct strat from mflib.gsmgt where unit in ('",area,"'))  and to_char(sdate,'mm') in ('",mns,"')
                   					and spec in ",species," and age is not null;",sep=""))
                   }
               }
               if(cohort) {
   					yrs<-seq(syear, as.numeric(format(Sys.Date(),'%Y')),by=1)
                   					ages<-seq(1:length(yrs))
                   					iids<-paste(yrs,ages,sep="-")
                   					dat$iids<-paste(dat$YEAR,dat$AGE,sep="-")
                   					dat<-dat[dat$iids %in% iids,]
                }

                   dat<-na.omit(dat)
                   if(nrow(dat)<15) {stop('Not enough data points to fit')}

    parameters<-c()
    dat$wgts=1
    if(means) {
    	#use only mean at length for model
    	a1<-aggregate(dat$FLEN,by=list(dat$AGE),FUN=mean)
    	a2<-aggregate(dat$FLEN,by=list(dat$AGE),FUN=var)
    	a3<-aggregate(dat$FLEN,by=list(dat$AGE),FUN=length)
    	ad1<-merge(a1,a2,by='Group.1')
    	dat<-merge(ad1,a3,by='Group.1')
    	dat[is.na(dat)]<-1
    	names(dat)<-c('AGE','FLEN','VAR','N')
    	dat$wgts<-1
    	if(error.kimura.c) dat$wgts<-dat$N/dat$VAR
      }
	A<-sort(unique(dat$AGE))
	# LVB model
			
	 	# initial parameters
	plot(dat$AGE,dat$FLEN,xlab='Age',ylab='Fish Length')
 		a <- sort(unique(dat$AGE))
	 	mh <- c(); mi<-c()
	 	for(j in 1:length(a)){mh[j] <- mean(dat$FLEN[dat$AGE==a[j]],na.rm=T)}
	 	for(i in 2:(length(mh))) {mi[i]<-mh[i]-mh[i-1]}
	 	if(any(na.omit(mi<0))) {
	 		aa<-which(mi<0)
	 		for(k in 1:length(aa)) {
	 			if(aa[k]==length(mi)) {
	 					mh<-mh[-length(mh)]
	 					a<-a[-length(a)]
	 					}else{
	 		mh[aa[k]]<-mean(c(mh[aa[k]-1],mh[aa[k]+1]))
	 				}
	 			}
	 		}
	 	walford <- lm(mh[2:length(mh)]~mh[1:(length(mh)-1)])
	 	hinf <- walford$coefficients[1]/(1-walford$coefficients[2])
	 	K <- (walford$coefficients[2])
	 	t0 <- 0
	if(init.pars$hinf==0) init.pars <- list(hinf=hinf, K=K, t0=t0)
	init.pars$K<-ifelse(init.pars$K>.4,0.1,init.pars$K)
	init.pars$hinf<-ifelse(init.pars$hinf>300,300,init.pars$hinf)
		

		AGE<-dat$AGE
		FLEN<-dat$FLEN

		lvbf <- nls(FLEN~hinf*(1-exp(-K*(AGE-t0))), data = dat,weights=wgts,start = init.pars,control=control)
		lvb.fit = summary(lvbf)
		parameters1<-as.data.frame(lvb.fit$parameters)

if(conditional.bootstrap) {
	bs.par = NULL
			pp = predict(lvbf)
			rr = residuals(lvbf)
			for(i in 1:1000) {
				AGE =  dat$AGE
				FLEN = pp + sample(rr,length(rr))
			ai = try(nls(FLEN~hinf*(1-exp(-K*(AGE-t0))),start = init.pars,control=control))
 			print(ai)  
			if(!class(ai) %in% 'try-error') {          
			bs.par <- rbind(bs.par,t(summary(ai)$parameters[,1]))
			}
			}
		parameters = list(parameters1, bs.par)
		
}

	xlims=c(0,max(dat$AGE))
    ylims=c(0,max(dat$FLEN))
    plot(dat$AGE,dat$FLEN,ylab='Fish Length',xlab='Age',col='blue',pch=16,cex=1,xlim=xlims,ylim=ylims)
	ht <- parameters1[1,1]*(1-exp(-parameters1[2,1]*(A-parameters1[3,1])))     # von Bertalanffy equation #
	lines(A,ht,lwd=2,col='blue')
	if(conditional.bootstrap) {
			for(i in 1:nrow(bs.par)){
			ht <- bs.par[i,1]*(1-exp(-bs.par[i,2]*(A-bs.par[i,3])))     # von Bertalanffy equation #
			lines(A,ht,lwd=0.5,col='grey40')
				}
			}

		return(parameters)
			}	
	
				






