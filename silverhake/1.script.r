#silver hake update

loadfunctions('silverhake')
dat <- read.csv(file.path(project.directory('silverhake'),"data",'dat.csv'),header=T)
p <- list()
			dat <- dat[dat$Year>=1993,]
			
	#prepare data for bugs model
		input1 <-  list()
		input1$N <- nrow(dat)
		input1$I <- dat[,2] #survey index
		input1$nyrs=2 #projection
		
		
			
#landings 
land <- read.csv(file.path(project.directory('silverhake'),"data",'LANDINGS BY MONTH.csv'),header=T)
land$mm1 <- c(rep(0,6),rep(1:12,19),rep(0,4)) #July to Jun landings for a year
m <- 0
pp <- 0
land$y <- NA #survey year
land$fy <- NA #fishing year
	for(i in 1:nrow(land)) {
	if(land$mm1[i]==1) {m=m+1}
	if(land$MONTH[i]==4) {pp=pp+1}
	land$y[i] <- m	
	land$fy[i] <- pp	
	}

#landings from July 1993-June 1994 etc
	land1 <- land[land$mm1>0,]
	ll <- aggregate(land1$TOTAL,by=land1['y'],FUN=sum)[,2]/1000

#add in 2012 landings of 8057.337 t for assessment year july 2012-june 2013 # this is a simple hard coded number should really be added by month to the landings by month.csv from above
	p$ll <- ll <- c(ll,8.057337)

#landings July2013-Jan2014= 2965; if we assume only 5000t will be caught up to March 31 2014 and then the various catch
#5000 + prop prior to 2014 survey with prop =0.2879583 and remaining of landings for second run to march 2015
#scenarios for April2014-March 2015
#


#for projections
#assume landings up to March 2015==avg last three years

	ll2015<- mean(ll[(length(ll)-3):length(ll)])


#catch scenarios from verna 
	lvec <- c(ll2015,12,15,18)

#To get to june 2014
	lvecmar.june <- lvec*0.2879583

#July2013=June2014 assuming only 5 will be caught to march31

	lvec1 <- lvecmar.june+5

#July2014-March2015
	lvec2 <- lvec-lvecmar.june

	lann <- c(ll,lvec1[1],lvec2[1])

	input1$C <- lann

#Add in Priors for bugs model
			Ku<-max(input1$I)
			k <- find.moments(lo=1/Ku,up=1/(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K <<- find.moments(lo=Ku,up=(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
			input1$r.a<- 0.8406032000 ; input1$r.b <- 1/0.1290011479 
			input1$k.a<-k[[1]] ; input1$k.b <- k[[2]]
			input1$q.a<-0.05 ; input1$q.b <- 1
			input1$P0.a <- 0.0001; input1$P0.b <- 2		

		p$input1 <- input1
		
spBUGS3 <- function(input=input1,inits=inits,n = 5000, burn = 150, thin = 2, debug = F, wd=file.path(project.directory('silverhake'),'src')){
		require(R2WinBUGS)
			
	#	Initial values for WinBUGS one for each chain
		inits <- list(list(P0=.3,
		P=rep(0.75,times=input$N+2),
						r=0.8,
						k=0.003,
        				q=0.4
   						     					),
				list(P=rep(0.8,times=input$N+2),
						r=0.9,P0=.3,
						k=0.005,
        				q=0.4
        				),
     			list(P=rep(0.7,times=input$N+2),
						r=1.3,P0=.3,
						k=0.0005,
        				q=0.4
        				)	
     					)
			
		# Parameters to be returned 
		parameters <- c('sd.p','r','K','sd.o','q','B','Imean','P.res','P0')
					
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = file.path(wd,"sp3oneI.bug"), 
		n.chains = 3, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/WinBUGS14/", debug = debug)
		return(tmp)
	}

oneI<-spBUGS3(debug=T)

dput(oneI,file.path(project.directory('silverhake'), 'R','model with catch scenario 1.txt'))
#oneI <- dget('model with catch scenario 1.txt')
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#<><><><><><><><><>NOTES<><><><>><><><><><><><><><><><><>

#biomass , exploitation, reference point distributions, one year projection 15t (calendar year caught) 
#for rest of year use last couple of years, use landings scenarios for one year and relate to BMSY
#

##################################################
#Silver hake catch scenarios
##################################################


	p$lvec1 <- lvecmar.june+5

	#July2014-March2015
	p$lvec2 <- lvec-lvecmar.june

b2014<-list()
b2015<-list()
for(i in 1:4) {
a <- cs(i)
dput(a,file.path(project.directory('silverhake'), 'R',paste('catchscenario2yr',i,'.txt',sep='')))
}
save.image(file.path(project.directory('silverhake'), 'R',"catchscenarions2yr.dat")





