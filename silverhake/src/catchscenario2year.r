#silver hake
#get the imputs from silver hake script final

lvec1 <- lvecmar.june+5

#July2014-March2015
lvec2 <- lvec-lvecmar.june





#change for landings scenarios
cs <- function (t1) {
lann <- c(ll,lvec1[t1],lvec2[t1])
input1$C <- lann
#Add in Priors
			Ku<-max(input1$I)
			k <- find.moments(lo=1/Ku,up=1/(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
			K <<- find.moments(lo=Ku,up=(Ku*6),l.perc=0.05,u.perc=0.95,dist='lnorm')
			input1$r.a<- 0.8406032000 ; input1$r.b <- 1/0.1290011479 
			input1$k.a<-k[[1]] ; input1$k.b <- k[[2]]
			input1$q.a<-0.05 ; input1$q.b <- 1
			input1$P0.a <- 0.0001; input1$P0.b <- 2		

		
		
spBUGS3 <- function(input=input1,inits=inits,n = 300000, burn = 50000, thin = 20, debug = F, wd="C:/SilverHakeUpdate/"){
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
        				)
     					)
			
		# Parameters to be returned 
		parameters <- c('sd.p','r','K','sd.o','q','B','Imean','P.res','P0')
					
		## Call to WinBUGS ##
		tmp <- bugs(data = input, inits, parameters.to.save = parameters, model.file = paste(wd,"sp3oneI.bug",sep=""), 
		n.chains = 2, n.iter = n, n.burnin = burn, n.thin = thin, bugs.directory = "C:/Program Files/WinBUGS14/", debug = debug)
		return(tmp)
	} 
oneI<-spBUGS3(debug=F)
}

b2014<-list()
b2015<-list()
for(i in 1:4) {
a <- cs(i)
b2014[[i]] <- a$sims.list$B[,22]
b2015[[i]]<- a$sims.list$B[,23]
dput(a,paste('catchscenario2yr',i,'.txt',sep=''))
}
save.image("C:/SilverHakeUpdate/catchscenario2yr.RData")


