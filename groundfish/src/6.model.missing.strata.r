#model missing strata

loadfunctions(c('groundfish','utility','polygons'))

#this header stuff is if you are using ecomod, this will be handled by most of your sql queries
#be sure to have sets with zero info for the species of interest

#NEED TO DO THIS ON THE SET LEVEL AND PREDICT ON THE STRATA LEVEL, IMPROVED ESTIMATES NOT IMPLMENTED HERE YET

				ca  = groundfish.db('gscat')
				inf  = groundfish.db('gsinf')
				stra = groundfish.db(DS='gsstratum')  
				stra$NH = as.numeric(stra$area)/0.011801
                stra = stra[which(stra$strat %in% c(440:466,470:478,480:485,490:495)),c('strat','NH','depth')] #these are just the summer strata
			#get your data for the series of interest
				inf = inf[which(months(inf$sdate) %in% c('June','July','August')),]
				inf = inf[which(inf$strat %in% c(440:495)),]
			#get a data frame combining gsinf and gscat with variables of mission, setno, strat, bottom_temperature, depth, tow length, totno, totweight, species
				ca = ca[which(ca$spec==10),]
				ci = merge(inf,ca,by = 'id',all.x=T )

				vars.to.keep = c('id','sdate','spec','yr','strat','bottom_temperature','bottom_depth','totwgt','totno','dist_km')

				ci = ci[,vars.to.keep]

				ci$dist = ci$dist_km * 0.53997 #this is km to nautical miles

				ci$spec = 10
				ci[,c('totwgt','totno')] = na.zero(ci[,c('totwgt','totno')])
				ci$mission <- ci$setno <- NA
				ci[,c('mission','setno')] = do.call(rbind,(strsplit(ci$id,"\\.")))
				
				ci$totwgt = ci$totwgt*1.75/ci$dist
				ci$totno = ci$totno*1.75/ci$dist

					cis = aggregate(cbind(totno,totwgt,bottom_depth)~strat+yr,data=ci,FUN=mean)
					cisd = aggregate(cbind(totno,totwgt)~strat+yr,data=ci,FUN=sd)
					cit = aggregate(cbind(totno)~strat+yr,data=ci,FUN=length)
					names(cit)[3] = 'Nsets'
					names(cisd)[3:4] = c('totno.sd','totwgt.sd')
					cisd = na.zero(cisd)
					cis = merge(cis,cit, by=c('strat','yr'))
					cis = merge(cis,cisd,by=c('yr','strat'))

#get stratified total numbers for all years--assuming that all strata are sampled and more than 2 sets per strata
out = NULL
yi = unique(cis$yr)
for(y in yi) {
		u = cis[which(cis$yr==y),]
		u = merge(u,stra[,c('strat','NH')],by='strat')
		sNH = sum(u$NH)
		
	#stratified means
	xbarno = sum(u$totno*u$NH/sNH) 
	xbarwgt = sum(u$totwgt*u$NH / sNH)

	#stratified total
	
	xtotno = xbarno * sNH  
	xtotwgt = xbarwgt * sNH  

	
	out = rbind(out,c(yr=y,Ystn = xtotno, Ystw = xtotwgt))
}

out = as.data.frame(out) #hold on to this object for comparisons after dropping strata and modeling the predictions

plot(out$yr, out$Ystn, type='b', ylab='Total Abundance',xlab = 'Year',pch=16)

#cis is the data frame we will be modeling 
#it contains: yr, tow adjusted mean number and mean weight per strata, sd of mean number and mean wt,
#mean depth, strat, nsets in a strata 




		require(gamlss) #super flexible package to fit a wide range of distributions including mixture distributions like zero-adjusted gamma (ie the family zaga)
		yrs = 2011:2015 #trade off between few enough years to assume that the population abundance/distribution is approximately stable and large enough to get reasonable fits
		cc = cis[which(cis$yr %in% yrs),]
		a = gamlss(totno ~ strat + yr + bottom_depth, nu.formula = ~strat + yr + bottom_depth, data=cc, family='ZAGA')
		#zero adjusted inverse gaussian, reasonable AIC improvement removing bottom_depth
		aa = gamlss(totno ~ strat + yr , nu.formula = ~strat + yr , data=cc, family='ZAIG')
		
		#some validation plots
		plot(aa)
		summary(aa)
		b = predict(a,type='response',se.fit=T)



# using the set info
		require(gamlss) #super flexible package to fit a wide range of distributions including mixture distributions like zero-adjusted gamma (ie the family zaga)
		ci = na.zero(ci)
		
		yrs = 2011:2015 #trade off between few enough years to assume that the population abundance/distribution is approximately stable and large enough to get reasonable fits
 		cc = ci[which(ci$yr %in% yrs),]
		a = gamlss(totno ~ strat + yr + bottom_depth, nu.formula = ~strat + yr + bottom_depth, data=cc, family='ZAGA')
		#zero adjusted inverse gaussian, reasonable AIC improvement removing bottom_depth
		aa = gamlss(totno ~ strat + yr , nu.formula = ~strat + yr , data=cc, family='ZAIG')
		
		#some validation plots
		plot(aa)
		summary(aa)
		b = predict(a,type='response',se.fit=T)




##testing 

oo = list()
oa = list()
for(j in 1:5) {
	o = NULL
	a = NULL
	for(i in 1:5000){
		g = missing.strata.prediction(x=cis, yr2sample=2014, n2remove=j,stra = stra)
		o = rbind(o,g[[1]])	#stratified totals
		a = rbind(a,g[[2]]) #which strata were removed
		}
oo[[j]] <- o
oa[[j]] <- a
	}

#there is a problem with the second year something to do with the new data predict functino

for(i in 1:5) {

			pdf(file.path(project.figuredirectory('groundfish'),paste('sample.removed',i,2014,'pdf',sep=".")))
			hist(as.data.frame(oo[[i]])[,'Ystn1'],'fd',main=paste('Missing',i, "strat",sep=" "),xlab='Stratified Total Number',xlim=c(2.3E7,7.9E7))
			abline(v=out[which(out$yr==2014),'Ystn'],col='red')
			dev.off()
		}