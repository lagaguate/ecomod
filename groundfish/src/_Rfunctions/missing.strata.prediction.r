missing.strata.prediction <-	function(x, yr2sample, n2remove,stra = stra,t = out) {
								require(gamlss)
								#// x contains: yr, tow adjusted mean number and mean weight per strata, sd of mean number and mean wt, mean depth, strat, nsets in a strata 
									print('This is only setup for totno right now')
								#split out random strata
									y = x[which(x$yr %in% yr2sample),]
									sc = unique(y$strat)
									sc1 = sample(sc,size=length(sc)-n2remove) 
									scR = sc[-which(sc %in% sc1)]
									sc = sc1
									y = y[which(y$strat %in% sc),]
									w = x[-which(x$yr %in% yr2sample),]
									w = rbind(w,y) #data frame with missing strat
									ww = w[which(w$yr %in% c((yr2sample-4):yr2sample)),]
								
								#run the gam model
									a = gamlss(totno ~ strat + yr + bottom_depth, nu.formula = ~strat + yr + bottom_depth, data=ww[,c('strat','yr','bottom_depth','totno')], family='ZAGA')
									aa = gamlss(totno ~ strat + yr , nu.formula = ~strat + yr , data=ww[,c('strat','yr','totno')], family='ZAIG')
									
									nd = expand.grid(yr=yr2sample,strat=scR)
									nd = merge(nd,stra[,c('strat','depth')],by='strat') #need to use the bottom_depth from strata defns to fill in the gaps
									names(nd)[3] = 'bottom_depth'
									nd$bottom_depth = as.numeric(nd$bottom_depth)
									nd = data.frame(factor2character(nd,vars='strat'))
									nd1 = nd
								#predict on missing strata
									b = predict(a,newdata = nd, type='response',data=ww)
									bb = predict(aa,newdata = nd1, type='response',data=ww)
									nd$totno = b
									nd1$totno = bb
									
								#merge data back together
									nd = nd[which(nd$strat %in% c(scR) & nd$yr %in% c(yr2sample)),c('strat','yr','totno')]
									w1 = rbind(y[,c('strat','yr','totno')],nd)
									w1 = merge(w1,stra[,c('strat','NH')],by='strat')
									
									nd1 = nd1[which(nd1$strat %in% c(scR) & nd1$yr %in% c(yr2sample)),c('strat','yr','totno')]
									w2 = rbind(y[,c('strat','yr','totno')],nd1)
									w2 = merge(w2,stra[,c('strat','NH')],by='strat')
								

								#stratified totals
								xtotno1 = sum(w1$totno*w1$NH) 
								xtotno2 = sum(w2$totno*w2$NH) 
								
								return(list(c(yr=unique(w1$yr),Ystn1 = xtotno1, Ystn2 = xtotno2),scR))
										}

