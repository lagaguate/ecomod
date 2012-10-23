
  loadfunctions( "shrimp, functionpattern="load.shrimp.environment.r" ) 

  require(mgcv)
  require(effects)
  require(car)

  s = shrimp.db( DS="shrimp.shrcomlog.redo" )
  s = shrimp.db( DS="shrimp.shrcomlog" )

  out = shrimp.fishery.stats( s, "all", suffix="all" )
  
  for (fleet in c("mobile", "fixed") ) {
    for (region in c("StMargaretsBay", "MahoneBay" ) ) {
      x = which(s$loc.small==region )
      tmp =  shrimp.fishery.stats( s[x,], fleet=fleet )
      if  (is.null (tmp) ) next()
      names(tmp) = paste( fleet, region, names(tmp), sep="." ) 
      out = merge( out, tmp, by.x="yr", by.y=paste(fleet,region,"yr",sep="."), all.x=T, all.y=F, sort=T )
    }
    for (region in sort(unique(s$sfa) )) {
      x = which(s$sfa==region )
      tmp =  shrimp.fishery.stats( s[x,], fleet=fleet )
      if  (is.null (tmp) ) next()
      names(tmp) = paste( fleet, region, names(tmp), sep="." ) 
      out = merge( out, tmp,  by.x="yr", by.y=paste(fleet,region,"yr",sep="."), all.x=T, all.y=F, sort=T )
    }
  }

  write.table( out, file="clipboard" ,sep="\t"  ) 



  # ---------------
  ### Catch analysis ... 

  # fixed gear fisheries stats
  sf = s[ which( 
	s$btype.simple=="fixed" 
    & is.finite( s$ntraps )
    & is.finite(s$fhours) 
    & s$ntraps > 0 
    & s$fhours > 0 
    & !grepl("NA", s$loc.small)  
  ) ,]
  sf$cpue = sf$weight / sf$ntraps
  sf$yr = as.factor(sf$yr)
  sf$loc.small = as.factor(sf$loc.small)

   
  # statistical model
  mf.glm0 = glm( cpue+1 ~ as.factor(yr) , data=sf )
  
  mf.glm0 = glm( cpue ~ yr*loc.small  , data=sf, family=Gamma()  )
  
  mf.glm1 = glm( cpue+1 ~ as.factor(yr) , data=sf, family=Gamma() )
  mf.glm2 = glm( cpue+1 ~ as.factor(yr) +  as.factor(month) , data=sf, family=Gamma() )
  mf.glm3 = glm( cpue+1 ~ as.factor(yr) +  as.factor(month) + fhours , data=sf, family=Gamma() )
  mf.glm4 = glm( cpue+1 ~ as.factor(yr) +  as.factor(month) + fhours + as.factor(loc.small), data=sf, family=Gamma() )
 	
  # (AIC, null/residual deviance) = (23900, 10000)
	
  mf0 = gam( cpue+1 ~ as.factor(yr) +  as.factor(month) + as(fhours) + as.factor(loc.small), data=sf, family=Gamma() )
  mf1 = gam( cpue+1 ~ as.factor(yr) +  s(month) + s(fhours) + as.factor(loc.small), data=sf, family=Gamma() )
  mf2 = gam( cpue+1 ~ s(yr) +  s(month) + s(fhours) + as.factor(loc.small), data=sf, family=Gamma() )
  mf3 = gam( cpue+1 ~ s(yr) +  s(month) + s(log10(fhours)) + as.factor(loc.small), data=sf, family=Gamma() )

  mf7 = lmer( cpue+1 ~ as.factor(yr) * as.factor(loc.small) +  as.factor(month) + as.factor(fhours), random=list(bcode=~-1) , data=sf, family=Gamma() )
  
  mf6 = gamm( weight ~  s(fhours) + as.factor(yr) +  as.factor(month) + as.factor(loc.small) , data=sf, offset=ntraps, family=Gamma() )

  plot( mf, trans=function(x) {x-1}, scale=0, seWithMean=T, all.terms=T, shade=T, rug=F )
 

  u = allEffects(mf.glm0)

  
 
 
  # --------------
  # mobile fisheries stats
  
  sm = s [ which( s$btype.simple=="mobile") ,]
  sm$cpue = sm$weight / sm$fhours
  mi = which( is.finite( sm$cpue) & (sm$fhours > 0)  )
   
  # statistical model
  mm = gam( log(cpue+1) ~ s(yr) +  s(month)  + s(log10(depth))+ as.factor(sfa) , data=sm[mi,] )
  mm = gam( log10(weight) ~ s(log10(fhours)) + s(yr) +  s(month) + as.factor(sfa) , data=sm[mi,] )
  mm = gamm( cpue ~ s(yr) +  s(month) + as.factor(sfa), data=sm[mi,], random=list(bcode=~-1) )

  
  #  plot( mm, trans=function(x) {exp(x)-1}, scale=0, seWithMean=T, all.terms=T, shade=T, rug=F )
  plot( mm$gam, scale=0, seWithMean=T, all.terms=T, shade=T, rug=F )
 
     
  
   

