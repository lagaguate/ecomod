ab.nls = function(ds,p){
  require(nlstools)
  require(splancs)
	    Qhigh<- 1-(p$alpha/2)
        Qlow<-(p$alpha/2)
       
        fit = nls(fwt~a*flen^b,ds,start=list(a=0.01,b=3.3))
        coef.fits<-coef(fit)
      r2 = 1-(deviance(fit)/sum((ds$fwt-mean(ds$fwt))^2)) #I know r2 is not really a proper measure of fit in nls...but a pseudoproxy sort of...AMC
        #bootstrapped confidence intervals
        boots<-nlsBooter(fit,ds,Qlow1=Qlow,Qhigh1=Qhigh)
        x<-seq(min(ds$flen),max(ds$flen),by=0.1)
        xa<-seq(min(ds$flen),max(ds$flen),by=1)
      
      #polygon choice
      pp1<-data.frame(x=c(x,rev(x)),y=c(boots$bootCI[1,2]*x^boots$bootCI[2,2],boots$bootCI[1,3]*rev(x)^boots$bootCI[2,3]))
      pp2<-data.frame(Length=xa,lower.wt=boots$bootCI[1,2]*xa^boots$bootCI[2,2],upper.wt=boots$bootCI[1,3]*(xa)^boots$bootCI[2,3])
      names(ds) = c('y','x','fsex','spec','year')
      regs<-inout(ds,pp1)
      outs<-length(regs[regs==F])
      abc<-c(sex = unique(ds$fsex),yr = unique(ds$year), species = unique(ds$spec), alpha=p$alpha,N.Measured=nrow(ds),N.Outside.of.Polygon=outs,a=coef.fits[1],b=coef.fits[2],a.lower=boots$bootCI[[3]],     	a.upper=boots$bootCI[[5]],b.lower=boots$bootCI[[4]],b.upper=boots$bootCI[[6]])
      return(abc)
}


