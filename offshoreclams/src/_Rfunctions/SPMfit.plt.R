SPMfit.plt <- function(model.out,yrs,name="", rows=3, CI=F,CV=F,H=1,Uadj=1,graphic='R',ymax,alpha=0.05,path='',ht=8,wd=6){

  # Fit plots
  
  
  if(graphic=='pdf')pdf(file.path(path,paste("fit", name, ".pdf", sep="")),ht,wd)
  if(graphic=="R")x11(ht,wd)
  
  if(missing(yrs))yrs = 1:model.out$data$NY
  NJ = model.out$data$NJ
  if(length(H)==1)H = rep(H,NJ)

  par(mfrow = c(rows,1), mar = c(0, 3, 0, 1), omi = c(0.5, 0.3, 0.3, 0.3))
  
  for(j in 1:NJ){
  
  
    Bposts = sweep(model.out$sims.list$P[,,j],2,FUN='*',model.out$sims.list$K[,j])
    Bmed = model.out$median$P[,j]*model.out$median$K[j]
    q = model.out$median$q/H[j]
    O = model.out$data$O[,j]/H[j]
  
    if(missing(ymax))yl2<-ifelse(CI,max(c(apply(Bposts, 2, quantile, 1-alpha/2)*q, O),na.rm=T),max(c(Bmed*q),O,na.rm=T))*1.1
    else yl2 = ymax
    #browser()
    plot(yrs, (Bmed[1:length(yrs)]*q), type = 'l', lwd = 2, ylim = c(0, yl2),   ylab = "", las = 1, xlim = c(min(yrs)-1, max(yrs)+1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -  0.3, asp = 'xy', cex.axis=1.2,xaxt='n')
    axis(1, lab = F, tcl = -0.3)
    axis(4, lab = F, tcl = -0.3)
    if(CI){
      lines(yrs, apply(sweep(Bposts,2,FUN='*',q), 2, quantile, alpha/2), lty =   2)
      lines(yrs, apply(sweep(Bposts,2,FUN='*',q), 2, quantile, 1-alpha/2), lty   = 2)
    }
    points(yrs, O, col = 'red', pch = 16,cex=1.2)
    if(CV)segments(yrs, O+O.cv*(O), yrs, O-O.cv*(O),col='red')
   
 
    if(j%in%(rows*(1:floor(NJ/rows)))){
      axis(1)
      mtext( expression(CPUE(t/km^2)), 2, 0,outer=T, cex = 1.25)
      if(graphic=="R"&&j != NJ){
      	x11(ht,wd)
        par(mfrow = c(rows,1), mar = c(0, 3, 0, 1), omi = c(0.5, 0.3, 0.3, 0.3))
      }

    }
    if(j == NJ){
      axis(1)
      mtext( expression(CPUE(t/km^2)), 2, 0,outer=T, cex = 1.25)
    }
  }
  if(graphic!="R")dev.off()
}


