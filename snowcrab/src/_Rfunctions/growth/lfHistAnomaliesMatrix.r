
   lfHistAnomaliesMatrix <- function(x,bin=1,running.mean=5) {
   	#similar to Pauly and David 1981 to identify modes
    
   	  x <- x[order(x$yr),]
      years <- unique(x$yr)
      nrows = length(years)
      sexs <- c(0,1)
      ncols = length(sexs)
     
     par(mfcol=c(nrows,ncols),oma=c(6, 6, 4, 1),mar=c(0, 0, 0.4, 1.5)) # outer margins default:  c(0, 1, 0, 1)'c(bottom, left, top, right)'

      
      	xlim1 = c(0,140)
      	xlim2 = c(0,90)
    	cols = c("gray40" )
		x$rcw <- rCW(x$cw,bin=bin)
		rmean <- rep(1,running.mean)
		
      
	for (a in 1:(ncols)) {
        set0 = x[x$sex==sexs[a],]
        l <- max(aggregate(lat~rcw+yr,data=set0,FUN='length')[,3])*0.6
        ylim=c(0,l)
        browser()
        ru <- aggregate(lat~rcw,data=set0,FUN='length')
	    
      for (y in 1:nrows) {
          set1 = set0[ which(set0$yr==years[y] ), ]
          
      
         if(a==1) set1 <- set1[set1$cw<140,]
         if(a==2) set1 <- set1[set1$cw<90,]
          axes = "s"
          if(y<nrows) axes = "n"
         
		
          axisnames = F
          if (years[y]==years[nrows]) axisnames=T  # last row
		if(a==1)	hist(set1$rcw,xlim=c(xlim1),ylim=ylim,main='',xlab='',breaks=seq(0,140,bin),xaxt=axes)
		if(a==2)	hist(set1$rcw,xlim=c(xlim2),ylim=ylim,main='',xlab='',breaks=seq(0,90,bin),xaxt=axes)
         # if(a==1) barplot(toplot, space=0, axisnames=axisnames,  axes=axes, col=cols, xpd=F, lwd=3,xlim=xlim1,ylim=ylim)
         # if(a==2) barplot(toplot, space=0, axisnames=axisnames,  axes=axes, col=cols, xpd=F, lwd=3,xlim=xlim2,ylim=ylim)
          if (a==2) {text(80,l*0.8,years[y],cex=0.8)          }
        }
     }

     mtext("Carapace width (mm)", side=1, outer=T, line=4, cex=1.2)
     mtext("Count", side=2, outer=T, line=4, cex=1.2)
     mtext("Males", side=3, outer=T, line=0, at=0.25, cex=1.2)
     mtext("Females", side=3, outer=T, line=0, at=0.75, cex=1.2)
     }