
   lfHistMatrix <- function(x,bin=1,title='',logbin=F) {
     if(nrow(x)>50) {
   	  x <- x[order(x$yr),]
      years <- unique(x$yr)
      nrows = length(years)
      sexs <- c(0,1)
      ncols = length(sexs)
     par(mfcol=c(nrows,ncols),oma=c(6, 6, 4, 1),mar=c(0, 0, 0.4, 1.5)) # outer margins default:  c(0, 1, 0, 1)'c(bottom, left, top, right)'

      
      	x1 = 140
      	x2 = 90
    	cols = c("gray40" )
		x$rcw <- rCW(x$cw,bin=bin)
		if(logbin) {
				x$rcw <- log(x$rcw)
				x1= log(140)
				 x2 = log(90)
				 bin = 0.02
				 }
	for (a in 1:(ncols)) {
        set0 = x[x$sex==sexs[a],]
        l <- max(aggregate(lat~rcw+yr,data=set0,FUN='length')[,3])*0.6
        ylim=c(0,l)
    
      for (y in 1:nrows) {
          set1 = set0[ which(set0$yr==years[y] ), ]
          
      
         if(a==1) set1 <- set1[set1$rcw<x1,]
         if(a==2) set1 <- set1[set1$rcw<x2,]
          axes = "s"
          if(y<nrows) axes = "n"
         
		
          axisnames = F
          if (years[y]==years[nrows]) axisnames=T  # last row
		if(a==1)	hist(set1$rcw,xlim=c(0,x1),ylim=ylim,main='',xlab='',breaks=seq(0,x1+log(5),bin),xaxt=axes)
		if(a==2)	hist(set1$rcw,xlim=c(0,x2),ylim=ylim,main='',xlab='',breaks=seq(0,x2+log(5),bin),xaxt=axes)
         # if(a==1) barplot(toplot, space=0, axisnames=axisnames,  axes=axes, col=cols, xpd=F, lwd=3,xlim=xlim1,ylim=ylim)
         # if(a==2) barplot(toplot, space=0, axisnames=axisnames,  axes=axes, col=cols, xpd=F, lwd=3,xlim=xlim2,ylim=ylim)
          if (a==2) {text(80,l*0.8,years[y],cex=0.8)          }
        }
     }

     mtext("Carapace width (mm)", side=1, outer=T, line=4, cex=1.2)
     mtext("Count", side=2, outer=T, line=4, cex=1.2)
     mtext("Males", side=3, outer=T, line=0, at=0.25, cex=1.2)
     mtext("Females", side=3, outer=T, line=0, at=0.75, cex=1.2)
     mtext(title, side=3, outer=T, line=1, at=0.5, cex=1.2)
     }
     }