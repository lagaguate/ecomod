 figure.timeseries.conditional = function( B, main="", xlab="", ylab="", span=0.4 ) {
 
    names(B) = c( "y", "x", "f" ) # value, value, factor
    nf = length( unique(B$f) )  # a factor

    ylim=range( B$y, na.rm=T); dy=(ylim[2]-ylim[1])/10; ylim=ylim+c(-dy, +dy)
    xlim=range( B$x, na.rm=T); dx=0.5; xlim=xlim+c(-dx, +dx)

    setup.lattice.options()
    pl = xyplot( y ~ x | f, data=B, 
      layout=c(1,nf), 
      par.strip.text = list(cex=3),
      par.settings=list(  
        axis.text=list(cex=3), 
        par.main.text = list(cex=3),
        layout.heights=list(strip=0.3, panel=1, main=0.3 ) 
      ),
      xlim=xlim, 
      # ylim=ylim,
      scales = list(y = "free"),
      # main=main,
      xlab=xlab, ylab=ylab,
      panel = function( x, y, ...) {
        panel.bwplot( x, y,  box.ratio=0.5 , horizontal=F, notch=F, pch="|", col="gray", lwd=3, ...)
        panel.xyplot( x, y, type=c("a"), col="black", lwd=4, ...)
        panel.xyplot( x, y, type=c("smooth", "a"), span=span, col="red", lty=2, lwd=2, ...)
      }
    )
     
      return(pl)
  }


