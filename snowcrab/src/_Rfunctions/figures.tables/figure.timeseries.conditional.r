 figure.timeseries.conditional = function( B, main="", xlab="", ylab="", span=0.4 ) {
 
    names(B) = c( "y", "x", "f" ) # value, value, factor
    nf = length( unique(B$f) )  # a factor

    ylim=range( B$y, na.rm=T); dy=(ylim[2]-ylim[1])/10; ylim=ylim+c(-dy, +dy)
    xlim=range( B$x, na.rm=T); dx=1; xlim=xlim+c(-dx, +dx)

    xlabels = seq(min(xlim), max(xlim), 1)
   # xlabels = c(xlim[1], xlim[1]+4, xlim[1]+8, xlim[1]+12, xlim[1]+16, xlim[1]+18) 
    ylabels = round(seq(0, round(ylim[2], -2), length.out=6),-1)

    setup.lattice.options()
    pl = xyplot( y ~ x | f, data=B, 
      layout=c(1,nf), 
      par.strip.text = list(cex=1),
      par.settings=list(  
        axis.text=list(cex=0.75), 
        par.main.text = list(cex=1),
        layout.heights=list(strip=1, panel=1, main=0.3 ),
        strip.background=list(col="lightgrey"), 
        box.umbrella = list(col = "black"), 
        box.rectangle = list(col="black"), 
        box.dot = list(col = "black", pch = 3, cex=2),
        plot.symbol=list(col='black', fill='darkgrey', cex=0.75, pch=21)
      ),
      xlim=xlim, 
      # ylim=ylim,
      #scales = list(y =list(at=ylabels, labels=ylabels), x=list(at=xlabels, labels=xlabels))
      scales = list(y = "free", x=list(at=xlabels, labels=xlabels, rot=45)),
      main=main,
      xlab=xlab, ylab=ylab,
      panel = function( x, y, ...) {
        panel.bwplot( x, y,  box.ratio=0.5 , horizontal=F, notch=F, pch="|", col="black", lwd=1, ...)
        panel.xyplot( x, y, type=c("a"), col="black", lwd=4, ...)
        panel.xyplot( x, y, type=c("smooth", "a"), span=span, col="red", lty=2, lwd=1, ...)
      }
    )
     
      return(pl)
  }


 #pl = xyplot( mean~year|region, data=td, ub=td$ub, lb=td$lb,
       # layout=c(1,n.areas), 
       # par.strip.text = list(cex=1.0),
       # par.settings=list(  
       #   plot.symbol=list(col='black', fill='darkgrey', cex=0.75, pch=21),
        #  axis.text=list(cex=0.75), 
        #  par.main.text = list(cex=1),
        #  layout.heights=list(strip=1, panel=1, main=0.5 ), 
        #  strip.background=list(col="lightgrey")
        #),
        #ylim=ylim,
        #xlim = xlabels, 
        #ylim=(c(ylim[1], ylim[2])),
        #scales = list(y =list(at=ylabels, labels=ylabels), x=list(at=xlabels, labels=xlabels)),
        #main=v, xlab=list("Year", cex=1), ylab=list("Geometric mean No. / km^2", cex=1),
        #panel = function(x, y, subscripts, ub, lb, ...) {
          #larrows(x, lb[subscripts], x, ub[subscripts], angle = 90, code = 3, length=0.01,lwd=1)
         # panel.xyplot(x, y, type = "b", lty=1, lwd=1, col="black", ...)
          #panel.abline(h=0, col="gray75", lwd=1, ...)