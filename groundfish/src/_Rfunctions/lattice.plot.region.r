
  lattice.plot.region = function(yy, outdir, layout=c(3,3), sdy=2.5, log=F, ...) {

    xlim = range(yy$yr) + c(-0.5, 0.5 )
    # ylim = c(0, mean(yy$mean, na.rm=T) + sdy * sd(yy$mean, na.rm=T) )
    ylim = range( yy$ub, yy$lb, na.rm=T )
#    if (log) ylim = mean(yy$mean, na.rm=T) + sdy * c(-1,1) * sd(yy$mean, na.rm=T)

    y = xyplot( mean~yr|region, data=yy, ub=yy$ub, lb=yy$lb, weight=yy$weight,
      layout=layout, 
      main="", xlab="Year", ylab=capwords(va),
      par.strip.text = list(cex = 0.01),
      strip = function(..., bg) strip.default(..., bg = "white"),
      xlim=xlim,
      ylim=ylim,
      scales = list( y=list(alternating=F, tick.number=4), x=list(tick.number=4), cex=1.4 ),
      panel = function(x, y, subscripts,  ub, lb, weight, ...) {
        panel.abline(v=1993.5, col="gray40", lty="dotted", lwd=1.2, ...)
        panel.abline(h=mean(y, na.rm=T), col="gray40", lwd=2, lty=3,...)
        panel.abline(h=globalmean, col="gray40", lty=1, lwd=1.2, ...)
        larrows(x, lb[subscripts], x, ub[subscripts], angle=90, code = 3, 
          length=0.02, col="gray20",lwd=1.4, ...)
        panel.xyplot(x, y, type="b",  lty="dotted", lwd=1.5, pch=20, col="gray20", ...)
        if (length(x) > 7) {
          panel.loess(x, y, weights=weight, span=1/4, degree=1.25, family="gaussian",
            evaluation=100, lwd=2.5, col="black", ...)
        }
      }
    )

    fname = file.path( outdir, paste( va, pe, "svg", sep=".") )
    
    print(fname)
    Cairo( file=fname, type="svg", bg="white", units="in", width=6, height=6, dpi=75 )
      print (y)
    dev.off()
  
  }

