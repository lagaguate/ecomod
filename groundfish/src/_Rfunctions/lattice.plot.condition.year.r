
  lattice.plot.condition.year = function(yy, outdir, layout=c(3,3), xrange, yrange, xlabel, ...) {
    
    y = xyplot( mean~sizeclass|yr, data=yy, ub=yy$ub, lb=yy$lb, weight=yy$weight,
      layout=layout, xlim=xrange, ylim=yrange,
      main="", xlab=xlabel, ylab="Condition",
      par.strip.text = list(cex = 0.04),
      strip = function(..., bg) strip.default(..., bg = "white"),
      scales = list(y=list(alternating=F, tick.number=3), x=list(tick.number=5) ),
      aspect = 3/5,
      panel = function(x, y, subscripts,  ub, lb, weight, ...) {
        panel.abline(h=0, col="gray25", lty=1, lwd=1.5, ...)
        panel.abline(h=mean(y, na.rm=T), col="gray40", lwd=1.5, lty="dashed",...)
        larrows(x, lb[subscripts], x, ub[subscripts], angle = 90, code = 3, 
          length=0.03, col="gray20", lwd=1.5, ...)
        panel.xyplot(x, y, type="p", lty="dotted", lwd=1.5, pch=20, col="gray20", ...)
        if (length(x) > 7) {
          panel.loess(x, y, weights=weight, span=1/3, degree=1.5, family="gaussian",
            evaluation=100, lwd=2, col="black", ...)
        }
      }
    )
       
    fname = file.path( outdir, paste( va, pe, re, tx, "svg", sep=".") )
    print(fname)
    Cairo( file=fname, type="svg", bg="white", units="in", width=4, height=8, dpi=75 )
      print (y)
    dev.off()
 
  }


