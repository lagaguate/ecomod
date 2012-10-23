
  lattice.plot.number.year = function(yy, outdir, layout=c(3,3), xrange, yrange, xlabel, ...) {
    
    y = xyplot( mean~sizeclass|yr, data=yy,  ub=yy$ub, lb=yy$lb, weight=yy$weight,
      layout=layout, xlim=xrange, ylim=yrange,
      main="", xlab=xlabel, ylab="log10(number/km^2)",
      cex.lab=cex.lab, cex.axis=cex.axis, cex.main = cex.main, 
      par.strip.text = list(cex = 0.04),
      panel = function(x, y, subscripts, ub, lb, weight, ...) {
        panel.abline(h=globalmean, col="gray", lty=1, lwd=1.2, ...)
        larrows(x, lb[subscripts], x, ub[subscripts], angle = 90, code = 3, length=0.02, col="gray60", ...)
        panel.xyplot(x, y, type="p", lty=1, lwd=1.2, pch=20, col="gray20", ...)
        if (length(x) > 6) {
#          panel.loess(x, y, weights=weight, span=1/4, degree=1, lwd=2, col="orange", ...)
          panel.loess(x, y, span=0.3, degree=2, family="symmetric", lwd=2, col="orange", ...)
        }
      }
    )

    fname = file.path( outdir, paste( "No", pe, re, tx, "svg", sep=".") )
    print(fname)
    Cairo( file=fname, type="svg", bg="white", units="in", width=6, height=8, dpi=75 )
      print (y)
    dev.off()
  
  }


