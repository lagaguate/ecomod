  figure.landings.timeseries = function( type="line", yearmax, outdir=NULL, outfile=NULL ) {
     
    landings = get.landingsdb( )  
    uyrs = sort(unique(landings$yr))
    uyrs = uyrs[ uyrs <= yearmax ]
    YR = data.frame( yr=uyrs )
    regions = c("cfanorth", "cfasouth", "cfa4x")
    l = e = c = NULL
    for (r in regions) {
      res = get.fishery.stats.by.region(landings ,r, YR)
      l = cbind( l, res$landings / 1000 )
    }
    rownames(l) = uyrs

    dir.create( outdir, recursive=T, showWarnings=F  )
    fn = file.path( outdir, paste(outfile,"png",sep="." ) )
  #   fn = file.path( outdir, outfile )
    Cairo( file=fn, type="png", bg="white", pointsize=30, units="in", width=6, height=4, dpi=300 )

    if (type=="bar") {
      cols = c("grey10", "grey40",  "grey80")
      reverse = c(3,2,1)
      formed.data = t( l[,c(3,2,1)] ) # re-order for plot
      formed.data[ is.na(formed.data) ] = 0
      barplot( formed.data, space=0, xlab="Year", ylab="Landings (t)", col=cols)
      legend(x=1, y=10000, c("N-ENS", "S-ENS", "4X"), fill=cols[reverse], bty="n")
    }
    if (type=="line") {
      pts = c(19, 22, 24)
      lns = c(1, 3, 1)
      cols = c("grey10", "grey10",  "grey20")
      yrange = range (l, na.rm=T)
      yrange[1] = 0
      xrange = range(uyrs)
      xrange[1] = xrange[1] - 0.5
      xrange[2] = xrange[2] + 0.5
      m=1; plot( uyrs, l[,m],  type="b", ylab="Landings (t)", xlab="Year", col=cols[m], lwd=4, lty=lns[m], pch=pts[m], axes=F, xlim=xrange, ylim=yrange)
      m=2; points(uyrs, l[,m], type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m])
      m=3; points(uyrs, l[,m], type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m])
      axis( 1 )
      axis( 2 )
      legend(x=1980, y=8500, c("N-ENS", "S-ENS", "4X"), bty="n", lty=lns, lwd=3, pch=pts, col=cols, cex=1.4 )
    }
    
    dev.off()
    cmd( "convert -trim -frame 10x10 -mattecolor white ", fn, fn )
    table.view( l )
   return( fn )
  }


