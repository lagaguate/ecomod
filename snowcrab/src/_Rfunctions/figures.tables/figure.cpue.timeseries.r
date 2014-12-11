  figure.cpue.timeseries = function( yearmax, outdir=NULL, outfile=NULL ) {
   
    regions = c("cfanorth", "cfasouth", "cfa4x")
    k = NULL
    for (r in regions) {
      res = get.fishery.stats.by.region(Reg=r)
      k = cbind( k, res$cpue )
    }

    k = as.data.frame( k )
    colnames(k) = regions
    rownames(k) = res$yr
   
    k = k[ which( as.numeric(rownames(k)) <= yearmax ), ] 
    uyrs = as.numeric(rownames(k) ) 

    pts = c(19, 22, 24)
    lns = c(1, 3, 1)
    cols = c("grey10", "grey10",  "grey20")

    yrange = range (k, na.rm=T)
    yrange[1] = 0
    xrange = range(uyrs)
    xrange[1] = xrange[1] - 0.5
    xrange[2] = xrange[2] + 0.5
    
    dir.create( outdir, recursive=T, showWarnings=F  )
    fn = file.path( outdir, paste( outfile, "png", sep="." ) )
    #Cairo( file=fn, type="png", bg="white", , pointsize=30, units="in", width=6, height=4, dpi=300 )
     png( file=fn,units='in', width=15,height=12,pointsize=18, res=300,type='cairo')
       m=1; plot( uyrs, k[,m],  type="b", ylab="Catch rate (kg/trap)", xlab="Year", col=cols[m], lwd=3, lty=lns[m], pch=pts[m], axes=F, xlim=xrange, ylim=yrange)
      m=2; points(uyrs, k[,m], type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m])
      m=3; points(uyrs, k[,m], type="b", col=cols[m], lwd=3, lty=lns[m], pch=pts[m])
      axis( 1 )
      axis( 2 )
      legend(x=1980, y=100, c("N-ENS", "S-ENS", "4X"), bty="n", lty=lns, lwd=3, pch=pts, col=cols, cex=1.4 )
    dev.off()
    cmd( "convert -trim -frame 10x10 -mattecolor white ", fn, fn )
    table.view(k)
    return( fn )
  }


