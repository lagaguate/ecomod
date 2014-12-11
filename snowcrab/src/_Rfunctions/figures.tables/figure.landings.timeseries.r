  figure.landings.timeseries = function( yearmax, outdir=NULL, outfile=NULL, type="line" ) {

    regions = c("cfanorth", "cfasouth", "cfa4x")
    l = NULL
    for (r in regions) {
      res = get.fishery.stats.by.region( Reg=r)
      l = cbind( l, res$landings  )
    }
    l = l / 1000

    l = as.data.frame( l )
    colnames(l) = regions
    rownames(l) = res$yr
   
    l = l[ which( as.numeric(rownames(l)) <= yearmax ), ] 
    uyrs = as.numeric(rownames(l) ) 

    dir.create( outdir, recursive=T, showWarnings=F  )
    fn = file.path( outdir, paste(outfile,"png",sep="." ) )
  #   fn = file.path( outdir, outfile )
  png( file=fn,units='in', width=15,height=12,pointsize=18, res=300,type='cairo')
 #pdf( file=fn, bg="white",)
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


