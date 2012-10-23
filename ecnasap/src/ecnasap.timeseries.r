
  source(file.path(ecnasapdir, "load.ecnasap.environment.r"))

  set = ecnasap.catches(source="file")
   

  # -------------------------
  # time series of data
  
  areas = c("nafo.2j3kl", "nafo.3no", "nafo.3p", "nafo.4rs", "nafo.4t", "nafo.4vw", 
            "nafo.4x", "nafo.5y", "nafo.5zew" )
  regions = c("2J3KL", "3NO", "3P", "4RS", "4T", "4VW", "4X", "5Y", "5Zew" )
#  variables = c( "allcaught", "grd", "pel", "shark", "pd_anom", "pred1", "pred2", "prey" )
  variables = c( "pred2", "prey" )
       
  for (v in variables) {
    td =  get.time.series (set, areas, v, outfile="ts.tmp.csv")
   
#    if (v=="pred2") {
#      # these are data extremes that seem very suspicious/aberrant
#      iremove1 = which( td$mean<1 | td$mean > 3 )
#      iremove2 = which( td$region=="nafo.4rs" &  td$year %in% c(1970, 1981) )
#      iremove = unique( c(iremove1, iremove2) )
#      td = td[ -iremove, ]
#    }
    
    td$ub = td$mean+(td$sd/sqrt(td$n-1))
    td$lb = td$mean-(td$sd/sqrt(td$n-1))
    
#    npanels = length(areas)
#    par(omd=c(0, 0, 0, 0)) # outer margins
#    par(mfcol=c(npanels,1))
#    par(mai=c(0, 0, 0, 0))

    ylim=range(c(td$ub, td$lb), na.rm=T)
    xlim=range(td$year, na.rm=T)
   
    for (reg in areas) {
      outfile = paste( v, "ts", reg, sep="." )
      u = td[(td$region==reg),]
      u = u[is.finite(u$mean) ,]
      if (dim(u)[1] > 1) {
        u = u[order(u$year),]
        errbar(u$year, u$mean, u$ub, u$lb, type="n", axes=F, xlim=xlim, ylim=ylim, 
               xlab=NULL, ylab=NULL ) 
        lines(u$year, u$mean, col="black", lwd=3 )
        points(u$year, u$mean, pch=10)
        axis( 1 )
# if (reg == areas[length(areas)]) axis( 1 )
        axis( 2 )
        text(xlim[2]-1, ylim[1] + (ylim[2]-ylim[1])*9/10, regions[which(areas==reg)], cex=1.2)
      }
    mtext("xxx", side=2, outer=T, line=1, cex=1.4)
    mtext("Year", side=1, outer=T, line=3, cex=1.4)
    Pr(dev="pdfpng", dname="ts", fname=outfile, width=8, height=6)
    }
  }
 
 
