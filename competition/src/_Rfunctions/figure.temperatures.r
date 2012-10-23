
figure.temperatures = function() {
 
	  pcoords = c("plon", "plat")
    out = NULL

    trange = c(-5, 20 )
    zrange = c( 50, 500 )
    tyears = c(1970:2007)
    regions = c("4vw", "4x")
    areas =  c("4VW", "4X")

    p=list()
    p$fisheries.grid.resolution = 2 
    p$pres = 1
    p$internal.projection = "utm20"
    p = spatial.parameters( type="SSE" )


    for (y in tyears ) {
      PS =   hydro.db( DS="bottom.gridded", yr=y, p=p )
 
      PS.to.remove.z = which( PS$depth < zrange[1] |  PS$depth > zrange[2] )
      PS.to.remove.t = which( PS$temperature < trange[1] |  PS$temperature > trange[2] )

      PS.to.remove = unique( c( PS.to.remove.z, PS.to.remove.t ) )
      rm( PS.to.remove.t, PS.to.remove.z ); gc()
      
      PS = PS[-PS.to.remove,]
      rm(PS.to.remove ); gc()

      totalsurfacearea = dim(PS)[1] * (p$pres*p$pres) 
      for (r in regions) {
         i = filter.region.polygon(x=PS[, pcoords], region=r, planar=T, proj.type=p$internal.projection)
         meantemp = NA
         surfacearea = length(i) * (p$pres*p$pres) 
         meantemp = sdtemp = NA
         if ( length(i) > 3) {
            meantemp = mean(PS$t[i], na.rm=T)
            sdtemp = sd(PS$t[i], na.rm=T)
         }
         res = NULL
         res = data.frame( cbind(yr=y, region=r, totalsurfacearea=totalsurfacearea, 
          surfacearea=surfacearea, meantemp=meantemp, sdtemp=sdtemp) )
         out = rbind( out, res )
      }
    }
    out = factor2number (out, c("yr", "totalsurfacearea", "surfacearea", "meantemp", "sdtemp") )
    out = factor2character (out, c("region") )
    
    require(lattice)

    td =out
       v = "meantemp"
    td$total = td[,v]
    td$region = factor( td$region, levels=regions, labels=areas)
    td$ubound = td$total +  td$sdtemp
    td$lbound = td$total -  td$sdtemp
    td = td[is.finite(td$total) ,]
    td = td[order(td$region,td$yr),]
    xlim=range(td$yr); xlim[1]=xlim[1]-0.5; xlim[2]=xlim[2]+0.5
    setup.lattice.options()
    pl = xyplot( total~yr|region, data=td, ub=td$ubound, lb=td$lbound,
        layout=c(1,2), xlim=xlim, ylim=c(-1,10), 
            main="Average summer bottom temperatures in depths 50 to 500 m", xlab="Year", ylab="Celcius",
            panel = function(x, y, subscripts, ub, lb, ...) {
            larrows(x, lb[subscripts],
                    x, ub[subscripts],
                   angle = 90, code = 3, length=0.05)
            panel.xyplot(x, y, type="b", lty="11", lwd=1, pch=19, col="gray50", ...)
            # panel.loess(x,y, span=0.15, lwd=2)
            panel.abline(h=mean(y, na.rm=T), col="gray40", lwd=1.5, lty="dashed",...)
            panel.abline(v=1992.5, col="gray40", lwd=1.5, lty=1,...)

#         panel.abline(h=globalmean, col="gray40", lty=1, lwd=1.5, ...)

       }
    )
    print( pl )
    Pr(dev="png", dname="R.gs" , fname= "bottom.temp.ts", width=8, height=10)
    means = tapply(td$total, td$region, mean, na.rm=T)
    print("mean temperatures:")
    print(means)
    print("SD:")
    print( tapply(td$total, td$region, sd, na.rm=T) ) 
    print( "latest year:" )
    print( td$total[ td$yr==p$current.assessment.year ])

  
	}



