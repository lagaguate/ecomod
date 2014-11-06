
  figure.timeseries.gam.potential.habitat.survey.biomass= function( outdir=file.path(project.directory('snowcrab'), "assessments","2013"), all.areas=T ) {
 
    set = snowcrab.db( DS="set.merge.det")
  
    if (all.areas) {
      areas = c("cfa4x", "cfasouth", "cfanorth" )
      regions = c("4X", "S-ENS", "N-ENS")
    } else {
      areas = c("cfasouth", "cfanorth" )
      regions = c("S-ENS", "N-ENS")
    }

    n.regions = length(regions)
    n.areas = length(areas)
    varnames = names(set)
#    vars = varnames[ grep ( vars, varnames) ]
fdir <-file.path( project.directory("snowcrab"), "R", "gam","habitat" )
        fs <- dir(fdir)
    fs <- fs[setdiff(grep('K.R0.mass',fs) , grep('environmentals.only',fs))]
        lo <- c()
        for(i in 1:length(fs)) {
          load(file.path(fdir,fs[i]))
        lo <- rbind(lo,K)
        rm(K)
        }
        
        areas=c("cfanorth", "cfasouth", "cfa4x")
        
        td = lo[ which( lo$region %in% areas & lo$yr>1995) , c('yr','region','sa.region') ]
        td1 =data.frame(yr=rep(2013,3),region=c('cfa4x','cfanorth','cfasouth'),sa.region=aggregate(sa.region~region,data=td[which(td$yr>2008),],FUN=mean)[2])
        
        td = rbind(td,td1)
        td$year =td$yr
        td$sa.region = td$sa.region/1000
        load(file.path(project.directory('snowcrab'),"R","ts.rdata"))
    ts <- ts[which(ts$variable==p$vars.to.model & ts$region %in% areas),c('year','region','mean','ub','lb')]
      td <- merge(ts,td, by=c('year','region'),all.x=T)
    
         td = td[ order(td$region, td$year) , ]
      td$region = factor(td$region, levels=areas, labels =regions)
      td[which(td$mean<0),c('mean','ub','lb')] <- NA
      td$mean <- td$mean*td$sa.region
td$lb <- td$lb*td$sa.region
td$ub <- td$ub*td$sa.region

      ylim=range(c(td$mean+td$ub), na.rm=T); ylim[1]=ylim[1]-0.1*ylim[2]; ylim[2] = ylim[2]+ylim[2]*0.2
      xlim=range(td$year); xlim[1]=xlim[1]-0.5; xlim[2]=xlim[2]+0.5

      dir.create( outdir, recursive=T, showWarnings=F )
      fn = file.path( outdir, paste( 'fishablebiomasssurveygam', "png", sep="." ) )
      Cairo( file=fn, type="png", bg="white",  units="in", width=5, height=6, dpi=300)
      setup.lattice.options()
      pl = xyplot( mean~year|region, data=td, ub=td$ub, lb=td$lb,
        layout=c(1,n.areas), 
        par.strip.text = list(cex=3.0),
        par.settings=list(  
          axis.text=list(cex=2.5), 
          par.main.text = list(cex=4),
          layout.heights=list(strip=0.2, panel=1, main=0.5 ) 
        ),
        xlim=xlim, scales = list(y = "free"),
        xlab=list("Year", cex=3), ylab=list("Fishable Biomass kt", cex=3),
        panel = function(x, y, subscripts, ub, lb, ...) {
          larrows(x, lb[subscripts], x, ub[subscripts], angle = 90, code = 3, length=0.05,lwd=3)
          panel.xyplot(x, y, type="b", lty=1, lwd=6, pch=20, col="black", ...)
          panel.abline(h=0, col="gray75", lwd=5, ...)
        }
      )
       
      print(pl)
      dev.off()
      cmd( "convert -trim -frame 10x10 -mattecolor white ", fn, fn )
    
    }



