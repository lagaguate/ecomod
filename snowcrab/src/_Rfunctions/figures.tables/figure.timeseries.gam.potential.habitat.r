
  figure.timeseries.gam.potential.habitat= function( outdir=file.path(project.directory('snowcrab'), "assessments","2013"), all.areas=T ) {
 
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
        
       # areas=c("cfanorth", "cfasouth", "cfa4x")
        
        td = lo[ which( lo$region %in% areas ) , c('yr','region','sa.region') ]
        td1 =data.frame(yr=rep(2013,3),region=c('cfa4x','cfanorth','cfasouth'),sa.region=aggregate(sa.region~region,data=td[which(td$yr>2008),],FUN=mean)[2])
        
        td = rbind(td,td1)
        td$year =td$yr
        td$sa.region = td$sa.region/1000

         td = td[ order(td$region, td$year) , ]
      td$region = factor(td$region, levels=areas, labels =regions)

      ylim=range(c(td$sa.region), na.rm=T); ylim[1]=ylim[1]-0.1*ylim[2]; ylim[2] = ylim[2]+ylim[2]*0.2
      xlim=range(td$year); xlim[1]=xlim[1]-0.5; xlim[2]=xlim[2]+0.5

      dir.create( outdir, recursive=T, showWarnings=F )
      fn = file.path( outdir, paste( 'potential','habitat', "combined", "png", sep="." ) )
      Cairo( file=fn, type="png", bg="white",  units="in", width=5, height=6, dpi=300)
      setup.lattice.options()
      pl = xyplot( sa.region~year|region, data=td, 
        layout=c(1,n.areas), 
        par.strip.text = list(cex=3.0),
        par.settings=list(  
          axis.text=list(cex=2.5), 
          par.main.text = list(cex=4),
          layout.heights=list(strip=0.2, panel=1, main=0.5 ) 
        ),
        xlim=xlim, scales = list(y = "free"),
       xlab=list("Year", cex=3), ylab=list("Potential Snow Crab Habitat '000 km^2", cex=3),
        panel = function(x, y,  ...) {
          mx <- median(y)
                 panel.xyplot(x, y, type="b", lty=1, lwd=6, pch=20, col=c(rep("black",(length(y)-1)),'red'), ...)
          panel.abline(h=mx, col="gray75", lwd=5, ...)
        }
      )
       
      print(pl)
      dev.off()
      cmd( "convert -trim -frame 10x10 -mattecolor white ", fn, fn )
    
    }



