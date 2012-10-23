
##############################################3
#  oxygen
##############################################3

figure.oxygen = function() {

  biochem.dir = file.path( "/home", "jae", "projects", "biochem" )
  source( file.path( biochem.dir, "biochem.r" ) )
 
  to.extract = c( "lon", "lat", "sal", "temp", "oxysat", "Z", "yr", "julian", "region" )
  # biochem.db( DS="sse.bottom.redo" )

  ss = biochem.db( DS="sse.bottom" )
  ss$sal = ss$salinity
  ss$temp = ss$temperature 
  ss$oxysat = ss$oxygen_saturation
  ss = ss[ , to.extract ]

  sm = groundfish.db("sm.complete")
  sm$Z = sm$sdepth
  sm$region = sm$name
  sm = sm[ , to.extract ]

  sse = rbind( ss, sm )
  sse = sse[ which( sse$Z <500  & sse$Z > 50 ), ]
  sse = sse[ which( sse$region %in% c("4VW", "4X")), ]
  sse = sse[ which( sse$yr >= 1965 ), ]

  sse.4vw = which(sse$region=="4VW" )
  mean.4vw = mean(sse$oxysat [ sse.4vw], na.rm=T) 
  
  sse.4x = which(sse$region=="4X" )
  mean.4x = mean(sse$oxysat [ sse.4x], na.rm=T) 
 
  mean.4vwx = mean(sse$oxysat, na.rm=T) 


  require(mgcv)
  q = gam( oxysat ~ s(lon, lat) + s(Z) + s(yr) + s(julian) + s(temp) +s(sal), data=sse )
  summary(q)
  plot(q)
  
  q4vw = gam( oxysat ~ s(lon, lat) + s(Z) + s(yr) + s(julian) + s(temp) +s(sal), data=sse, subset=sse.4vw )
  summary(q4vw)
  plot(q4vw)
 
  q4x = gam( oxysat ~ s(lon, lat) + s(Z) + s(yr) + s(julian) + s(temp) +s(sal), data=sse, subset=sse.4x )
  summary(q4x)
  plot(q4x)
   
  fn = file.path( biochem.dir, "annual.4x.svg" )
  Cairo( file=fn,  type="svg",  dpi=100, width=4, height=3, units="in" ) 
    plot( q4x, rug=F, shade=T, select=3, residuals=F, seWithMean=T, shift=mean.4x, ylim=c(48, 102)-mean.4x )
  dev.off()
  
  fn = file.path( biochem.dir, "annual.4vw.svg" )
  Cairo( file=fn,  type="svg",  dpi=100, width=4, height=3, units="in" )
    plot( q4vw, rug=F, shade=T, select=3, residuals=F, seWithMean=T, shift=mean.4x, ylim=c(48, 102)-mean.4vw  )
  dev.off()
  
  fn = file.path( biochem.dir, "annual.all.svg" )
  Cairo( file=fn,  type="svg",  dpi=100, width=4, height=3, units="in" )
    plot( q, rug=F, shade=T, select=3, residuals=F, seWithMean=T, shift=mean.4vwx, ylim=c(48, 102)-mean.4vwx  )
  dev.off()

}


