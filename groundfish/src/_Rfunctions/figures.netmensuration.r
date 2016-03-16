 figures.netmensuration = function( p, DS="", outdir = file.path( project.data.directory("groundfish"), "analysis" ) ) {

   if (DS %in% c("wing.v.door", "all") ) {
     gsinf = scanmar.db( DS="sweptarea",  p=p )  
     w2a = which( gsinf$geardesc == "Western IIA trawl" )     # for distribution checks for western IIA trawl
     fn = file.path( outdir, "wing.v.door.pdf" )
     pdf( file=fn )
      plot(door.mean~wing.mean, gsinf[ w2a,], type="n", xlab=" Wing spread (m)", ylab="Door spread (m)" ) 
      iold = which( gsinf$geardesc == "Western IIA trawl" & (gsinf$yr < 2011 | gsinf$yr==2012)  )
      points ( door.mean ~ wing.mean, gsinf[iold, ], col="blue", pch=20, cex=0.8 ) 
      imed = which( gsinf$geardesc == "Western IIA trawl" & gsinf$yr %in% c( 2011 ) )
      points ( door.mean ~ wing.mean, gsinf[imed, ], col="red", pch=20, cex=0.8 ) 
      inew = which( gsinf$geardesc == "Western IIA trawl" & gsinf$yr >= 2013 )
      points ( door.mean ~ wing.mean, gsinf[inew, ], col="green", pch=20, cex=0.8 )
      legend( "bottomright",  text.col=c("red", 
    dev.off()
      print( fn )
   }



 }


