SPmodel = function(params,sy=1986){
  
  BanSP = subset(Ban,Year>=sy)

  BanSP$Biomass = BanSP$Ban.CPUE * 1000 * 1000


  res0 = optim( params, fn=biomass.logistic.recursion, O=BanSP$Biomass, C=BanSP$Ban.Catch, errorType="lognormal" ) 


  # to obtain accessory estimates using solution parameters obtained above
  o = biomass.logistic( r=res["r"], K=res["K"], q=res["q"], B0=res["B0"],
    O=BanSP$Biomass, C=BanSP$Ban.Catch,  yrs=BanSP$Year ) 
  
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","SPplots.pdf"))
  # a few useful plots
  ymax = max( c( o$res$O, o$res$Op ), na.rm=T )
  plot( Op ~ yrs, o$res, type="b", ylim=c(0, ymax), pch=20 )  # predicted catch rate
  points( O ~ yrs, o$res, pch=22, col="red" )  # observed catch rate
  lines( O ~ yrs, o$res, lty="dashed", col="red" )  # observed catch rate
  abline ( v=BanSP$Year[ length(BanSP$Year)]+0.5, lty="dotted" )
  

  # catch
  plot( C ~ yrs, o$res, type="b" )
  abline ( v=BanSP$Year[ length(BanSP$Year)]+0.5, lty="dotted" )

  # biomass
  plot( B ~ yrs, o$res, type="b" ) 
  abline ( v=BanSP$Year[ length(BanSP$Year)]+0.5, lty="dotted" )


  # fishing mortality
  plot ( F ~ yrs, o$res, type="b" )
  abline ( v=BanSP$Year[ length(BanSP$Year)]+0.5, lty="dotted" )
  
  
  # surplus production 
  plot( P ~ yrs, o$res, type="b", pch=20 )
  abline ( v=BanSP$Year[ length(BanSP$Year)]+0.5, lty="dotted" )
  abline ( h=0, lty="dotted" )

  # surplus production 
  plot( P ~ B, o$res, type="p", pch=20 )
  tmp = na.omit( o$res[, c("P","B")] )
  tmp = tmp[ order( tmp$B ), ]
  lines( predict( loess(  P ~ B, tmp, span=0.6 ) ) ~ B, tmp )
  abline ( h=0, lty="dotted" )

  # surplus production 
  plot( P ~ C, o$res )
  tmp = na.omit( o$res[, c("P","C")] )
  tmp = tmp[ order( tmp$C ), ]
  lines( predict( loess(  P ~ C, tmp, span=0.6 ) ) ~ C, tmp )
  abline ( h=0, lty="dotted" )

dev.off()

}
