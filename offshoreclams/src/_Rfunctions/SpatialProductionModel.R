SpatialProductionModel = function(SPdata,params){
  
  o = list()
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","SPMplots.pdf"))

  # loop through model fitting
  for (i in 1:length(SPdata)) {

    d = SPdata[[i]]
    res0 = optim( params, fn=biomass.logistic.recursion, O=d$O, C=d$C, errorType="lognormal" ) 
    res = res0$par
    o[[i]] = biomass.logistic( r=res["r"], K=res["K"], q=res["q"], B0=res["B0"], O=d$O, C=d$C,  yrs=d$yrs ) 
 

    ## a few useful plots
    par(mfrow=c(2,2),mar=c(3.1, 2.1, 2.1, 1.1))
    browser()
    # fit
    ymax = max( c( o$res$O, o$res$Op ), na.rm=T )
    plot( Op ~ yrs, o$res, type="b", ylim=c(0, ymax), pch=20 )  # predicted catch rate
    points( O ~ yrs, o$res, pch=22, col="red" )  # observed catch rate
    lines( O ~ yrs, o$res, lty="dashed", col="red" )  # observed catch rate
    abline ( v=d$yrs[ length(d$yrs)]+0.5, lty="dotted" )

    # catch
    plot( C ~ yrs, o$res, type="b" )
    abline ( v=d$yrs[ length(d$yrs)]+0.5, lty="dotted" )

    # biomass
    plot( B ~ yrs, o$res, type="b" ) 
    abline ( v=d$yrs[ length(d$yrs)]+0.5, lty="dotted" )

    # fishing mortality
    plot ( F ~ yrs, o$res, type="b" )
    abline ( v=d$yrs[ length(d$yrs)]+0.5, lty="dotted" )
    
    ## surplus production 
    #plot( P ~ yrs, o$res, type="b", pch=20 )
    #abline ( v=d$yrs[ length(d$yrs)]+0.5, lty="dotted" )
    #abline ( h=0, lty="dotted" )

    ## surplus production 
    #plot( P ~ B, o$res, type="p", pch=20 )
    #tmp = na.omit( o$res[, c("P","B")] )
    #tmp = tmp[ order( tmp$B ), ]
    #lines( predict( loess(  P ~ B, tmp, span=0.6 ) ) ~ B, tmp )
    #abline ( h=0, lty="dotted" )

    ## surplus production 
    #plot( P ~ C, o$res )
    #tmp = na.omit( o$res[, c("P","C")] )
    #tmp = tmp[ order( tmp$C ), ]
    #lines( predict( loess(  P ~ C, tmp, span=0.6 ) ) ~ C, tmp )
    #abline ( h=0, lty="dotted" )
  }
  
  dev.off()

  return(o)

}
