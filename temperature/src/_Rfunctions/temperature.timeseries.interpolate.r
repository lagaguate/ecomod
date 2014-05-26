

  temperature.timeseries.interpolate = function( ip=NULL, p, P, B ) {


    if (exists( "init.files", p)) loadfilelist( p$init.files ) 
    if (exists( "libs", p)) loadlibraries( p$libs ) 
    if (is.null(ip)) ip = 1:p$nruns


    debug.strangedata = FALSE
    if (debug.strangedata) {
      sg = which( P$plon < 520 & P$plon > 510 & P$plat> 5180 & P$plat < 5190 )
      mm = sg[1]
      hist(b$t)
    }

    # default output grid
    OP0 = expand.grid( weekno=p$wtimes, yr=p$tyears )
    OP0$fit = NA  # these will be filled in with predicted fits and se's
    OP0$se  = NA

    for ( iip in ip ) {
      mm = p$runs[iip,"loc"]
      Pi=P[mm,]
      print (mm)			
   
      OP = OP0

      for ( dm in p$dist.km ) { 
        
        drange = c(-1,1) * dm
        plon0 = Pi$plon + drange
        plat0 = Pi$plat + drange

        i = which( 
          B$plon > plon0[1] & 
          B$plon < plon0[2] & 
          B$plat > plat0[1] & 
          B$plat < plat0[2] 
        )

        if (length(i) > p$nMin.tbot ) {  
          # only attempt interpolation if we have enough data (nMin.tbot)
          b = B[i,] # faster to reduce the size of B here
          
          # remove potentially noisy/erroneous data --- they are highly influential when there is little data 
          #bt = quantile( b$t, probs=c(0.005, 0.995) )
          #bi =  which( b$t >= bt[1] & b$t <= bt[2] ) 
          #b = b[ bi ,  ] 
          b$w = 1 / (( Pi$plon - b$plon)**2 + (Pi$plat - b$plat)**2 )# weight data in space: inverse distance squared
          b$w[ which( b$w < 1e-3 ) ] = 1e-3
          OP = timeseries.impute( x=b[,c("t", "w", "yr", "weekno")], OP=OP, method=p$tsmethod, harmonics=p$tsharmonics, gam.optimizer=p$gam.optimizer )
          if ( any( is.finite ( OP$fit ) ) ) break()  # solution found
        }
      } # end for dm loop						
     
     
      # return original (observed) data back into the predictions .. even if no solutions
      ii = which( B$plon==Pi$plon & B$plat==Pi$plat )
      if ( length (ii) > 0 ) {
        b = B[ii,]
        b$yrwk = b$yr + b$weekno/52 
        bd = duplicated( b$yrwk )
        if (any( bd)) {
          for ( bi in which(bd) ) {
            bj = which( b$yrwk == b$yrwk[bi] )
            b$t[bj] = mean( b$t[bj] ) # update dups temps with mean
          }
        }
        b = b[ -which(bd), ]
        ii = which( b$plon==Pi$plon & b$plat==Pi$plat ) #update list
        b = b[ii, c("yr","weekno","t") ]
        OP = merge ( OP, b, by=c("yr", "weekno"), all.x=TRUE, all.y=FALSE, sort=TRUE )
        orig = which( is.finite( OP$t ) )
        if ( length( orig) > 0 ) {
          OP$fit[orig] = OP$t[orig]
          OP$se[orig] = 0  # fix to 0 as it is raw observation
        }
      }

      debug = FALSE
      if (debug) {
        x11()
        plot(  fit ~ I(yr+weekno/52), data=OP, pch=".", type="l")
        points( t ~I(weekno/52 + yr), data=b, pch="*", col="red")
      }
 
      tbot <- attach.big.matrix( p$tbot.desc )
      tbot[ mm,] <- OP$fit
      
      tbot.se <- attach.big.matrix( p$tbot.se.desc )
      tbot.se[mm,] <- OP$se
    
    } # end each point


    debug = FALSE
    if (debug) {
      AIC(model)
      summary(model)
      x11()
      
      x = b

      # comparison/debug of different imputation methods here:
      compare.imputation = FALSE
      if (compare.imputation) {
   
        oH1 = timeseries.impute( x=x, OP=OP, method="harmonics", harmonics=1, gam.optimizer=p$gam.optimizer, smoothdata=TRUE ) 
     
        oH1 = timeseries.impute( x=x, OP=OP, method="harmonics", harmonics=1, gam.optimizer=p$gam.optimizer ) 
        oH2 = timeseries.impute( x=x, OP=OP, method="harmonics", harmonics=2, gam.optimizer=p$gam.optimizer ) 
        oH3 = timeseries.impute( x=x, OP=OP, method="harmonics", harmonics=3, gam.optimizer=p$gam.optimizer ) 
        oS1 = timeseries.impute( x=x, OP=OP, method="simple", gam.optimizer=p$gam.optimizer ) 
        oS2 = timeseries.impute( x=x, OP=OP, method="seasonal.smoothed", gam.optimizer=p$gam.optimizer ) 
        
        oH1$time = oH1$yr + oH1$weekno/52
        oH2$time = oH2$yr + oH2$weekno/52
        oH3$time = oH3$yr + oH3$weekno/52
        oS1$time = oS1$yr + oS1$weekno/52
        oS2$time = oS1$yr + oS2$weekno/52
        x$time = x$yr + x$weekno/52 
        
        plot( t~time, x )
        lines( oH1$time, oH1$fit, col="black", lwd=2 )
        lines( oH2$time, oH2$fit, col="blue", lwd="3" )
        lines( oH3$time, oH3$fit, col="orange" )
        lines( oS1$time, oS1$fit, col="red")
        lines( oS2$time, oS2$fit, col="brown", lwd=2)
      }

    }


  }


