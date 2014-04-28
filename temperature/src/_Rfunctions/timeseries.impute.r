
  timeseries.impute = function( x, OP, method="harmonics", harmonics=2, gam.optimizer="bam", smoothdata=FALSE, smoothing.kernel=kernel( "modified.daniell", c(2,1)) ) {
        
    OP$fit = NA
    OP$se  = NA
    
    if ( method=="simple" ) {
      # this method recovers the mean interannual trends nicely but misses the seasonality
      # model formulae .. first one is a basic one in case the others fail
      # order is important here .. most complex at bottom
 
      x$tiyr =  x$yr + x$weekno/52
      OP$tiyr = OP$yr + OP$weekno/52
 
      mf = c( 
        ' t ~ s(tiyr) + s(z)',
        ' t ~ s(tiyr) + s(plon) +s(plat) + s(z)',
        ' t ~ s(tiyr) + s(plon, plat) + s(plon) + s(plat) + s(z) '
      )
      nmods = length(mf)

      for ( h in nmods:1 ) {
        model = switch( gam.optimizer,
          bam = try( bam( formula( mf[h] ), data=x, weights=w ) ) ,
          bfgs = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","bfgs")  ) ) ,
          perf = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("perf")  ) ) ,
          newton = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","newton")  ) ) ,
          nlm = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","nlm")  ) ) 
        )
        if ( ! "try-error" %in% class(model) ) { 
          out = NULL
          out = try( predict( model, newdata=OP, type="response", se.fit=T ) ) 
          if ( ! "try-error" %in% class(out) ) {
            OP$fit = out$fit 
            OP$se = out$se  
          }
        }
      }
    }

   
    if ( method=="seasonal.smoothed" ) {
      # this method recovers the mean interannual trends nicely but misses the seasonality
      # model formulae .. first one is a basic one in case the others fail
      # order is important here .. most complex at bottom
 
      x$tiyr =  x$yr + x$weekno/52
      OP$tiyr = OP$yr + OP$weekno/52
 
      mf = c( 
        ' t ~ s(yr) + s(weekno, bs="cc") +  s(z) ',
        ' t ~ s(yr) + s(weekno, bs="cc") + s(plon) + s(plat) + s(z) ', 
        ' t ~ s(yr) + s(weekno, bs="cc") + s(plon, plat) + s(plon) + s(plat) + s(z) ',
        ' t ~ s(yr, weekno) + s(yr) + s(weekno, bs="cc") + s(plon, plat) + s(plon) + s(plat) + s(z) '  
      )
      nmods = length(mf)

      for ( h in nmods:1 ) {
        model = switch( gam.optimizer,
          bam = try( bam( formula( mf[h] ), data=x, weights=w ) ) ,
          bfgs = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","bfgs")  ) ) ,
          perf = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("perf")  ) ) ,
          newton = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","newton")  ) ) ,
          nlm = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","nlm")  ) ) 
        )
        if ( ! "try-error" %in% class(model) ) { 
          out = NULL
          out = try( predict( model, newdata=OP, type="response", se.fit=T ) ) 
          if ( ! "try-error" %in% class(out) ) {
            OP$fit = out$fit 
            OP$se = out$se  
          }
        }
      }
    }


  
    if ( method=="harmonics" ) {
      
      # old method use 1 harmonic ... forcing sinusoid as a seasonal component
	    # to add an offset to a trig function (b) must add cos to a sin function
      # y ~ a + c*sin(x+b)
      # y ~ a + c*sin(b)*cos(x) + c*cos(b)*sin(x)  
      #   .. as C*sin(x+b) = C*( cos(b) * sin(x) + sin(b) * cos(x) )
      # y ~ b0 + b1*x1 + b2*x2
      # where: 
      #   a = b0
      #   c^2 = b1^2 + b2^2 = c^2*(sin^2(b) + cos^2(b))
      #   c = sqrt(b1^2 + b2^2)
      #   b1/b2 = tan(b)  
      #   b = arctan(b1/b2)
      
      # model formulae .. first one is a basic one in case the others fail
      # order is important here .. last three must be harmonic 3, 2 and 1, then altenates in case they fail
      # at present these additional models are unsmoothed harmonics ..2 and 1 
      mf = c( 
' t ~ s(z, plon, plat)+ s(plon) +s(plat) + s(z) + s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w)' ,
' t ~ s(z, plon, plat)+ s(plon) +s(plat) + s(z) + s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w) + s(yr, cos.w2) + s( yr, sin.w2 )+ s(cos.w2) + s( sin.w2 ) ' ,
' t ~ s(z, plon, plat)+ s(plon) +s(plat) + s(z) + s(yr) + s(yr, cos.w) + s(yr, sin.w) + s(cos.w) + s(sin.w) + s(yr, cos.w2) + s( yr, sin.w2 )+ s(cos.w2) + s( sin.w2 ) + s(yr, cos.w3) + s( yr, sin.w3)  + s(cos.w3) + s(sin.w3) '
      ) 
      
      x$tyr = 2*pi *  x$weekno/52
      x$cos.w  = cos( x$tyr )
      x$sin.w  = sin( x$tyr )
      OP$tyr = 2*pi * OP$weekno/52 
      OP$cos.w  = cos( OP$tyr )
      OP$sin.w  = sin( OP$tyr )

      if (harmonics>1) {
        # compute additional harmonics only if required (to try to spped things up a bit)
        x$cos.w2 = cos( 2*x$tyr )
        x$sin.w2 = sin( 2*x$tyr )
        OP$cos.w2 = cos( 2*OP$tyr )
        OP$sin.w2 = sin( 2*OP$tyr )
      }

      if (harmonics>2) {
        x$cos.w3 = cos( 3*x$tyr )
        x$sin.w3 = sin( 3*x$tyr )
        OP$cos.w3 = cos( 3*OP$tyr )
        OP$sin.w3 = sin( 3*OP$tyr )
      }

      for ( h in harmonics:1) {
        model = switch( gam.optimizer,
          bam = try( bam( formula( mf[h] ), data=x, weights=w ) ) ,
          bfgs = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","bfgs")  ) ) ,
          perf = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("perf")  ) ) ,
          newton = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","newton")  ) ) ,
          nlm = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","nlm")  ) ) 
        )
        if ( ! "try-error" %in% class(model) ) break() 
      }
     
      if ( ! "try-error" %in% class(model) ) { 
        out = NULL
        out = try( predict( model, newdata=OP, type="response", se.fit=T ) ) 
        if ( ! "try-error" %in% class(out) ) {
          OP$fit = out$fit 
          OP$se = out$se  
        }
      } 
    }  # end harmonic method


    # final pass to constrain predictions to be smoother and within empirical range (99.9% quantiles)
    if( any( is.finite( OP$fit) ) ) {
      if ( smoothdata ) {
        # default is a 2X smoothing kernel: 2 adjacent values and then 1 adjacent on the smoothed series .. i.e. a high-pass filter
        nd = length(smoothing.kernel$coef) - 1 # data points will be trimmed from tails so need to recenter:
        nd0 = length(OP$fit) 
        si = (nd+1):(nd0-nd)
        # plot( fit~time, OP, type="l" )
        OP$fit[si] = kernapply( as.vector(OP$fit), smoothing.kernel ) 
        # lines( fit~time, OP, col="green" )
      }
      
      # constrain range of predicted data to the input data range
      TR =  quantile( x$t, probs=c(0.0005, 0.9995), na.rm=TRUE  )
      TR[1] = max( TR[1], -3)
      TR[2] = min( TR[2], 30)
      toolow = which( OP$fit < TR[1] )
      if ( length(toolow) > 0 )  OP$fit[toolow] = TR[1]
      toohigh = which( OP$fit > TR[2] )
      if ( length(toohigh) > 0 ) OP$fit[toohigh] = TR[2]
    }


    debug = FALSE
    if(debug) {
      OP$time = OP$yr + OP$weekno / 52
      OP = OP[ order( OP$time ) ,]
      
      # STL method
      OPts = ts( oH1$fit, start=c( min(p$tyears), 1), frequency=52 )  # testing oH1's fit
      plot.default(  OPts, type="l" )

      OPstl = stl( OPts, s.window=4) # seasonal decomposition using loess 
      plot(OPstl)

      # StructTS method
      OPstr  = StructTS( OPts, type = "BSM") ### much slower ... used only as a daignostic tool checking stability
      OPstrsm = tsSmooth( OPstr )
      plot( OPstrsm )
   
      spectrum( OPts )
      kn = kernel( "modified.daniell", c(2,1))

      opop = kernapply( as.vector(oH1$fit), kn)
      plot(oH1$fit, type="l")
      lines(opop, type="l", col="orange")

      ppp = kernapply( OPts, kn)
      plot.default(  OPts, type="l", col="green" )
      lines( ppp, col = "red")  

      ooo = spectrum( OPts, kn, plot=FALSE )
      plot( ooo, plot.type = "marginal") # the default type
      plot( ooo, plot.type = "coherency")
      plot( ooo, plot.type = "phase")


      require(forecast)
      ppp = Arima( OPts )


    }

    return ( OP ) 

  }



