
  timeseries.impute = function( x, OP, method="harmonics", harmonics=3 ) {
        
    OP$fit = NA
    OP$se  = NA

    if ( nrow(x) < 30 ) return(OP)  # data needs vary depending upon modelling approach
 
    if ( method=="gam" ) {
      require(mgcv)
        # base model -- seasonal smoothing model
          model = try( gam( t ~ s(weekno, bs="cc") + s(yr) + s(plon, plat) + s(plon) + s(plat) + s(z), data=x, optimizer="perf", weights=w ) )  
        # time-smoothed model 
        # if ( "try-error" %in% class(model) ) { 
          # model = try( gam( t ~ s(yr, weekno) +s(yr) +s(weekno) + s(plon, plat) + s(plon) + s(plat), data=x, optimizer="perf", weights=w ) )  
        # }
        if ( "try-error" %in% class(model) ) { 
          model = try( gam( t ~ s(yr) + s(weekno) + s(plon) + s(plat) + s(z), data=x, optimizer="perf" ) )  
        }
           # last resort .. simplest time smoothed model
        if ( "try-error" %in% class(model) ) { 
          model = try( gam( t ~ s(weekno, bs="cc") + s(yr) + s(z), data=x, optimizer="perf", weights=w ) )  
        }

        if ( ! "try-error" %in% class(model) ) { 
				  out = try( predict( model, newdata=OP, type="response", se.fit=T ) ) 
          OP$fit = out$fit
          OP$se = out$se
        }
    }
  

    if ( method=="seasonal.detrending.arma" ) {
      stop( "not yet working fully ... need to fix a few things ... see below")

      # not sure if this is worth it .. slow and hard to use with NA's  ... 
      # need to recode directly as a regression model
      # require(forecast)
      # xts = ts(x$t )
      # g = Arima( xts) ## but recoded as a regression --- fix me
      # predict (g, x) # , model=fit )
      # plot( forecast( f1, 20  ) )
      
      # estimate seasonal trend with regression and remove effect and then model annual trends with residuals
      
      md = gam( t ~ s(weekno), data=x, weights=w)
      x$t.seasonal = predict( md, x, se.fit=FALSE )
      x$t.detrended = x$t - x$t.seasonal
      x$t_1 = lag( x$t.detrended, -1 ) 
      x$t_2 = lag( x$t.detrended, -2 )
      x$t_3 = lag( x$t.detrended, -3 )

      md2 = gam( t.detrended ~ s(yr) + s(t_1) + s(t_2) + s(t_3) + s(plon,plat), data=x, weights=w )

      ### fix me --- seed OP with raw data to speed up convergence and also get a better estimate for OP$t[1,] than the mean
      OP$t.seasonal = predict( md, OP, se.fit=FALSE )
      OP$t.detrended = NA
      OP$t.detrended[1:10] = mean( x$t.detrended, na.rm=TRUE )
      OP$t_1 = lag( OP$t.detrended, -1 ) 
      OP$t_2 = lag( OP$t.detrended, -2 )
      OP$t_3 = lag( OP$t.detrended, -3 )

      icount = 0
      ii = which( !is.finite( OP$t.detrended ))
      while( length(ii)>0) {
        OP$t.detrended.predicted = predict( md2, OP, se.fit=FALSE )
        ii = which( !is.finite( x$t.detrended ))
        OP$t.detrended[ii] = OP$t.detrended.predicted[ii]
        # refresh lags
        OP$t_1 = lag( OP$t.detrended, -1 ) 
        OP$t_2 = lag( OP$t.detrended, -2 )
        OP$t_3 = lag( OP$t.detrended, -3 )
        icount = icount + 1
        if (icount > nrow(OP) ) break() # looks like no solution ... break
      }
      
      OP$t.predicted = OP$t.detrended + OP$t.seasonal
      jj = which(!is.finite( OP$t))
      if ( length(tt) > 0 ) OP$t[jj] = OP$t.predicted[jj]

      ### easier to do in one model: gam( t ~ s(weekno) + s(yr) ... ) 
      ### .. but the above is more simplistic and so more flexible wrt AR effect
      ### .. however, we could use gamm with ARMA random effect ... to consider one day ...
      
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
      n.additional.models = 3 # how many more than the 3 harmonic models in mf
      mf = c( 
        ' t ~ s(yr) + s(plon) +s(plat) + cos.w + sin.w' ,
        ' t ~ s(yr) + s(plon, plat)+ s(plon) +s(plat) + s(z) + cos.w + sin.w ' ,
        ' t ~ s(yr) + s(plon, plat)+ s(plon) +s(plat) + s(z) + cos.w + sin.w + cos.w2 + sin.w2 ' ,
        ' t ~ s(yr) + s(plon, plat)+ s(plon) +s(plat) + s(z) + s(cos.w, yr) + s(sin.w, yr) + s(cos.w) + s(sin.w)' ,
        ' t ~ s(yr) + s(plon, plat)+ s(plon) +s(plat) + s(z) + s(cos.w, yr) + s(sin.w, yr) + s(cos.w) + s(sin.w) + s(cos.w2, yr ) + s(sin.w2, yr ) + s(cos.w2) + s(sin.w2) ' ,
        ' t ~ s(yr) + s(plon, plat)+ s(plon) +s(plat) + s(z) + s(cos.w, yr) + s(sin.w, yr) + s(cos.w) + s(sin.w) + s(cos.w2, yr ) + s(sin.w2, yr ) + s(cos.w2) + s(sin.w2) + s(cos.w3, yr ) + s(sin.w3, yr ) + s(cos.w3) + s(sin.w3) '
      )
      x$tyr = 2*pi * x$weekno/52
      x$cos.w  = cos( x$tyr )
      x$sin.w  = sin( x$tyr )

      if (harmonics>1) {
        # compute additional harmonics only if required (to try to spped things up a bit)
        x$cos.w2 = cos( 2*x$tyr )
        x$sin.w2 = sin( 2*x$tyr )
      }

      if (harmonics>2) {
        x$cos.w3 = cos( 3*x$tyr )
        x$sin.w3 = sin( 3*x$tyr )
      }

      for ( h in (harmonics+1):1) {
        model = switch( p$gam.optimizer,
          bam = try( bam( formula( mf[h] ), data=x, weights=w ) ) ,
          bfgs = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","bfgs")  ) ) ,
          perf = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("perf")  ) ) ,
          newton = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","newton")  ) ) ,
          nlm = try( gam( formula( mf[h] ), data=x, weights=w, optimizer=c("outer","nlm")  ) ) 
        )
        if ( ! "try-error" %in% class(model) ) break() 
      }

      if ( ! "try-error" %in% class(model) ) { 
        OP$tyr = 2*pi * OP$weekno/52
        OP$cos.w  = cos( OP$tyr )
        OP$sin.w  = sin( OP$tyr )

        if (harmonics>1) {
          # compute additional harmonics only if required (to try to spped things up a bit)
          OP$cos.w2 = cos( 2*OP$tyr )
          OP$sin.w2 = sin( 2*OP$tyr )
        }

        if (harmonics>2) {
          OP$cos.w3 = cos( 3*OP$tyr )
          OP$sin.w3 = sin( 3*OP$tyr )
        }

        out = NULL
        out = try( predict( model, newdata=OP, type="response", se.fit=T ) ) 
        if ( ! "try-error" %in% class(out) ) {
          OP$fit = out$fit 
          OP$se = out$se  
        }
      }
    }
  
    debug = FALSE
    if (debug) {
      AIC(model)
      summary(model)
      x11()
      x$ty = x$weekno/52 
      OP$ty = OP$weekno / 52
      plot(  fit ~ I(yr+ty), data=OP, pch=".", type="l")
      points( t ~I(ty + yr), data=x, pch="*", col="red")
    }
   
    # constrain range of predicted data to the input data range
    
    TR =  quantile( x$t, probs=c(0.005, 0.995), na.rm=TRUE  )
    TR[1] = max( TR[1], -3)
    TR[2] = min( TR[2], 30)
    toolow = which( OP$fit < TR[1] )
    if ( length(toolow) > 0 )  OP$fit[toolow] = TR[1]
    toohigh = which( OP$fit > TR[2] )
    if ( length(toohigh) > 0 ) P[toohigh,ww] = TR[2]

    return ( OP ) 

  }



