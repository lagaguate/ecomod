
  speciesarea.statistics = function( ip=NULL, p ) {
     
    if (exists( "init.files", p)) loadfilelist( p$init.files ) 
    if (exists( "libs", p)) loadlibraries( p$libs ) 
    if (is.null(ip)) ip = 1:p$nruns

    predict.param = data.frame( sa = pi * ( p$pred.radius ^ 2), nyrs=1 )   # km^2

    Y = speciesarea.db( DS="speciesarea.counts", p=p )
    nY = speciesarea.db( DS="speciesarea.counts.ny", p=p )
    o <- attach.big.matrix( p$bigmem.desc )
    
    for (ii in ip ) {
      print(ii)
      y =  t( Y[ii,,] ) 
      ny =  t( nY[ii,,] ) 

      rownames(y) = rownames(ny) = p$lengthscale
      colnames(y) = colnames(ny) = p$timescale
      
      y = as.data.frame.table( y, stringsAsFactors=F )
      colnames(y) = c( "lengthscale", "timescale", "nspec" )
      
      ny = as.data.frame.table( ny, stringsAsFactors=F )
      colnames(ny) = c( "lengthscale", "timescale", "nyrs" )

      y$lengthscale = as.numeric( y$lengthscale )
      y$timescale = as.numeric( y$timescale )
      y$sa =  pi * (y$lengthscale ^ 2)
      y$nyrs.ee =  y$timescale*2 + 1  # number of years of data entering into the count
      
      ny$lengthscale = as.numeric( ny$lengthscale )
      ny$timescale = as.numeric( ny$timescale )
      
      y = merge( y, ny, by=c("lengthscale", "timescale" ), all.x=T, all.y=F, sort=F ) 
      iiyy = which( !is.finite( y$nyrs ) )
      if (length (iiyy) > 0 ) y$nyrs[iiyy] = y$nyrs.ee[iiyy]
      
      X = na.omit(y) 
      X = X[ which(X$nspec>0), ]
      
      if ( nrow(X) < 5 ) next()

      if (p$speciesarea.method=="glm") {
        m0 = try ( glm( (nspec+1) ~ log(sa) + log(nyrs), data=X, family=gaussian("log") ) )
        if ("try-error" %in% class( m0)) next()
        m  = summary(m0)
        mod0 = coefficients(m)
        if (nrow(mod0) !=3 ) next()
        p0 = predict( m0, newdata=predict.param, se.fit=T, type="response" ) 
        out = c( mod0[1,1], mod0[2,1], mod0[3,1], mod0[1,2], mod0[2,2], mod0[3,2], summary.lm(m0)$r.squared, p0$fit-1, p0$se.fit )
      }
  
      o[ii,] =  out 
  
    }

    return ( NULL )
  }



