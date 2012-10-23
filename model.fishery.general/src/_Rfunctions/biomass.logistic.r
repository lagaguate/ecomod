
  
  biomass.logistic = function(r, K, q, B0, O, C, TAC=NULL, yrs=NULL, errorType="lognormal" ) {
    
    # estimate full solution set at a fixed set of paramaeters and O, biomass index, and C, catch
    # and TAC projections

    if (is.null(yrs)) yrs = 1:length(C)
    if (is.null(TAC)) TAC = rep( C[length(C)], 5 ) # default to a status quo TAC

    eps = 1e-4

    nx  = length(O) # no. data points in Time Series
    np = length(TAC )
    nt = np + nx

    # output data
    res = data.frame( yrs=0:(nt-1) )
    res$yrs = res$yrs + yrs[1]
    
    B = rep( 0, nx ) # model biomass
    B[1] =  B0/K * (1 + r * ( 1 - B0/K )) - C[1]/K  # initial conditions 
    for ( t in 2:nx)  {
      B[t] = max( eps, B[t-1] * ( 1 + r * ( 1 - B[t-1] )) - C[t-1]/K )
    }
    
    O = O/K   # observed index of abundance
    Op = q*B  # modelled index of abundance
   
    if (errorType == "normal" ) error = sum ( (O - Op)^2, na.rm=T )
    if (errorType == "lognormal" ) error = sum ( (log(O) - log(Op))^2, na.rm=T )

    # predictions using a given TAC scenario
    Bp =  rep( 0, np )
    Bp[1] = B[nx] * ( 1 + r * ( 1 - B[nx] )) - C[nx]/K
    for ( t in 2:np)  {
      Bp[t] = Bp[t-1] * ( 1 + r * ( 1 - Bp[t-1] )) - TAC[t-1]/K
    }
   
    # Fishing mortality estimate
    F = rep( 0, nx )
    F[1] <- -log( max ( eps, 1 - C[1]/K / ( B[1] + C[1]/K ) ) ) # a dummy value using approx catches
    for(i in 2:nx) {
      F[i] <- -log( max ( eps, 1 - C[i-1]/K / ( B[i] + C[i-1]/K ) ) )
    }
   
    # Fishing mortality with given TAC scenarios
    Fp = rep( 0, np )
    Fp[1] <- -log( max ( eps, 1 - C[nx]/K / ( Bp[1] + C[nx]/K ) ) ) 
    for(i in 2:np) {
      Fp[i] <- -log( max ( eps, 1 - TAC[i-1]/K / ( Bp[i] + TAC[i-1]/K ) ) )
    }

    MSY <- r * K / 4  # maximum height of of the latent productivity (yield)
    BMSY <- K/2  # biomass at MSY
    FMSY <- 2 * MSY / K # fishing mortality at MSY
    Fcrash <- 4 * MSY / K # fishing mortality at which the stock will crash
    EMSY = FMSY/q  # optimal effort
    OMSY = MSY/FMSY/q # optimal index of abundance or CPUE

    res$B = c( B, Bp ) * K  # return to original untis
    res$C = c( C, TAC )  
    res$F = c( F, Fp )
    res$Op = q*res$B  # modelled index of abundance
    res$O = NA
    res$O[1:nx] = O*K # return to original scale 
    
    # estimates of surplus production
    res$P = NA
    res$P[1] <- res$B[2]- res$B[1] + res$C[1]
    for (i in 2:(nx-1) ){
      res$P[i] <- (res$B[i+1]- res$B[i-1])/2 + res$C[i]  # linear interpolation
    }
    res$P[nx] <- res$B[nx]- res$B[nx-1] + res$C[nx]
    
    params = data.frame(cbind( r=r, K=K, B0=B0, q=q, error=error, errorType=errorType, msy=MSY, fmsy=FMSY, bmsy=BMSY, fcrash=Fcrash),
        row.names="biomass logistic" )
    
    out = list( res=res, params=params )
    return(out)

  }

