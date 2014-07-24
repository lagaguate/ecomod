
  # AKA, "biomass dynamics model" or "Schaeffer" model 
  # example using Georges Bank scallop data: Jonsen et al 2009 (CSAS 2009/034)
  
  loadfunctions( "model.fishery.general" )


  params = c(r=1, K=24000, q=0.1 , B0=7000 )
  scallop.example =  data.frame(
      yrs = 1981:2007,
      CPUE = c( 9.19, 6.52, 4.41, 3.22, 4.27, 11.37, 10.15, 5.78, 6.92, 8.19,
          9.43, 9.06, 10.62, 8.28, 5.51, 10.21, 12.92, 7.98, 10.45, 36.53, 
          21.15, 36.59, 14.75, 10.14, 9.24, 11.97, 26.69 ),
      C = c( 4149.54, 2766.28, 2702.56, 3741.11, 4876.54, 6759.31, 4227.42, 
          4632.01, 4948.75, 5737.49, 5902.89, 5988.35, 4363.53, 1894.13,
          2990.58, 4203.05, 3194.12, 2498.75, 6180.7, 6469.61, 6456.64, 5984.63,
          3523.94, 2481.88, 3931.83, 4000.52, 4000.52),
      B = c( 1820.16, 1743.27, 1524.04, 1021.86, 3482.27, 9210.6, 4851.26,
          3821.28, 4371.83, 3233.19, 3699.17, 8645.15, 5162.93, 2076.86, 2724.49,
          4735.88, 3270.51, 2418, 3416.86, 12243.91, 14570.25, 12536.41, 9492.82,
          5486.86, 6117.09, 6775.39, 7545.41 ) )

  # lognormal timeseries method
  res0 = optim( params, fn=biomass.logistic.recursion, O=scallop.example$B, C=scallop.example$C, 
      errorType="lognormal" ) 
  res1 = nlminb( start=params, objective=biomass.logistic.recursion, O=scallop.example$B, C=scallop.example$C,
      errorType="lognormal" , control=list(eval.max=500, iter.max=500 ))
  res2 = nlm( p=params, f=biomass.logistic.recursion, O=scallop.example$B, C=scallop.example$C,errorType="lognormal"  )
  
  # Schnute's method
  res3 = biomass.logistic.schnute.noequilibrium( O=scallop.example$B, C=scallop.example$C ) 
   

  res = res0$par
  # eg --
  # normal errors
  # res = optim( params, fn=biomass.logistic, O=scallop.example$B, C=scallop.example$C, errorType="normal" ) 
  #          r            K            q           B0 
  #   2.386061 26398.577857     0.223553  7000.074956 
  
  # in this run, lognormal errors are used:   

  # to obtain accessory estimates using solution parameters obtained above
  o = biomass.logistic( r=res["r"], K=res["K"], q=res["q"], B0=res["B0"],
    O=scallop.example$B, C=scallop.example$C, TAC=rep(4000.52, 5), yrs=scallop.example$yrs ) 
  
  # a few useful plots
  ymax = max( c( o$res$O, o$res$Op ), na.rm=T )
  plot( Op ~ yrs, o$res, type="b", ylim=c(0, ymax), pch=20 )  # predicted catch rate
  points( O ~ yrs, o$res, pch=22, col="red" )  # observed catch rate
  lines( O ~ yrs, o$res, lty="dashed", col="red" )  # observed catch rate
  abline ( v=scallop.example$yrs[ length(scallop.example$yrs)]+0.5, lty="dotted" )
  

  # catch
  plot( C ~ yrs, o$res, type="b" )
  abline ( v=scallop.example$yrs[ length(scallop.example$yrs)]+0.5, lty="dotted" )

  # biomass
  plot( B ~ yrs, o$res, type="b" ) 
  abline ( v=scallop.example$yrs[ length(scallop.example$yrs)]+0.5, lty="dotted" )


  # fishing mortality
  plot ( F ~ yrs, o$res, type="b" )
  abline ( v=scallop.example$yrs[ length(scallop.example$yrs)]+0.5, lty="dotted" )
  
  
  # surplus production 
  plot( P ~ yrs, o$res, type="b", pch=20 )
  abline ( v=scallop.example$yrs[ length(scallop.example$yrs)]+0.5, lty="dotted" )
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




#---------------
# now using INLA to do the above:

# observation equation: observed biomass O with error;  and true unobserved biomass B
  O{t} = B{t}/q    ; q ~ LN(0.V)  -- eq1 (observation equation)

# population dynamic equation: logistic
  B{t} = B{t-1} + r * B{t-1} * ( 1 - B{t-1} / K ) - C{t-1}    


  require(INLA)

  Schnute parameterisation:

  ln( O[t+1]/O[t] ) = 4 * m / K - 4 * m / (q *K^2) *(O[t] +O[t+1])/2 - q*( C[t]/O[t] )
  
  nyrs = length(scallop.example$yrs)
  ne = 2  # number of state equations

    ii = 2:nyrs
    jj = 1:(nyrs-1)
    y  = log( scallop.example$B [ii] / scallop.example$B [jj] ) 
    x1 = (scallop.example$B[ii] + scallop.example$B[jj]) /2 
    x2 = (scallop.example$C[ii] / scallop.example$B[ii] + scallop.example$C[jj]/scallop.example$B[jj]) /2

  dat = data.frame( y, x1, x2, yr=scallop.example$yrs[jj] )

  # stacking the data 
  # e1 = expectation for state equation 1 = observed biomass, O ~ N( B{t}, V^-1 )
  # e2 = expectation for state equation 2 = 0 (after moving everything to right hand side) ~ N(0,W^-1) 
  #       deterministically == 0 .. so force high fixed precision (low variance) CV=-10
  # ti = time (yr) 
  # iB = index of B{t}
  # iBp = index of B{t-1}
  # 
  
  d1 = data.frame( 
    e1=scallop.example$B , # left hand side of eq 1
    e2=NA, # left hand side eq 2 not relevent .. set to NA
    ti=scallop.example$yrs, 
    iB=1:nyrs, # indices for B{t} in eq 1
    iBp=NA # indices for B{t-1} in eq 1  ... does not appear in eq 1 so set to NA
  )

  d2 = data.frame( 
    e1=NA,  # left hand side of eq 1 not relevent .. set to NA
    e2=0,   # left hand side of eq 2 
    ti=scallop.example$yrs[-1],  # remove the first year as this is the index for t-1 
    iB=2:nyrs,   # indices for B{t} in eq 2 (values must match those in d1)
    iBp=1:(nyrs-1) # indices for B{t-1} in eq 2 
  )
  
  d = rbind( d1, d2)

  Y = as.matrix( d[,c(1:ne)] )


