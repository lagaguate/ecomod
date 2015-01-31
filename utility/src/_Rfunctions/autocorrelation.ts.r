  correlation.ts <- function( dat, formu='y1 ~ 0 + f( x, model="ar1" )', method="inla", inla.vars=list(x="x", y="y"),
    random.ar=0.8, random.missing=0.3, random.n=200, ... ) {
    
    if ( method=="random.data" ) {
      n = random.n
      x =  1:n
      y = arima.sim(n=n, model=list( ar=random.ar ))
      missing.values = sample( x, trunc(n * random.missing ))  
      y1 = y
      y1[ missing.values ]  = NA 
      y1  = y1 + rnorm( n, mean=0, sd=sd( y ) )  # add noise
      dat = data.frame( x=x, y=y, y_noise=y1 ) 
      return(dat)
    }

    if (method == "inla" ) {
      nn = abs( diff( dat[inla.vars["x"]]  ) )
    dd = median( nn[nn>0], na.rm=TRUE )
    z$xiid = z$x = jitter( z$x, amount=dd / 20 ) # add noise as inla seems unhappy with duplicates in x?
    rsq = 0
    nw = length( which(is.finite( z$y)))
    nw0 = nw + 1
 
      r = inla( formu, data = dat, ... )
      dat$predictions =  r$summary.random$x[["mean"]]
      ar.pred =  r$summary.hyperpar["Rho for x", "mean" ]
      mm = glm( predictions~y, data=dat )
   

    }

  
  }
