

  gam.simulation = function( M, X, nsims=1000 ) {
    library(MASS)
    Xh <- predict(M, newdata=X, type="lpmatrix") # ~ 8 min for ncol=1000 and nvars=100 
    M.coef <- mvrnorm(nsims, coef(M), M$Vp) ## nsims replicate param. vectors
    res <-  Xh %*% t(M.coef) 
    res = family(M)$linkinv( res )  # inverse.transform.glm
    return( res )
  }


