
   setmerge = function(X, Y, varname, filter, variable, index=NULL) {
    factors = c("trip", "set")
    if (!is.null(filter)) {
      i = filter.class(Y, filter )
    } else {
      i = index  # "index" is the override mechanism
    }
    if (length(i)>0) {
      y = sum.data(Y[i,], factors, variable)
      names(y) = c(factors, varname)
      X = merge(x=X, y=y, by=factors, all.x=T )
      X[,varname] = X[,varname] / X$sa   # express as x / km2
      X[!is.finite(X[,varname]),varname] = 0
    } else {
      dummy = rep(0, dim(X)[1])
      oldnames = names(X)
      X = cbind(X, dummy)
      names(X) = c(oldnames, varname)
    }
    return(X)
  }


