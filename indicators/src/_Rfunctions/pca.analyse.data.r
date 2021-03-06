  pca.analyse.data = function(X, t0, t1, fname, vars=NULL, newnames=NULL) {
    
    if (!is.null(vars)) X = X[, vars ]
    if (!is.null(newnames) ) names(X) = newnames
    X = X[ which( rowSums( X, na.rm=T) !=0 ) ,]
    X = X[ which( as.numeric(rownames(X)) >= t0 ) ,]
    X = X[ which( as.numeric(rownames(X)) <= t1 ) ,]
    X = scale( X, center=T, scale=T )
    X.stats = sorted.ordination( X, outfileroot=fname ) # look in work directory for figures and data outputs
    return( X.stats ) 
  }




