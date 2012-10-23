
  xtab.2way = function( xval, factors, k=10^6 ) {
    v =  as.matrix( xtabs( as.integer( xval * k ) ~ factors[,1] + factors[,2] ) /k )
    rn = rownames(v)
    cn = colnames(v)
    attr(v, "dimnames") = NULL
    attr(v, "class") = NULL
    attr(v, "call") = NULL
    w = as.data.frame(v)
    rownames(w) = rn
    colnames(w) = cn
    return (w)
  }


