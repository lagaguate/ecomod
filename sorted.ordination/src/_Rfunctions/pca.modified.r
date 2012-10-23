 pca.modified = function (b) {
    id = rownames(b)
    vars = colnames(b)
    corel = cor(b, use="pairwise.complete.obs")
    corel[is.na(corel)] = 0
    s = svd(corel)
    scores = matrix.multiply (b, s$v)  # i.e., b %*% s$v  .. force a multiplication ignoring NA
    evec = s$v
    eval = s$d
    x = cbind(scores[,1]/sqrt(eval[1]), scores[,2]/sqrt(eval[2]) )
    y = cbind(evec[,1]*sqrt(eval[1]) , evec[,2]*sqrt(eval[2]) )
    X11()
    biplot(x, y, var.axes=T, col=c("blue", "red"), ylabs=vars)
    X11()
    plot(y)
    text(y[,1], y[,2], vars)
  }

