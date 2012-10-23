
  pca.analyse = function(b, colscheme="default", addscores=T, sortsequence=F) {
    years=rownames(b)
    vars =colnames(b)
    corel = cor(b, use="pairwise.complete.obs")
    corel[is.na(corel)] = 0
    s = svd(corel)
    scores = matrix.multiply (b, s$v)  # i.e., b %*% s$v  .. force a multiplication    ignoring NA
    evec = s$v
    eval = s$d
    x = cbind(scores[,1]/sqrt(eval[1]), scores[,2]/sqrt(eval[2]) )
    y = cbind(evec[,1]*sqrt(eval[1]) , evec[,2]*sqrt(eval[2]) )

    outscores = data.frame(x)
    outscores$yr = as.numeric(years)

    x11()
    par(cex=2, lty=2)
    plot(years,x[,1], ylab=paste("PCA1 (", round(eval[1]/sum(eval)*100, 1), "%)", sep="") )
    lines(lowess(years,x[,1], f=1/5) )
    dev.print (pdf, file="scoresPC1.pdf", width=12, height=8)

    x11()
    par(cex=2, lty=2)
    plot(years,x[,2], ylab=paste("PCA2 (", round(eval[2]/sum(eval)*100, 1), "%)", sep="" ))
    lines(lowess(years,x[,2], f=1/5))
    dev.print (pdf, file="scoresPC2.pdf", width=12, height=8)


    # form residual variations figure based upon a sorting of ordination scores
    varloadings = NULL
    varloadings = as.data.frame(cbind(y, vars))

    q=as.data.frame(t(as.matrix(b)))
    q$vars = rownames(q)
    q = merge(x=q, y=varloadings, by.x="vars", by.y="vars", all=T)

    ordered = sort(as.numeric(as.character(q$V1)),  index.return =T, decreasing=sortsequence)
    
    qq=q[ ordered$ix, ]

    if (addscores) {
      varnames = paste(qq$vars, 
                     " {", 
                     round(as.numeric(as.character(qq$V1)),2), 
                     ", ",
                     round(as.numeric(as.character(qq$V2)),2), 
                     "}", 
                     sep="")
    }
    else if (!addscores) { varnames = qq$vars }
    
      
    qq$vars = NULL
    qq$V1 = NULL
    qq$V2 = NULL
    qq = as.data.frame(t(qq))
    colnames(qq) = varnames
    Z = as.matrix(qq)

    a = max ( abs(min(Z, na.rm=T)), abs(max(Z, na.rm=T)) )
    br = seq( -a, a, .1)

    X11()
    par(mai=c(2, 6, 1, 0.5))
    par(cex=2)
    if (colscheme=="default") image(z=Z, x=c(t0:t1), breaks=br, col=rainbow(length(br)-1, 
                                    start=0, end=1/3), xlab="Years", ylab="", axes=F )
    if (colscheme=="redgreen") image(z=Z, x=c(t0:t1), breaks=br, col=rev(green.red(length(br))),
                                     xlab="Years", ylab="", axes=F )
    if (colscheme=="heat") image(z=Z, x=c(t0:t1), breaks=br, col=rev(heat.colors(length(br)-1)),
                                 xlab="Years", ylab="", axes=F )

    for (i in seq(t0, t1, by=10)) abline( v=i-0.5, col="slategray")
#    for (i in seq(t0, t1, by=10)) abline( v=i, col="slategray")
    for (i in seq(t0, t1, by=1)) abline( v=i-0.5, col="slategray1")
#   for (i in seq(t0, t1, by=1)) abline( v=i, col="slategray1")

    z = 1/(dim(Z)[2] - 1)
    for (i in seq(0, 1, by=z*10)) abline( h=i-(z/2), col="slategray")
    for (i in seq(0, 1, by=z)) abline( h=i-(z/2), col="slategray1")

    par(las=1)
    axis( 1 )

    vars = colnames(Z)
    par(cex=1.4)
    axis( 2, at=seq(0,1,length=length(vars)), labels=vars)
    write.table(Z, file="anomalies.dat", quote=F, sep=";")
    dev.print (pdf, file="anomalies.pdf", width=20, height=20)
    system ("acroread anomalies.pdf &")

    return(outscores)
  }



