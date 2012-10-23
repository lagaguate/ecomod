
# ordination ans data sort into a graphical time series..

  pca.analyse.nosort = function(b) {
    years=rownames(b)
    vars =colnames(b)

    Z=as.matrix(as.data.frame(b))
    a = max ( abs(min(Z, na.rm=T)), abs(max(Z, na.rm=T)) )
    br = seq( -a, a, .1)
    X11()
    par(mai=c(2, 6, 1, 0.5))
    par(cex=2)
    image(  z=Z, x=c(t0:t1), breaks=br, col=rev(green.red(length(br))),
            xlab="Years", ylab="", axes=F )

#    for (i in seq(t0, t1, by=10)) abline( v=i-0.5, col="slategray")
    for (i in seq(t0, t1, by=10)) abline( v=i, col="slategray")
#    for (i in seq(t0, t1, by=1)) abline( v=i-0.5, col="slategray1")
    for (i in seq(t0, t1, by=1)) abline( v=i, col="slategray1")

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
  }


