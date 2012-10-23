bio.image = function (Z, title, print, t0, t1)
{  
  source(file.path(grdfishdir, "colour.functions.r"))
  Z = bio.form(Z, t0, t1)
  a = max ( abs(min(Z, na.rm=T)), abs(max(Z, na.rm=T)) ) 
  br = seq( -a, a, .1)
  X11()
  par(mai=c(1, 2.5, 1, 0.5))
  image( z=Z, x=c(t0:t1), breaks=br, col=rev(green.red(length(br))), xlab="Years", ylab="", axes=F )

  for (i in seq(t0, t1, by=10)) abline( v=i-0.5, col="slategray")
  for (i in seq(t0, t1, by=1)) abline( v=i-0.5, col="slategray1")
  
  z = 1/(dim(Z)[2] - 1)
  for (i in seq(0, 1, by=z*10)) abline( h=i-(z/2), col="slategray")
  for (i in seq(0, 1, by=z)) abline( h=i-(z/2), col="slategray1")

  par(las=1)

  vars = colnames(Z)
  axis( 1 )
  axis( 2, at=seq(0,1,length=length(vars)), labels=vars)
  #  title( main=title)
  #  text( t0+2, 0.99, "green - above global mean", adj=c(0,0), cex=1.5)
  #  text( t0+2, 0.95, "red   - below global mean", adj=c(0,0), cex=1.5)
  if (print)  dev.print (pdf, file=paste(unlist(strsplit(title,split=" "))[1], ".spec", ".pdf", sep=""), width=14, height=14)
}



