bio.view.t = function (b, title, print, t0, t1)
{ 
  source(file.path(grdfishdir, "colour.functions.r"))
  corel = cor(b, use="pairwise.complete.obs")  
  # Z = lower.tri(corel,diag=F) * corel
  Z = corel
  br = seq(-1, 1, .02)
  X11()
  par(mai=c(2, 0.1, 0.1, 2))
  par(las=2)
  image( z=Z, breaks=br, col=rev(green.red(length(br))), xlab="", ylab="", axes=F )
  vars = colnames(b)
  axis( 1, at=seq(0,1,length=length(vars)), labels=vars)
  axis( 2, at=seq(0,1,length=length(vars)), labels=vars)
  #  text(0.1, 0.95, title, adj=c(0,0), cex=2)
  #  text(0.15, 0.92, "green - positive correlation", adj=c(0,0), cex=1.2)
  #  text(0.15, 0.90, "red    - negative correlation", adj=c(0,0), cex=1.2)
  if (print)  dev.print (pdf, file=paste(unlist(strsplit(title,split=" "))[1], ".mt", ".pdf", sep=""), width=14, height=14)
}



