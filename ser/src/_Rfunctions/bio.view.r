bio.view = function (b, title, print, t0, t1)
{ 
  source(file.path(grdfishdir, "colour.functions.r"))
      
  corel = cor(t(b), use="pairwise.complete.obs")  

# Z = lower.tri(corel,diag=F) * corel
  Z = corel 
  br = seq(-1, 1, .02)
  X11()
  image( Z, x=c(t0:t1), y=c(t0:t1), 
         breaks=br, col=rev(green.red(length(br))), 
         xlab="", ylab="", axes=F )
  axis( 1, labels=T)
  axis( 2, labels=T)
  #  text(1970, 2000, title, adj=c(0,0), cex=2)
  #  text(1971, 1998, "green - positive correlation", adj=c(0,0), cex=1.2)
  #  text(1971, 1996.5, "red    - negative correlation", adj=c(0,0), cex=1.2)
  if (print)  dev.print (pdf, file=paste(unlist(strsplit(title,split=" "))[1], ".m", ".pdf", sep=""), width=6, height=6)
}



