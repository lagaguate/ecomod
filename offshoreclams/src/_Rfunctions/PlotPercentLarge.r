PlotPercentLarge <- function(fig.num, lf.data, Sel.Bank){
  ##############################################################################
  ## Calculate and plot percentage of clams over 105 or 120 mm in the 
  ##  commercial samples
  ##############################################################################
  op <- par(
            omi = c(6, 1, 0.5, 1.25),      ## Set outer margins 
            mai = c(1, .75, 0.5, 0.5)  ## Set Figure margins (inches)
            )  
  ##---------------------------------------------------
  ge.txt <- "\u2265"  ## >= symbol
  if(Sel.Bank == 1){
    len.data <- subset(lf.data, lf.data$AREA == 4)
    y.limit <- c(0, 15)
    t.level <- 1
    c.size <- 120
    bank.txt <- "Banquereau Bank"
    sel.txt <- "120"
    trigger.x.pos = 1999
    lowess.y.pos = 4
  }else if(Sel.Bank == 2){
    len.data <- subset(lf.data, lf.data$AREA == 3)
    y.limit <- c(-4, 50)
    t.level <- 0.5
    c.size <- 105
    sel.txt <- "105"
    bank.txt <- "Grand Bank"
    trigger.x.pos = 2000
    lowess.y.pos = 25
  }
  
  yy <- seq(min(len.data$YEAR),max(len.data$YEAR))
  yp <- unique(len.data$YEAR)
  miss.years <- yy[match(yy,yp,0) == 0]
  t <- matrix(rep(0, 4 * length(miss.years)), length(miss.years) , 4)
  t[,2] <- miss.years
  t[,1] <- len.data[1,1]
  t[,3] <- 50
  t <- as.data.frame(t)
  names(t) <- names(len.data)
  len.data <- merge(t,len.data, all = TRUE)
  
  ## reformat into columns
  len.mat <- cast(len.data, SHELL_LEN~YEAR, sum, value = "NUM", fill = 0) 
  ## calculate percentages
  p_mat <<- prop.table(as.matrix(len.mat), margin = 2) * 100  
  out <- p_mat
  for (i in 1:ncol(out)){out[ ,i] = cumsum(out[, i])} ## get cumulative %
  ## y values = shell lengths
  P120 <<- 100 - 
    out[which(as.integer(unlist(dimnames(out)[1])) == c.size - 1), ] 
  plot(cbind(as.integer(names(P120)), P120), ylim = y.limit, pch = 20, 
       col = "red", lty = 2, xlab = "Year", tcl = -0.5, cex = 1.0,
       mgp = c(2.8, 1, 0),
       ylab = bquote(paste("Percent Shell Length" >= .(sel.txt), "mm"))) 
   thigmophobe.labels(as.integer(names(P120)), P120,
                     labels = colSums(len.mat))
  ## lowess fit to data
  low.fit <- lowess(cbind(as.integer(names(P120)), P120)) 
  lines(low.fit, col = "red") 
  abline(h = t.level, col = "black", lty = 2)
  text(2001, lowess.y.pos, "lowess trend fit", pos = 4, col = "red")
  text(trigger.x.pos, t.level - 0.05 * y.limit[2],
       bquote(paste("trigger level for %" >= .(sel.txt),"mm")),
       adj = c(0,0), pos = 4)
  xs <- as.integer(names(P120))                ## set up for sub tic marks
  ys1 <- rep(par("usr")[3], length(P120))
  ys2 <- rep(par("usr")[3] - 0.2, length(P120))
  segments(xs, ys1, xs, ys2, xpd = NA)
  text.3 <- paste("on ", bank.txt, ".", sep = "")
  if(Sel.Bank == 2) { text.3 <- paste(text.3,
           " Values since 2006 are not representative of stock status.",
           sep = "")
  }
  mtext(bquote(paste("Figure ", .(fig.num), 
                     ". Percent of large (" >= .(sel.txt)," mm)",
                      " Arctic Surfclams in unsorted commercial catch")),
        side = 1, line = -0.5, adj = 0, outer = TRUE)
  mtext(text.3, side = 1, line = 0.5, adj = 0, outer = TRUE)
  ## Now make table #######################################################
  out.table2 <- cbind( colSums(len.mat), as.numeric(P120))

  colnames(out.table2) <- c("n Unsorted", 
                            paste("% >= ", sel.txt, sep=""))
  return(list(p_mat = p_mat, P120 = P120, Table = out.table2))
  par(op) ## reset parameters
} ## end of function PlotPercentLarge


