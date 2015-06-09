PlotLengthFreq <- function(fig.num, P120, p_mat, bank.txt, table, c.size){
  ##############################################################################
  ## Plot of length frequencies
  ##############################################################################
  yy <- as.integer(names(P120))[seq(1, length(P120))]
  y.lab.pos <- as.integer(length(yy) / 2) + 1
  ## set up for series of plots on page
  op <- par(
            mfrow = c(length(yy), 1), 
            mar = c(0, 5, 0 , 1),
            omi = c(2, 1, 1.5, 1)
           ) 
  t <- as.integer(rownames(p_mat))
  t2 <- trunc((t - 1) / 5) * 5 + 2.5
  for(i in 1:dim(p_mat)[2]) {
    t3 <- aggregate(p_mat[, i], by = list(as.factor(t2)), FUN = sum)
    plot(as.numeric(as.character(t3$Group.1)), t3$x, xlim = c(0,200),
         ylim = c(0, 29),type="l", col = "black", lty = 2, 
         xaxt = 'n', ann = FALSE)
    abline(v = c.size, lty = 2, col = "red", xpd = FALSE)
    text(175,15,label = yy[i]) ## add year label
    text(10,15,label = paste("n = ", prettyNum(table[i, 1], big.mark = ","),
                                                sep="")) # add n
    ## Add Y-axis title
    if(i == y.lab.pos){mtext("Frequency %", side = 2, line = 2.5)} 
  }
  axis(1)
  mtext("Shell Length (mm)", side = 1, line = 2.5)  ## Add Y-axis title
  # Add figure text below plots
  text.1 <- paste("Figure ", as.character(fig.num),
                  ". Length frequencies of unsorted Arctic Surfclams in",
                  " commercial catches", sep = "")
  text.2 <- paste("on ", bank.txt, ".", sep = "")
  mtext(text.1, side = 1, line = 5, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 6.5, adj = 0, outer = TRUE)
  ## reset parameters
  par(op) ## reset parameters
} ## end of PlotLengthFreq function


