
Footprint <- function(fig.num, BB_Footprint, Sel.Bank){
  ##############################################################################
  ## Look at footprint of the fishery, still using same data
  ##############################################################################
  ## plot catch and footprint
  op <- par(                        ## set up figure parameters
    omi = c(5, 1, 1, 1),      ## Set outer margins
    mai = c(1, 1.0, 0.25, 1.0)  ## Set Figure margins (inches)
  )  
  Ax_Tic = seq(range(BB_Footprint$Year)[1], range(BB_Footprint$Year)[2], 1)
  plot(BB_Footprint[, c("Year", "Logged Catch (t)")], ylim = c(0, 30000), 
       pch = 20, type = "b", xaxt = "n", col = "green", ann = FALSE, las = 1)
  if(Sel.Bank == 1){
    lines(matrix(c(1988, 2005.5, 30000, 30000), 2, 2), 
          col = "green", lty = 1, lwd = 2)
    lines(matrix(c(2005.5, max(BB_Footprint$Year), 24000, 24000), 2, 2), 
          col = "green", lty = 1, lwd = 2)
    text(1989, 30000, "TAC", pos = 1, col = "green")
  }else if(Sel.Bank == 2){
    lines(matrix(c(range(BB_Footprint$Year), 20000, 20000), 2, 2), 
          col = "green", lty = 1, lwd = 2)
    text(2005, 20000, "TAC", pos = 3, col = "green")  
  }
  
  mtext("Year", side = 1, line = 2.5)             ## Add Axis titles
  mtext("Catch (t)", side = 2, line = 3.5)
  op2 <- par(new = TRUE)                        ## reset for overlay plot
  plot(BB_Footprint[, c("Year", "Area Dredged (km²)")], xaxt = "n",
       type="b", pch = 20, col = "red", lty = 2,
        ylim = c(0, 300), axes = FALSE, ann = FALSE)
  axis(4)                       ## draw new y axis on right
  ## Add Y-axis title
  mtext(expression(paste("Footprint (k", m^2, ")")), side = 4, line = 2.5) 
  if(Sel.Bank == 1){
    legend(1987, 230, c("Catch", "Footprint"), bty = "n", pch = 20, 
           col = c("green", "red"))
    lines(matrix(c(range(BB_Footprint$Year), c(250,250)), 2, 2), 
          col = "red", lty=2, lwd=2)
    text(2004, 248, "Trigger level for Footprint", pos = 3, col = "red")
    bank.txt <- "Banquereau Bank"
  } else if(Sel.Bank == 2){
    legend(1987, 275, c("Catch", "Footprint"), bty = "n", pch = 20, 
           col = c("green", "red"))
    lines(matrix(c(range(BB_Footprint$Year), c(125, 125)), 2, 2), 
          col = "red", lty = 2, lwd = 2)
    text(2003.5, 125, "Trigger level for Footprint", pos = 3, col = "red")
    bank.txt <- "Grand Bank"
  }
  axis(1, tcl = -0.2, labels = FALSE, at = Ax_Tic)  # minor tics for x axis
  axis(1, tcl = -0.5)  ## Major tics and labels for x axis

  mtext(bquote(paste("Figure ", .(fig.num),
                     ". Footprint (k", m^2, ") of the offshore clam fishery ",
                     "by Year on ", .(bank.txt), ".", sep = "")),
        side = 1, line = 0, adj = 0, outer = TRUE)
 
  par(op) ## reset parameters
  par(op2) ## reset parameters
 ## return(out.table)
} ## end of function Footprint


