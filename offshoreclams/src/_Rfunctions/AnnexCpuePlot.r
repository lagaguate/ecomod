AnnexCpuePlot <- function(fig.num, Ann.data, Sel.Bank, normalize.plot){
  ##############################################################################
  ## Annex: Plot of Annual average CPUE for last 4 active vessels
  ##############################################################################
  op <- par(
            mfrow = c(1,1),
            omi = c(5, 1, 1, 1),     ## Set outer margins with space at bottom
            mai = c(1, 1.0, 0.25, 0.25)  ## Set Figure margins (inches)
           ) 
  max.cpue <- max(Ann.data$CPUE)
  y.lab = expression(paste("CPUE g/",m^2))
  y.lim = c(0, 300)
  normal.divisor <- 1.0
  if(normalize.plot == 1){
       Ann.data$CPUE <- Ann.data$CPUE / max.cpue   ## normalize CPUE to Maximum
       y.lab = "Normalized CPUE"
       y.lim = c(0, 1.1)
       normal.divisor <- max.cpue
  }
  ## matrix and Title for plotting already exists as L4
  plot(Ann.data[, c("Year", "CPUE")],
       xlim = range(Ann.data$Year) + c(-1, 1),
       ylim = y.lim, pch = 20, 
       col = 'black',
       xlab = "Year", ylab = y.lab, axes = FALSE,
       ann = TRUE, xaxt = "n")
  axis(2)
  
  axis.POSIXct(Ann.data$Year, side = 1, format = "%Y", labels = TRUE,
               xaxs = "i", tcl = -0.5) #tics & labels
  r <- range(Ann.data$Year) + c(-60*60*24*365,60*60*24*365) 
  axis.POSIXct(1, at = levels(cut(r, "years")), format = "%Y", tcl = -0.25,
               labels = FALSE) ## minor tic marks
  box()
    ## Now do overall annual CPUE
  lines(Ann.data[ , c("Year", "CPUE")], pch = 20, col = "black")
  ## Draw dashed black line for proposed lower CPUE limit
  if(Sel.Bank == 1){
    cpue.trigger = 70 / normal.divisor
    bank.txt = "Banquereau Bank"
  }else if(Sel.Bank == 2){
    cpue.trigger = 50 / normal.divisor
    bank.txt = "Grand Bank"
  }
  lines(matrix(c(range(Ann.data$Year), 
                 c(cpue.trigger, cpue.trigger)), 2, 2), col = "black",
        lty = 2) 
  text(as.POSIXct("1991", format = "%Y"), cpue.trigger,
       "Trigger level for CPUE", col = "black", pch = 20, pos = 4)
  ## Add text below Figure
  if(normalize.plot == 1){
    text.1 <- paste("Figure", as.character(fig.num), 
              ". Normalized (to maximum value) Annual CPUE for the last four",
                    " vessels active")
    text.2 <- paste(" in the Arctic Surfclam fishery on ", bank.txt, ".",
                    sep = "")
    if(Sel.Bank == 2){ paste(text.2, 
               " Values since 2006 are not representative of stock status.",
               sep = "")}
  } else {
    text.1 <- paste("Figure", as.character(fig.num), 
                    ". Annual CPUE for the last four vessels active",
                    " in the Arctic Surfclam fishery")
    text.2 <- paste(" on ", bank.txt, ".", sep = "")
    if(Sel.Bank == 2){ paste(text.2, 
                " Values since 2006 are not representative of stock status.",
                sep = "")}
  }
  op2 <- par(xpd = NA)  ## no clipping region
  mtext(text.1, cex = 1, side = 1, line = 1, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 2, adj = 0, outer = TRUE)
  par(op) ## reset parameters
  par(op2) ## reset parameters
} ## End of function AnnexCpuePlot


