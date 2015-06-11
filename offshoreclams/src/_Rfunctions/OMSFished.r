OMSFished <- function(fig.num, BBData, Sel.Bank){
  ##############################################################################
  ## Look at area impacted as number of one minute squares fished
  ##############################################################################
  op <- par(                        ## set up figure parameters
            omi = c(5, 1, 1, 1),      ## Set outer margins
            mai = c(1, 1.0, 0.25, 0.25)  ## Set Figure margins (inches)
            )  
  if(Sel.Bank == 1){
    bank.txt = "Banquereau Bank"
  }else if(Sel.Bank == 2){
    bank.txt = "Grand Bank"
  }
  # Lat and Long with positve Long
  pos <- cbind(BBData$LAT_DD, -1  *BBData$LON_DD) 
  # one minute resolution DDMM
  squares <- trunc(pos) * 100 + (1 + trunc((pos %% 1) * 60))
  # DDMMDDMM as one value for unique square
  oms <- squares[ ,1] * 10000 + squares[ ,2] 
  # number of unique 1 minute squares fished
  squares.fished <- length(unique(oms))  
  # oms.freq is a table of frequency of minute squares fished by year
  temp <- aggregate(oms ~BBData$Year, FUN = length) 
  ## fill in missing years
  yy <- seq(min(temp[,1]),max(temp[,1]))
  oms.freq <- cbind(yy,rep(0,length(yy)))
  oms.freq[match(temp[,1],oms.freq[,1]),2] <- temp[,2]
  oms.freq <- as.data.frame(oms.freq)
  colnames(oms.freq) <- c("Year", "One minute squares fished")
  ##############################################################################
  ## now do plot of oms.freq
  ##############################################################################
  y.max <- c(2500, 1500)
  ax.tic <- seq(range(oms.freq$Year)[1], range(oms.freq$Year)[2], 1)
  plot(oms.freq, ylim = c(0,y.max[Sel.Bank]), pch = 20, type = "b", xaxt = "n",
       col = "red", ann = FALSE, las = 1)
  axis(1, tcl = -0.2, labels = FALSE, at = ax.tic) # minor tics for x axis
  axis(1, tcl = -0.5) # Major tic marks and labels for x axis
  mtext("Year", side = 1, line = 2.5)
  mtext("One Minute Squares Fished", side = 2, line = 3.5)
  ## Add text below Figure
  text.1 = paste("Figure ", as.character(fig.num),
                 ". Number of one minute squares fished by the offshore clam ",
                 "fishery by year ", sep = "")
  text.2 = paste("on ", bank.txt, ".", sep = "")
  op2 <- par(xpd = NA)  ## no clipping region
  mtext(text.1, side = 1, line = 1, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 2, adj = 0, outer = TRUE)
  par(op) ## reset parameters
  par(op2) ## reset parameters
  return(oms.freq)
} ## end of function OMSFished


