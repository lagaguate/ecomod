
CpuePlot <- function(fig.num, log.data, Sel.Bank){
  ##############################################################################
  ## Plot of CPUE for last 4 active vessels
  ##############################################################################
  op <- par(
    mfrow <- c(1,1),
    omi = c(5, 1, 1, 1),      ## Set outer margins with space at bottom
    mai = c(1, 1.0, 0.25, 0.25)  ## Set Figure margins (inches)
  )  
  ## Set up matrix and Title for plotting
  ## matrix of info for last four vessles active in fleet
  L4<- cbind(c(101277, 101276, 133542, 176085, 000000),
             c("Atlantic Vigour", "Atlantic Pursuit", "Ocean Concord",
               "Arctic Endurance", "Average all four"),
             c("red", "blue", "green", "orange", "black"))
  L4[,1] <- as.numeric(L4[,1]) ## ensure CFVs ar numeric and not text
  ## Data for last 4 vessels 
  L4Data <- subset(log.data, (log.data$CFV %in% L4[, 1] 
                              & log.data$BANK == Sel.Bank)) 
  ## MMM - had to wrap trip_no in as.numeric to allow error-less
  ##      addition of CFV and trip_no
  temp4Trip <- cbind(aggregate(RECORD_DATE~CFV + as.numeric(TRIP_NO),
                               data = L4Data, mean,na.action = na.omit), 
                     aggregate(cbind(ROUND_CATCH, AREA) ~CFV + 
                                 as.numeric(TRIP_NO), data = L4Data,
                                sum, na.rm = F))
  ## CPUE recalculated after summations
  temp4Trip$CPUE <- temp4Trip$ROUND_CATCH / temp4Trip$AREA * 1000 
  ## plot with points for trip CPUE values by vessel
  plot(temp4Trip[, c("RECORD_DATE", "CPUE")],
       xlim = range(temp4Trip$RECORD_DATE) + c(-60*60*24*100,60*60*24*100),
       ylim = c(0, 500), pch = 20, 
              col = L4[match(temp4Trip$CFV, as.numeric(L4[, 1])),3],
       xlab = "Year", ylab = expression(paste("CPUE g/",m^2)), axes = FALSE,
       ann = TRUE, xaxt = "n")
  axis(2)
  
  ## Now do annual values for each vessel
  temp4YEAR <- aggregate(cbind(ROUND_CATCH, AREA) ~CFV + Year, 
                         data = L4Data, sum)
  ## Year has to be converted back to POSIXct date
  temp4YEAR$Year <- as.POSIXct(paste(temp4YEAR$Year, "-07-01", sep = "")) 
  ## CPUE calculated after summations
  temp4YEAR$CPUE <- temp4YEAR$ROUND_CATCH / temp4YEAR$AREA * 1000

  axis.POSIXct(temp4YEAR$Year, side = 1, format = "%Y", labels = TRUE,
               xaxs = "i", tcl = -0.5) #tics & labels
  ## add +- 365 days in seconds to range
  r <- range(temp4YEAR$Year) + c(-60*60*24*100, 60*60*24*365) 
  axis.POSIXct(1, at = levels(cut(r, "years")), format = "%Y", tcl = -0.25,
               labels = FALSE) ## minor tic marks
  box()
  
  for(v in 1:4){ ## add annual values as lines
    lines(temp4YEAR[which(temp4YEAR$CFV == L4[v, 1]), c("Year", "CPUE")], 
          pch = 20, col = L4[v, 3])
  }
  legend("topleft",L4[,2],bty="n",pch=20,col=L4[,3]) ## Add legend
  
  ## Now do overall annual CPUE
  ANNUAL <- aggregate(cbind(ROUND_CATCH, AREA)~Year, data = L4Data, sum)
  ## Year has to be converted back to POSIXct date
  ANNUAL$Year <- as.POSIXct(paste(ANNUAL$Year, "-07-01", sep = "")) 
  ## CPUE calculated after summations
  ANNUAL$CPUE = ANNUAL$ROUND_CATCH / ANNUAL$AREA * 1000  
  lines(ANNUAL[ , c("Year", "CPUE")], pch = 20, col = "black")
  ## Draw dashed black line for proposed lower CPUE limit
  if(Sel.Bank == 1){
    cpue.trigger = 70
    bank.txt = "Banquereau Bank"
  }else if(Sel.Bank == 2){
    cpue.trigger = 50
    bank.txt = "Grand Bank"
  }
  lines(matrix(c(range(temp4Trip$RECORD_DATE), 
                 c(cpue.trigger, cpue.trigger)), 2, 2), col = "black",
                 lty = 2) 
  text(as.POSIXct("1991", format = "%Y"), cpue.trigger,
       "Trigger level for CPUE", col = "black", pch = 20, pos = 4)
  ## Add text below Figure
  text.1 <- paste("Figure", as.character(fig.num), 
                  ". CPUE for the last four vessels active in the Arctic ",
                   "Surfclam fishery on ", sep = "")
  text.2 <- paste( bank.txt,
                   ". Symbols are coloured by vessel, dots are CPUE by trip,",
                  " and lines ", sep = "")
  text.3 <- "connect annual averages for vessels."
  op2 <- par(xpd = NA)  ## no clipping region
  mtext(text.1, cex = 1, side = 1, line = 1, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 2, adj = 0, outer = TRUE)
  mtext(text.3, side = 1, line = 3, adj = 0, outer = TRUE)
  ## Add caution for recent low effort on Grand Bank
  text.4 <- " Values since 2006 are not representative of stock status."
  if(Sel.Bank == 2){
    mtext(text.4, side = 1, line = 4, adj = 0, outer = TRUE)
  }
  par(op) ## reset parameters
  par(op2) ## reset parameters
  return(ANNUAL)
} ## End of function CpuePlot


