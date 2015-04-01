MakeConnection <- function(){
  ##############################################################################
  #Establish connection to database
  ##############################################################################
  require(RODBC)
  RODBCconn <-odbcConnect(oracle.dsn, uid = oracle.clam.user, pwd = oracle.clam.password)
  return(RODBCconn)
}

GetLogData <- function(RODBCconn){
  ##############################################################################
  # Get data from database, download whole view "Dale_log_cpue2'
  ##############################################################################
  log.data <- sqlQuery(RODBCconn, "SELECT * FROM Dale_log_cpue2")
 return(log.data)
}


ProcessLogData <- function(log.data){
  ##############################################################################
  ## Do initial processing, assign fishing areas, fill in some missing values, 
  ## set up coordinates for Banquereau and Grand Bank
  ##############################################################################
  ## ADD Year FIELD TO MAKE THINGS EASIER TO MANIPULATE
  log.data$Year <- as.integer(format(log.data$RECORD_DATE, '%Y'))  ## Add Year
  ## Change NAs in N_TOWS and ROUND CATCH to 0s, usually valid 0's
  log.data$N_TOWS[is.na(log.data$N_TOWS)] <- 0  
  log.data$ROUND_CATCH[which(is.na(log.data$ROUND_CATCH))] <- 0  
  ##############################################################################
  ## Assign fishing area, currently Banquereau (in NAFO 4Vsc = 1), 
  ##                                Grand Bank (in 3L,3O or 3N = 2) or 
  ##                                Outside (=0)
  ## Consider trips that were actually early surveys covering Scotian Shelf
  ##############################################################################
  log.data$BANK <- rep(0,dim(log.data)[1])
  log.data$BANK[which(log.data$NAFO %in%  c("4VSC",'4VS'))] <- 1
  log.data$BANK[which(log.data$NAFO  %in% c("3L","3O","3N"))] <- 2
  ##############################################################################
  ## TOWING SPEED and TIME are fairly constant so for missing values fill 
  ## the average values for that vessel-trip-subtrip
  ##############################################################################
  t <- which((is.na(log.data$AVE_TIME) & log.data$N_TOWS > 0))
  if(length(t) > 0) {
    for(i in 1:length(t)) {
      recs <- which((log.data$CFV == log.data[t[i], "CFV"]) & 
                     (log.data$TRIP_NO == log.data[t[i], "TRIP_NO"]) & 
                     (log.data$SUBTRIP_NO == log.data[t[i], "SUBTRIP_NO"]))
      log.data$AVE_TIME[t[i]] <- mean(log.data$AVE_TIME[recs], na.rm = T)
    }
  }
  ## if any remaining fill in by CFV-trip
  t <- which((is.na(log.data$AVE_TIME) & log.data$N_TOWS > 0))
  if(length(t) > 0) {
    for(i in 1:length(t)) {
      recs <- which((log.data$CFV == log.data[t[i], "CFV"]) & 
                     (log.data$TRIP_NO == log.data[t[i], "TRIP_NO"]))
      log.data$AVE_TIME[t[i]] <- mean(log.data$AVE_TIME[recs], na.rm = T)
    }
  }
  ## Now do same for tow speed
  s <- which((is.na(log.data$SPEED) & log.data$N_TOWS > 0))
  if(length(s) > 0) {
    for(i in 1:length(s)) {
      recs <- which((log.data$CFV == log.data[s[i], "CFV"]) & 
                   (log.data$TRIP_NO == log.data[s[i], "TRIP_NO"]) & 
                   (log.data$SUBTRIP_NO == log.data[s[i], "SUBTRIP_NO"]))
      log.data$SPEED[s[i]] <- mean(log.data$SPEED[recs], na.rm = T)
    }
  }  
  ## if any remaining fill in by CFV-trip
  s <- which((is.na(log.data$SPEED) & log.data$N_TOWS > 0))
  if(length(s) > 0) {
    for(i in 1:length(s)) {
      recs <- which((log.data$CFV == log.data[s[i], "CFV"]) & 
                   (log.data$TRIP_NO == log.data[s[i], "TRIP_NO"]))
      log.data$SPEED[s[i]] <- mean(log.data$SPEED[recs], na.rm = T)
    }
  }
  ## Recalculate AREA with some NA's replaced in SPEED and AVE_TIME, 
  ## use this instead of original AREA_TOWED
  log.data$AREA <- (log.data$SPEED * 1000 * log.data$AVE_TIME * 
                      log.data$B_WIDTH * log.data$N_TOWS / 60.0)
  log.data$AREA[which(is.na(log.data$AREA))] = 0  ## change AREA NA's to 0,
  ## Set dome global parameters
  Min_lat <<- c(44.0, 43.0)        ## First set of values for Banquereau
  Max_lat <<- c(45.25, 46.5)       ## Second set for Grand Bank
  Min_long <<- c(-60.083, -51.5)
  Max_long <<- c(-57.0, -48.5)
  Area <<- c(10908.1, 49473.0)
  return(log.data)
} # end of function ProcessLogData
################################################################################
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

CatchTable <- function(fig.num, BBData, Sel.Bank){
  ##############################################################################
  ## catch and effort table, still using same data
  ##############################################################################
  temp <- aggregate(cbind(ROUND_CATCH / 1000, AREA  /1000000) ~Year, 
                            data = BBData, sum)
  ## fill in missing years
  yy <- seq(min(temp[, 1]), max(temp[, 1]))
  table.1 <- cbind(yy, matrix(rep(0, 2 * length(yy)), length(yy), 2))
  table.1[match(temp[, 1], table.1[, 1]), 2] <- temp[, 2]
  table.1[match(temp[, 1], table.1[, 1]), 3] <- temp[, 3]
  if(Sel.Bank == 1){
    survey.area <- 10908.1 ## area within 100m contour of Banquereau Bank
  }else if(Sel.Bank == 2){
    survey.area <- 49473.0 ## original survey area on Grand Bank
  }
  table.1  <- cbind(table.1, 100.0 * table.1[,3] / survey.area)
  table.1 <- cbind(table.1[,1], round(table.1[,2], 0),
                     round(table.1[,3], 1),
                     rep(0,length(yy)),
                     round(table.1[,4],2))
  idx <- which(table.1[,3] != 0)                  
  table.1[idx,4] <- round(table.1[idx,2] / table.1[idx,3], 0)
  colnames(table.1) <- c("Year", "Logged Catch (t)",
                            paste("Area Dredged (km", "\u00B2", ")", sep = ""),
                            "CPUE", "% Area")  
  return(as.data.frame(table.1))
} ## end of function CatchTable

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

MapLogData <- function(fig.num, log.data, Sel.Bank){
  ##############################################################################
  ## Now Map last 12 months log data  
  ## -------- Doing 3 years for now as database not up to date ---------
  ##############################################################################
  today <- (Sys.Date())
  op <- par(
            xpd = TRUE,
            mfrow = c(1,1),
            omi <- c(1, 1, 1, 1),      ## Set outer margins  
            mai <- c(1.0, 1.0, 0.0, 0)   ## Set figure margins
           )
  ## get last year in log data 
  max.data.year <- max(as.numeric(format(log.data$RECORD_DATE,"%Y")))
  ## Subset data with non NA Latitude and Longitude
  MapData = subset(log.data,log.data$BANK == Sel.Bank
                   & (is.finite(log.data$LAT_DD) & is.finite(log.data$LON_DD))
                   ## time difference in weeks from today, use 3 years as data
                   ## base not up to date
                   & as.numeric(format(log.data$RECORD_DATE,"%Y")) > 
                     max.data.year - 3)
  idx <- sort(as.numeric(format(MapData$RECORD_DATE,"%Y")), index.return = TRUE,
              decreasing = TRUE)
  MapData <- MapData[idx$ix,]
  idx.year.1 <- which(format(MapData$RECORD_DATE,"%Y") == 
                                          as.character(max.data.year - 2))
  idx.year.2 <- which(format(MapData$RECORD_DATE,"%Y") == 
                                          as.character(max.data.year - 1))
  idx.year.3 <- which(format(MapData$RECORD_DATE,"%Y") == 
                                         as.character(max.data.year))
  ln <- dim(MapData)[1] ## save number of records in file
  ## make into data frame for plotting
  fish.points <- data.frame(PID = seq(1, ln),POS = seq(1, ln),  
                            X = MapData$LON_DD, Y = MapData$LAT_DD)
  fish.points <- as.PolyData(fish.points) ## Chage to PolyData for plotting
  attr(fish.points, "projection") <- "LL"
  ## Make polyProps file = Properties file for plotting
  PID <- seq(1, ln)
  pch <- rep(20, ln)
  col <- rep("red", ln)
  col[idx.year.2] <- rep("green", length(idx.year.2))
  col[idx.year.1] <- rep("yellow", length(idx.year.1))
  cex <- rep(0.5, ln)
  ## Now draw map, add NAFO lines and labels (nafo = 'all'),
  ## add Bank labels (banks = T)
  Points_Par <- data.frame(PID, pch = I(pch), col = I(col), cex = I(cex))
  attr(Points_Par, "projection") <- "LL"
  #source("F:/Dale/R_Maps/ClamMap.r", local = T)
  #source(file.path(ecomod.directory,"offshoreclams/src/_Rfunctions/ClamMap.r"), local = T)
  if(Sel.Bank == 1){
    lat.lim <- c(44.0, 45.25)
    long.lim <- c(-60.083, -57.0)
    bank.txt <- "Banquereau Bank"
  }else if(Sel.Bank == 2){
    lat.lim <- c(43.0, 46.5)
    long.lim <- c(-52.0, -48.5)
    bank.txt <- "Grand Bank"
  }
  ClamMap(area = 'custom', ylim = lat.lim, xlim = long.lim, title = '',
          banks = T, nafo = "all", boundries = '', isobath = 'quick',
          points.lst = list(fish.points, polyProps = Points_Par),
          lines.lst = NULL, poly.lst = NULL, image.lst = NULL, 
          color.fun = tim.colors, color.adj = c(1, 100),
          zlim = NA, res = 'high', bathcol = rgb(0, 0, 1, 0.5), grid = NULL)
  # Add figure text
  text.1 <- paste("Figure ", as.character(fig.num), 
                  ". Map of fishing locations for the Offshore Clam fishery",
                  " for the last three years", sep = "")
  text.2 <- paste("on ", bank.txt, ".  Years are plotted in order ",
                  as.character(max.data.year)," is red, ",
                  as.character(max.data.year - 1), " is green and ",
                  as.character(max.data.year - 2), sep = "")
  text.3 <- "is yellow, with previous years covering recent years."
  mtext(text.1, side = 1, line = 1, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 2.4, adj = 0, outer = TRUE)
  mtext(text.3, side = 1, line = 3.8, adj = 0, outer = TRUE)
  par(op) ## reset parameters
} ## End of function MapLogData

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

FormatTable <- function(in.table, title1.text, title2.text, title3.text,
                        h.offset, new.page){
  ##############################################################################
  ## Format data frame and output as nicely formatted table
  ##############################################################################
  table <- tableGrob(in.table, cols = colnames(in.table), show.colnames = TRUE,
                     show.rownames = FALSE, col.just = "center", 
                     core.just = "right")
  if(new.page == TRUE) {
    grid.newpage()
  }
  y.pos <- unit(0.5,"npc") + 0.5 * grobHeight(table)  ## top of table
  x.pos <- unit(0.0,"npc") + unit(h.offset, "in")
  line.height <- unit(c(1,2,3) * 12 * 1.3, "points") ## 12 == fontsize 
  if(title3.text == " "){ line.height <- line.height[c(3,1,2)] }
  # title.text <- rbind(title.text, "     ") ## blank line after title line
  title1 <- textGrob(title1.text, y = y.pos + line.height[3], # to raise height
                     x = x.pos, vjust = 0, hjust = 0, gp = gpar(fontsize = 12)) 
  title2 <- textGrob(title2.text, y = y.pos + line.height[2], # to raise height
                     x = x.pos, vjust = 0, hjust = 0, gp = gpar(fontsize = 12)) 
  title3 <- textGrob(title3.text, y = y.pos + line.height[1], # to raise height
                   x = x.pos, vjust = 0, hjust = 0, gp = gpar(fontsize = 12))
  gt <- gTree(children = gList(table, title1, title2, title3))
  grid.draw(gt)
} ## End of function FormatTable

PlotTitlePage <- function(Sel.Bank){
  ##############################################################################
  ## Set up title page
  ##############################################################################
  op <- par(
            omi = c(1, 1, 1, 1),      ## Set outer margins with space at bottom
            mai = c(5, 0.25, 0.25, 0.25)  ## Set Figure margins (inches)
           )  
  plot(0:10, 0:10, type = "n", axes = FALSE, ann = FALSE)
  text.1 <- "Offshore Surfclam Science Monitoring Program"
  text.2 <- "Commercial Stock Status Indicators for"
  text.3 <- "Arctic Surfclams on"
  text.4 <- "Banquereau Bank and Grand Bank"
  text.5 <- paste("Run on",format(Sys.Date(), "%d %b, %Y"))
  text(5, 5, text.1, cex = 1.2)
  text(5, 4.3, text.2, cex = 1.) 
  text(5, 3.7, text.3, cex = 1.) 
  text(5, 3.1, text.4, cex = 1.) 
  mtext(text.5, side = 1, line = 1, cex = 1., adj = 0, outer = TRUE) 
  par(op) ## reset parameters
} ## End of function PlotTitlePage

PlotAnnexPage <- function(){
  ##############################################################################
  ## Set up title page for appendix
  ##############################################################################
  op <- par(
            omi = c(1, 1, 1, 1),      ## Set outer margins with space at bottom
            mai = c(1, 0.25, 0.25, 0.25)  ## Set Figure margins (inches)
            )  
  plot(0:10, 0:10, type = "n", axes = FALSE, ann = FALSE)
  text.1 <- "Annex 1"
  text(5, 8, text.1, cex = 2.0)
  text.2 <- "Revised figures for public distribution"
  text(5, 7.2, text.2, cex = 2.0)
  par(op) ## reset parameters
} ## End of function PlotAnnexPage

LoopThroughPlots <- function (Sel.Bank, fig.num, table.num, log.data, lf.data){
  ##############################################################################
  ## Function to loop through Banquereau and Grand Bank 
  ## producing plots and tables
  ##############################################################################
  if(Sel.Bank == 1){
    bank.txt <- "Banquereau Bank"
    c.size <- 120
  }else if(Sel.Bank == 2){
    bank.txt <- "Grand Bank"
    c.size <- 105
  }
  ANNUAL <- CpuePlot(fig.num, log.data, Sel.Bank)   
  fig.num <- fig.num + 1
  BBData <- subset(log.data, log.data$BANK == Sel.Bank) ## subset selected bank
  out.table <- CatchTable(fig.num, BBData, Sel.Bank)
  ## Now output table of catch and effort
  table.1 <- out.table ## save numeric table as formatting will convert to text
  table.1[,2] <- prettyNum(table.1[,2], big.mark = ",")
  title1.text <- paste("Table ", table.num, 
                       ". Catch (t), footprint (area dredged km",
                       "\u00B2","), CPUE and percent of survey ", sep = "")
  title2.text <- paste("area by year for the offshore clam fishery on ", 
                       bank.txt, ".", sep = "")
  title.off <- 1.0
  if(Sel.Bank == 1){
    title3.text <- " "
  } else {
    title3.text <- "Values since 2006 are not representative of stock status."
  }
  FormatTable(table.1, title1.text, title2.text, title3.text, title.off,
              new.page = TRUE)
  table.num <- table.num + 1
  ## Plot of footprint by year
  Footprint(fig.num, out.table[,c(1, 2, 3)], Sel.Bank)
  fig.num <- fig.num + 1
  # plot of number of one minute squares fished per year
  oms.freq <- OMSFished(fig.num, BBData, Sel.Bank)
  fig.num <- fig.num + 1
  ## Add table of number of one minute squares fished by year
  
  oms.freq[, 2] <- prettyNum(oms.freq[, 2], big.mark = ",")
  title1.text = paste("Table ", table.num, ".  Number of one minute squares",
                      "fished per year in the offshore clam")
  title2.text = paste(" fishery on ", bank.txt, ".", sep = "")
  FormatTable(oms.freq, title1.text, title2.text, " ", 1.0, new.page = TRUE)
  table.num <- table.num + 1
  ## Plot map of distribution of fishing effort
  MapLogData(fig.num, log.data, Sel.Bank)
  fig.num <- fig.num + 1
  ## plot percent of large clams in catch
  Result <- PlotPercentLarge(fig.num, lf.data, Sel.Bank)
  out.table2 <- Result$Table
  fig.num <- fig.num + 1
  
  ## Table of percent large clams in catch
  table.3 <- cbind(as.numeric(rownames(out.table2)), out.table2)
  table.3[, 3] <- round(table.3[, 3], 2)
  table.3[, 2] <- prettyNum(table.3[, 2], big.mark = ",")
  title1.text =  " "
  title2.text = paste("Table ", table.num, ". Percent of large Arctic",
                      " Surfclams in unsorted catch on ", bank.txt, ".",
                      sep = "")
  title.off <- 1.0
  if(Sel.Bank == 2){ 
    title3.text =
          " Values since 2006 are not representative of stock status."
  } else {
    title3.text <- " "
  }
  ## new viewport for table on same page
  #pushViewport(viewport(x = unit(0.5, "npc"), y = unit(0.25, "npc"),
  #                      width = unit(5.5, "in"), height = unit(5, "in"),
  #                      just = "centre", gp = gpar(), clip = "inherit",
  #                      xscale = c(0, 1), yscale = c(0, 1),angle = 0))
  FormatTable(table.3, title1.text, title2.text, title3.text, title.off,
                new.page = TRUE)
  table.num <- table.num + 1 
  ## Plot the length frequencies
  PlotLengthFreq(fig.num, Result$P120, Result$p_mat, bank.txt, Result$Table, c.size)
  fig.num <- fig.num + 1
  return(list(ANNUAL = ANNUAL, table.1b = out.table))
} ## End of function LoopThroughPlots

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

LoopThroughAppendix <- function(Sel.Bank, bank.txt, cpue.table, 
                                annual.values, fig.num, table.num, log.data, lf.data){
  ##############################################################################
  ## Loop through banquereau and Grand Bank producing plots and tables for
  ## Appendix
  ##############################################################################
  table.1 <- cpue.table[ ,seq(1,4)]
  table.1[,2] <- prettyNum(table.1[,2], big.mark = ",")
  title1.text <- paste("Table ", table.num, 
                       ". Catch (t), Footprint (Area dredged km",
                       "\u00B2","), and CPUE by year  for the offshore ",
                       sep = "")
  title2.text <- paste("clam fishery on ", bank.txt, 
                       sep = "")
  FormatTable(table.1, title1.text, title2.text, " ", 1.0, new.page = TRUE)
  table.num <- table.num + 1
  AnnexCpuePlot(fig.num, annual.values, Sel.Bank, 0)
  ## AnnexCpuePlot(1, annual.values, 1, 1) ## normalized CPUE plot
  fig.num <- fig.num + 1
  BBData <- subset(log.data, log.data$BANK == Sel.Bank) ## subset selected bank
  out.table <- CatchTable(fig.num, BBData, Sel.Bank)
  Footprint(fig.num, out.table[,c(1, 2, 3)], Sel.Bank)
  fig.num <- fig.num + 1
  ## plot percent of large clams in catch
  Result <- PlotPercentLarge(fig.num, lf.data, Sel.Bank)
  out.table2 <- Result$Table
  fig.num <- fig.num + 1
  
  ## Table of percent large clams in catch
  table.3 <- cbind(as.numeric(rownames(out.table2)), out.table2)
  table.3[, 3] <- round(table.3[, 3], 2)
  table.3[, 2] <- prettyNum(table.3[, 2], big.mark = ",")
  title3.text =  " "
  title2.text =  " "
  title1.text = paste("Table ", table.num, ". Percent of large Arctic",
                      " Surfclams in unsorted catch on ", bank.txt, ".",
                      sep = "")
  if(Sel.Bank == 2){ 
    title3.text <- "Values since 2006 are not representative of stock status."
  }
  ## new viewport for table
  pushViewport(viewport(x = unit(0.5, "npc"), y = unit(0.25, "npc"),
                        width = unit(5.5, "in"), height = unit(5, "in"),
                        just = "centre", gp = gpar(), clip = "inherit",
                        xscale = c(0, 1), yscale = c(0, 1),angle = 0))
  FormatTable(table.3, title2.text, title1.text, title3.text, 1.0,
              new.page = TRUE)
}  ## End of function LoopThroughAppendix


ClamPDF<-function(final.year){
################################################################################
## actually runs the script, calling the functions as needed.
## takes an input value of the final year - e.g. ClamPDF(2015)
################################################################################

## Construct file path for output
# fname = determine name of file to save
# fpath = full path to save the file, in the docs folder of this project (offshoreclams)
fname = paste("OffShoreClams_",final.year,".pdf",sep="")
fpath = file.path( project.datadirectory("offshoreclams"), "docs", fname)
pdf(file = fpath,
    ## onefile = "TRUE",
    height = 11.0,
    width = 8.5,
    title = "Offshore Surfclam fishery Indices for Monitoring",
)
################################################################################
## Load required packages
#-------------------------------------------------------------------------------
library(gridExtra)
require(PBSmapping)
require(reshape)
require(plotrix)
################################################################################
RODBCconn <- MakeConnection()
log.data <- GetLogData(RODBCconn)
log.data <- ProcessLogData(log.data)
## reduce to cut off year
log.data <- log.data[which(log.data$Year <= final.year), ] 
################################################################################
par(                        ## set up initial figure parameters
    mfrow = c(1,1),
    omi = c(5, 1, 1, 1),      ## Set outer margins
    mai = c(1, 1.0, 0.25, 0.25)  ## Set Figure margins (inches)
   )  
################################################################################
PlotTitlePage(Sel.Bank)
fig.num <- 1
table.num <- 1
## note: have already opened database connection and retrieved file "log.data"
##       above
## Get commercial length frequency data from database
lf.data <- sqlQuery(RODBCconn, "SELECT * FROM COM_LEN_FREQ")
lf.data <- lf.data[which(lf.data$YEAR <= final.year), ]
lf.data <- lf.data[which(is.finite(lf.data$SHELL_LEN)), ] ## remove NAs
## omit lengths 200 mm and over, they are measurement errors
lf.data <- lf.data[which(lf.data$SHELL_LEN < 200), ]  
Sel.Bank <- 1
## process main loop for Banquereau
BB.output <- LoopThroughPlots(Sel.Bank, fig.num, table.num, log.data, lf.data) 
Sel.Bank <- 2
fig.num <- 7
table.num <- 4
## process main loop for Grand Bank
GB.output <- LoopThroughPlots(Sel.Bank, fig.num, table.num, log.data, lf.data) 
################################################################################
################################################################################
## now do Appendix with public Figures and Tables
################################################################################
################################################################################
PlotAnnexPage()  ## Appendix cover page
################################################################################
## Banquereau Appendix Figures and Tables
Sel.Bank <- 1
bank.txt <- "Banquereau Bank"
cpue.table <- BB.output$table.1b ## Banquereau CPUE table
annual.values  <- BB.output$ANNUAL ## Banquereau annual CPUE values
fig.num <- 1
table.num <- 1
LoopThroughAppendix(Sel.Bank, bank.txt, cpue.table, 
                                annual.values, fig.num, table.num, log.data, lf.data)
################################################################################
## Now do Grand Bank Figures and Tables
bank.txt <- "Grand Bank"
Sel.Bank <- 2
cpue.table <- GB.output$table.1b ## Grand Bank CPUE table
annual.values  <- GB.output$ANNUAL ## Grand Bank annual CPUE values
fig.num <- 4
table.num <- 3
LoopThroughAppendix(Sel.Bank, bank.txt, cpue.table, 
                    annual.values, fig.num, table.num, log.data, lf.data)
################################################################################
## Finished 
################################################################################
odbcClose(RODBCconn) ## Close database connection
dev.off()            ## Close pdf file
}

#To run the file in ecomod, do the following commands
#
#loadfunctions("offshoreclams")
#loadfunctions("polygons")
#ClamPDF(2015) #or some other final year