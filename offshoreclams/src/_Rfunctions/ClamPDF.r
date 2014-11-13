################################################################################
## Script : ClamPDFversion4.r
################################################################################
# This is R code for connecting to the Offshore Clam Oracle database and
# producing commercial indices of the Arctic Surfclam stock status in relation
# to "trigger levels" that would indicate a closer look at the stock status
# is warranted.
# The script is presently set up to do Banquereau and Grand Bank, 
# in a main fuction LoopThroughPlots(Sel.Bank, fig.num, log.data) with 
# variable Sel.Bank = 1 or 2 to select which bank,
# Sel.Bank <- 1 is for Banquereau, Sel.Bank <- 2 is for Grand Bank
################################################################################
# Required libraries (called as needed)
# ------------------------------------------------------------------------------
# tcltk      - used for password function
# RODBC      - used for database connection
# PBSMapping - used for map drawing
# reshape    - used for cumulative sums
# Plotrix    - used for multiple plots per page
# grid       -
# gridExtra  - 
################################################################################
################################################################################
# Function definitions
################################################################################
MakeConnection <- function(){
  # Load RODBC Library
  require(RODBC)
  # Now get data from database
  # Make connection to Offshore Clam Oracle database on PTRAN 
  #   Note: Have to have previously setup Clam_DB in System DSN
  #   You only have to set this up once:
  #   Go to Start\Programs\Administrative tools\Data Sources (ODBC) 
  #   Select "System DSN" tab, click "Add", in the window that pops up  
  #   scroll down to "Oracle in OraClient10g2",
  #   click Finish, "Oracle ODBC Driver Configuration" window pops up,
  #   in "TSN Service Name" tab scroll down to PTRAN; put in your User ID 
  #   for Oracle, put in a Data Source Name that you will 
  #   call from R and add a description if you want.  
  #   When you test the connection it will ask for your password.
  #   Once this is set up and you can make the connection click OK
  #   You are now ready to connect from R
  ##############################################################################
  
  #MMM
  RODBCconn <-odbcConnect(oracle.dsn, uid = oracle.clam.user, pwd = oracle.clam.password)
  return(RODBCconn)
}
################################################################################
GetLogData <- function(RODBCconn){
  log.data <- sqlQuery(RODBCconn, "SELECT * FROM Dale_log_cpue2")
 return(log.data)
}
################################################################################
#### Function for password protection
################################################################################
##getPass = function(){
##  require(tcltk);
##  wnd=tktoplevel();tclVar("")->passVar;#Label
##  tkgrid(tklabel(wnd,text="Enter Password:"));
##  # Password box
##  tkgrid(tkentry(wnd,textvariable=passVar,show="*")->passBox);
##  # Hitting return will also submit password
##  tkgrid(tkbutton(wnd,text="OK",command=function() tkdestroy(wnd)));
##  # Wait for user to clik OK
##  tkwait.window(wnd);
##  password=tclvalue(passVar);
##  return(password);
##}
################################################################################
GetFinalYear = function(){
  require(tcltk);
  wnd=tktoplevel();tclVar("")->yearVar;#Label
  tkgrid(tklabel(wnd,text="Enter Last Year to Include:"));
  # Input Year box
  yearVar <- tclVar(as.character(format(Sys.Date(),'%Y')))
  tkgrid(tkentry(wnd,textvariable=yearVar)->yearBox);
  tkgrid(tkbutton(wnd,text="OK",command=function() tkdestroy(wnd)));
  # Wait for user to clik OK
  tkwait.window(wnd);
  final.year=tclvalue(yearVar);
  return(as.integer(final.year));
}

################################################################################
ProcessLogData <- function(log.data){
  ## ADD Year FIELD TO MAKE THINGS EASIER TO MANIPULATE
  log.data$Year <- as.integer(format(log.data$RECORD_DATE, '%Y'))  ## Add Year
  ## Change NAs in N_TOWS and ROUND CATCH to 0s, usually valid 0's
  log.data$N_TOWS[is.na(log.data$N_TOWS)] <- 0  
  log.data$ROUND_CATCH[which(is.na(log.data$ROUND_CATCH))] <- 0  
  ## Assign fishing area, currently Banquereau (in NAFO 4Vsc = 1), 
  ##                                Grand Bank (in 3L,3O or 3N = 2) or 
  ##                                Outside (=0)
  ## Consider trips that were actually early surveys covering Scotian Shelf
  ##############################################################################
  log.data$BANK <- rep(0,dim(log.data)[1])
  log.data$BANK[which(log.data$NAFO %in%  c("4VSC",'4VS'))] <- 1
  log.data$BANK[which(log.data$NAFO  %in% c("3L","3O","3N"))] <- 2
  ## TOWING SPEED and TIME are fairly constant so for missing values fill 
  ## the average values for that vessel-trip-subtrip
  ##
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
CpuePlot <- function(fig.num, log.data, Sel.Bank)
{
  ##############################################################################
  ## Plot of CPUE for last 4 active vessels
  par(
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
                   "Surfclam fishery on ")
  text.2 <- paste( bank.txt,
                   ". Symbols are coloured by vessel, dots are CPUE by trip,",
                  "and lines ", sep = "")
  text.3 <- "connect annual averages for vessels."
  par(xpd = NA)  ## no clipping region
  mtext(text.1, cex = 1, side = 1, line = 1, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 2, adj = 0, outer = TRUE)
  mtext(text.3, side = 1, line = 3, adj = 0, outer = TRUE)
  return(ANNUAL)
} ## End of function CpuePlot
################################################################################
## Now look at area impacted as number of one minute squares fished
################################################################################
OMSFished <- function(fig.num, BBData, Sel.bank)
{
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
  # mtext("Number of One Minute Squares Fished by Year", side = 3, line = 1)
  ##############################################################################
  ## Add text below Figure
  text.1 = paste("Figure ", as.character(fig.num),
                 ". Number of one minute squares fished by the offshore clam",
                 "fishery by year ")
  text.2 = paste("on ",bank.txt, ".",sep = "")
  par(xpd = NA)  ## no clipping region
  mtext(text.1, side = 1, line = 1, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 2, adj = 0, outer = TRUE)
  return(oms.freq)
} ## end of function OMSFished
################################################################################
## catch and effort table, still using same data
################################################################################
CatchTable <- function(fig.num, BBData, Sel.Bank)
{
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
}
################################################################################
## Look at footprint of the fishery, still using same data
################################################################################
Footprint <- function(fig.num, BB_Footprint, Sel.Bank){
  ## plot catch and footprint
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
  par(new = TRUE)                           ## reset for overlay plot
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
  # mtext("Logged catch and Footprint for the Offshore Clam Fishery",
  #      side = 3, line = 1.)  ## Add Main title
  ##

  mtext(bquote(paste("Figure ", .(fig.num),
                     ". Footprint (k", m^2, ") of the offshore clam fishery ",
                     "by Year on ",.(bank.txt), sep = "")),
        side = 1, line = 0, adj = 0, outer = TRUE)
  
#MMM
  #  return(out.table)
} ## end of function Footprint
################################################################################
## Now Map last 12 months log data  
## -------- Doing 3 years for now as database not up to date ---------
################################################################################
MapLogData <- function(fig.num, log.data, Sel.Bank)
{
  today <- (Sys.Date())
  par(
    omi <- c(2, 1, 1, 1),      ## Set outer margins  
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
  cex <- rep(0.3, ln)
  ## Now draw map of Banquereau, add NAFO lines and labels (nafo = 'all'),
  ## add Bank labels (banks = T)
  Points_Par <- data.frame(PID, pch = I(pch), col = I(col), cex = I(cex))
  attr(Points_Par, "projection") <- "LL"
  #source("F:/Dale/R_Maps/ClamMap.r", local = T)
  #source("R/OffshoreClams/ClamMap.r", local = T)
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
                  " for the last three years")
  text.2 <- paste("on ", bank.txt, ".  Years are plotted in order ",
                  as.character(max.data.year)," is red,",
                  as.character(max.data.year - 1), "is green and ",
                  as.character(max.data.year - 2))
  text.3 <- "is yellow, with previous years covering recent years."
  mtext(text.1, side = 1, line = 1, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 2.4, adj = 0, outer = TRUE)
  mtext(text.3, side = 1, line = 3.8, adj = 0, outer = TRUE)
} ## End of function MapLogData
################################################################################

PlotPercentLarge <- function(fig.num, lf.data, Sel.Bank,c.size, sel.txt)
{
  ##############################################################################
  ## Calculate and plot percentage of clams over 105 or 120 mm in the 
  ##  commercial samples
  ##############################################################################
  old.par <- par() ## save parameters
  par(
    omi = c(6, 1, 0.5, 1.25),      ## Set outer margins 
    mai = c(1, .75, 0.5, 0.5)  ## Set Figure margins (inches)
  )  
  ##---------------------------------------------------
  ge.txt <- "\u2265"  ## >= symbol
  if(Sel.Bank == 1){
    len.data <- subset(lf.data, lf.data$AREA == 4)
    y.limit <- c(0, 15)
    t.level <- 1
    #c.size <- 120
    bank.txt <- "Banquereau Bank"
    #sel.txt <- "120"
    trigger.x.pos = 1999
    lowess.y.pos = 4
  }else if(Sel.Bank == 2){
    len.data <- subset(lf.data, lf.data$AREA == 3)
    y.limit <- c(-4, 50)
    t.level <- 0.5
    #c.size <- 105
    #sel.txt <- "105"
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
       col = "red", lty = 2, xlab = "Year",
       ylab = bquote(paste("Percent Shell Length" >= .(sel.txt),"mm"))) 
  par(xpd = NA)
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
  par(old.par)
} ## end of function PlotPercentLarge
################################################################################
## Plot of length frequencies
################################################################################
PlotLengthFreq <- function(fig.num, P120, p_mat, bank.txt, table, c.size, sel.txt){
  yy <- as.integer(names(P120))[seq(1, length(P120))]
  y.lab.pos <- as.integer(length(yy) / 2) + 1
  old.par <- par() ## save parameters
  ## set up for series of plots on page
  par(
    mfrow = c(length(yy), 1), 
    mar = c(0, 5, 0 , 1),
    omi = c(3, 1, 1.5, 1)
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
                  " commercial catches")
  text.2 <- paste("on ", bank.txt, ".", sep = "")
  mtext(text.1, side = 1, line = 5, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 6.5, adj = 0, outer = TRUE)
  ## reset parameters
  par <- old.par  ## reset parameters
} ## end of PlotLengthFreq function
################################################################################
FormatTable <- function(in.table, title1.text, title2.text, h.offset, new.page)
{
  table <- tableGrob(in.table, cols = colnames(in.table), show.colnames = TRUE,
                     show.rownames = FALSE, col.just = "center", 
                     core.just = "right")
  if(new.page == TRUE) {
    grid.newpage()
  }
  h <- grobHeight(table)
  w <- grobWidth(table)
 # title.text <- rbind(title.text, "     ") ## blank line after title line
  title1 <- textGrob(title1.text, y = unit(0.5, "npc") + 0.5  *
                    h + unit(.5, "in"), # to raise height
                     x = unit(0.0, "npc") + unit(h.offset, "in"), vjust = 0,
                     hjust = 0, gp = gpar(fontsize = 12))
  title2 <- textGrob(title2.text, y = unit(0.5, "npc") + 0.5  *
                       h + unit(.25, "in"), # to raise height
                     x = unit(0.0, "npc") + unit(h.offset, "in"), vjust = 0,
                     hjust = 0, gp = gpar(fontsize = 12))
  gt <- gTree(children = gList(table, title1, title2))
  grid.draw(gt)
} ## End of function FormatTable
################################################################################
PlotTitlePage <- function(Sel.Bank){
  ## Set up title page
  old.par <- par() ## save parameters
  par(
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
  par <- old.par  ## reset parameters
} ## End of function PlotTitlePage
################################################################################
PlotAnnexPage <- function(){
  ## Set up title page
  old.par <- par() ## save parameters
  par(
    par(
      mfrow = c(1,1),
      omi = c(5, 1, 1, 1),      ## Set outer margins with space at bottom
      mai = c(1, 1.0, 0.25, 0.25)  ## Set Figure margins (inches)
    )  
  )  
  plot(0:10, 0:10, type = "n", axes = FALSE, ann = FALSE)
  text.1 <- "Annex 1, Revised figures for public distribution"
  text(5, 5, text.1, cex = 1.2)
  par <- old.par  ## reset parameters
} ## End of function PlotAnnexPage
################################################################################
LoopThroughPlots <- function (Sel.Bank, fig.num, log.data){
  ##############################################################################
  old.par <- par() ## save parameters
  par(
    mfrow = c(1,1),
    omi = c(5, 1, 1, 1),      ## Set outer margins with space at bottom
    mai = c(1, 1.0, 0.25, 0.25)  ## Set Figure margins (inches)
  )  
  ##############################################################################
  if(Sel.Bank == 1){
    bank.txt <- "Banquereau Bank"
    c.size=120
    sel.txt="120"
  }else if(Sel.Bank == 2){
    bank.txt <- "Grand Bank"
    c.size=105
    sel.txt="105"
  }
  ANNUAL <- CpuePlot(fig.num, log.data, Sel.Bank)   
  fig.num <- fig.num + 1
  BBData <- subset(log.data, log.data$BANK == Sel.Bank) ## subset selected bank
  out.table <- CatchTable(fig.num, BBData, Sel.Bank)
  ## Now output table of catch and effort
  table.1 <- out.table ## save numeric table as formatting will convert to text
  table.1[,2] <- prettyNum(table.1[,2], big.mark = ",")
  title1.text <- paste("Table ", table.num, 
                       ". Catch (t), Footprint (Area dredged km",
                       "\u00B2","), CPUE and percent of Survey ", sep = "")
  title2.text <- paste("area by year for the offshore clam fishery on ", 
                       bank.txt, ".", sep = "")
  FormatTable(table.1, title1.text, title2.text, 1.0, new.page = TRUE)
  table.num <- table.num + 1
  ## Plot of footprint by year
  Footprint(fig.num, out.table[,c(1, 2, 3)], Sel.Bank)
  fig.num <- fig.num + 1
  # plot of number of one minute squares fished per year
  oms.freq <- OMSFished(fig.num, BBData, Sel.bank)
  fig.num <- fig.num + 1
  ## Add table of number of one minute squares fished by year
  
  oms.freq[, 2] <- prettyNum(oms.freq[, 2], big.mark = ",")
  title1.text = paste("Table ", table.num, ".  Number of one minute squares",
                      "fished per year in the offshore clam")
  title2.text = paste("fishery on ", bank.txt, ".", sep = "")
  FormatTable(oms.freq, title1.text, title2.text, 1.0, new.page = TRUE)
  table.num <- table.num + 1
  ## Plot map of distribution of fishing effort
  MapLogData(fig.num, log.data, Sel.Bank)
  fig.num <- fig.num + 1
  ## plot percent of large clams in catch
  Result <- PlotPercentLarge(fig.num, lf.data, Sel.Bank, c.size, sel.txt)
  out.table2 <- Result$Table
  fig.num <- fig.num + 1
  
  ## Table of percent large clams in catch
  table.3 <- cbind(as.numeric(rownames(out.table2)), out.table2)
  table.3[, 3] <- round(table.3[, 3], 2)
  table.3[, 2] <- prettyNum(table.3[, 2], big.mark = ",")
  title1.text =  ""
  title2.text = paste("Table ", table.num, ". Percent of large Arctic",
                      " Surfclams in unsorted catch on ", bank.txt, ".",
                      sep = "")
  ## new viewport for table
  pushViewport(viewport(x = unit(0.5, "npc"), y = unit(0.25, "npc"),
                        width = unit(5.5, "in"), height = unit(5, "in"),
                        just = "centre", gp = gpar(), clip = "inherit",
                        xscale = c(0, 1), yscale = c(0, 1),angle = 0))
  FormatTable(table.3, title1.text, title2.text, -0.3, new.page = FALSE)
  table.num <- table.num + 1 
  ## Plot the length frequencies
  PlotLengthFreq(fig.num, Result$P120, Result$p_mat, bank.txt, Result$Table, c.size, sel.txt)
  fig.num <- fig.num + 1
  par <- old.par  ## reset parameters
  return(ANNUAL)
} ## End of function LoopThroughPlots
################################################################################
AnnexCpuePlot <- function(fig.num, Ann.data, Sel.Bank)
{
  ##############################################################################
  ## Annex: Plot of Annual average CPUE for last 4 active vessels
  par(
    mfrow = c(1,1),
    omi = c(5, 1, 1, 1),      ## Set outer margins with space at bottom
    mai = c(1, 1.0, 0.25, 0.25)  ## Set Figure margins (inches)
  )
#June 2014 - P. Hurley wanted Annual average only - not normalized  
##  max.cpue <- max(Ann.data$CPUE)
##  Ann.data$CPUE <- Ann.data$CPUE / max.cpue   ## normalize CPUE to Maximum
##  ## matrix and Title for plotting already exists as L4
##  plot(Ann.data[, c("Year", "CPUE")],
##       xlim = range(Ann.data$Year) + c(-1, 1),
##       ylim = c(0, 1.1), pch = 20, 
##       col = 'black',
##       xlab = "Year", ylab = "Normalized CPUE", axes = FALSE,
##       ann = TRUE, xaxt = "n")
##  axis(2)
##  
##  axis.POSIXct(Ann.data$Year, side = 1, format = "%Y", labels = TRUE,
##               xaxs = "i", tcl = -0.5) #tics & labels
##  r <- range(Ann.data$Year) + c(-60*60*24*365,60*60*24*365) 
##  axis.POSIXct(1, at = levels(cut(r, "years")), format = "%Y", tcl = -0.25,
##               labels = FALSE) ## minor tic marks
##  box()
##    ## Now do overall annual CPUE
##  lines(Ann.data[ , c("Year", "CPUE")], pch = 20, col = "black")
##  ## Draw dashed black line for proposed lower CPUE limit
##  if(Sel.Bank == 1){
##    cpue.trigger = 70/ max.cpue
##    bank.txt = "Banquereau Bank"
##  }else if(Sel.Bank == 2){
##    cpue.trigger = 50/ max.cpue
##    bank.txt = "Grand Bank"
##  }
##  lines(matrix(c(range(Ann.data$Year), 
##                 c(cpue.trigger, cpue.trigger)), 2, 2), col = "black",
##        lty = 2) 
##  text(as.POSIXct("1991", format = "%Y"), cpue.trigger,
##       "Trigger level for CPUE", col = "black", pch = 20, pos = 4)
##  ## Add text below Figure
##  text.1 <- paste("Figure", as.character(fig.num), 
##                  ". Normalized (to maximum value) Annual CPUE for the last four",
##                  " vessels active ")
##  text.2 <- paste("in the Arctic Surfclam fishery on ", bank.txt, ".", sep = "")
#Annual average starts here
  Ann.data$CPUE <- Ann.data$CPUE 

  ## matrix and Title for plotting already exists as L4
  plot(Ann.data[, c("Year", "CPUE")],
       xlim = range(Ann.data$Year) + c(-1, 1),
       ylim = c(0, 300), pch = 20, 
       col = 'black',
       xlab = "Year", ylab = expression(paste("CPUE g/",m^2)), axes = FALSE,
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
    cpue.trigger = 70
    bank.txt = "Banquereau Bank"
  }else if(Sel.Bank == 2){
    cpue.trigger = 50
    bank.txt = "Grand Bank"
  }
  lines(matrix(c(range(Ann.data$Year), 
                 c(cpue.trigger, cpue.trigger)), 2, 2), col = "black",
        lty = 2) 
  text(as.POSIXct("1991", format = "%Y"), cpue.trigger,
       "Trigger level for CPUE", col = "black", pch = 20, pos = 4)
  ## Add text below Figure
  text.1 <- paste("Figure", as.character(fig.num), 

                  ". Annual Average CPUE for the last four vessels active",
                  " in the Arctic Surfclam fishery on ")
  text.2 <- paste( bank.txt, sep = "")
 #done with replaced annual average.
  par(xpd = NA)  ## no clipping region
  mtext(text.1, cex = 1, side = 1, line = 1, adj = 0, outer = TRUE)
  mtext(text.2, side = 1, line = 2, adj = 0, outer = TRUE)
} ## End of function AnnexCpuePlot
################################################################################
## End of function definitions
################################################################################
################################################################################
################################################################################
## Set up pdf output
################################################################################
## Construct file path for desktop
# home = setwd(Sys.getenv("HOME")) ## Get current home directory
# fpath = file.path(home, "Desktop", "testpdf2.pdf", fsep = .Platform$path.sep) 
## This sets up to place the final pdf file ofn the desktop of the computer 
## running the script
## Note: This would not be good for running teh script on a server
final.year <- GetFinalYear() ## select cut off year for display
#home = Sys.getenv("USERPROFILE")
## Construct file path for desktop
#fpath = file.path(home, "Desktop", paste("OffShoreClams_",final.year,".pdf",sep=""), fsep = "\\") 
#fpath = file.path(getwd(),"R","OffshoreClams")
#MMM - Sep 2014 adjusted path for ecomod
fpath = file.path( project.directory("offshoreclams"), "docs")
fpath = fpath=paste(fpath, "/OffShoreClams_",final.year,".pdf",sep="")
pdf(file = fpath,
    ## onefile = "TRUE",
    height = 11.0,
    width = 8.5,
    title = "Offshore Surfclam fishery Indices for Monitoring",
    ##paper = 'special'
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
ANNUALBB <- LoopThroughPlots(Sel.Bank, fig.num, log.data) 
Sel.Bank <- 2
fig.num <- 7
table.num <- 4
## process main loop for Grand Bank
ANNUALGB <- LoopThroughPlots(Sel.Bank, fig.num, log.data) 
PlotAnnexPage()
AnnexCpuePlot(1,ANNUALBB,1)
AnnexCpuePlot(7,ANNUALGB,2)
odbcClose(RODBCconn) ## Close database connection
dev.off()            ## Close pdf file


