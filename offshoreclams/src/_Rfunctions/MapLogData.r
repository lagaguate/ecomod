
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


