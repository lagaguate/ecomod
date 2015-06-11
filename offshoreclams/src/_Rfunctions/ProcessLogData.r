
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



