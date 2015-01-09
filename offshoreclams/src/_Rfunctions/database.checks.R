database.checks<-function(){
  ###############################################################################
  ## Offshore clam Database checks
  ###############################################################################
  # Load RODBC Library
  require(RODBC)
  RODBCconn <-odbcConnect(oracle.dsn, uid=oracle.clam.user, pwd=oracle.clam.password)
  ## select trips labeled as 2013 or 2014
  RODBCdat <- sqlQuery(RODBCconn, "SELECT * FROM Dale_log_cpue where TRIP_NO > 201300")
  ## Look at view structure
  str(RODBCdat)
  ## Quick Check of latitudes
  table(round(RODBCdat$LAT_DD,0)) ## Table of rounded latitudes
  ## Quick check of Longitudes
  table(round(RODBCdat$LON_DD,0)) ## Table of rounded longitudes
  
  ## install.packages("data.table")
  library(data.table)
  RD <- data.table(RODBCdat)    
  ## sums of tows, blanched raw and whole catch by trip, and min and max speed and tow time  by CFV and trip                                                              
  RD[, j=list(n.tows = sum(N_TOWS, na.rm=TRUE),
              blanched = sum(BLANCHED, na.rm=TRUE),           
              raw = sum(RAW_CLAMS, na.rm=TRUE),
              whole = sum(WHOLE, na.rm=TRUE),
              min.speed = min(SPEED, na.rm=TRUE),
              max.speed = max(SPEED, na.rm=TRUE),
              min.t.time = min(AVE_TIME, na.rm=TRUE),
              max.t.time = max(AVE_TIME, na.rm=TRUE)
  ), by = list(CFV, TRIP_NO)]
}
