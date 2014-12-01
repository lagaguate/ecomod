

load.marport.rawdata = function( fn, fncfg ) {

  require(lubridate)

  # fn="/home/jae/Downloads/net/Alfred\ Needler-Ned_2013_002-045"
  # cfgfn = "/home/jae/Downloads/net/Config_Old.txt"

  cfg = parse.marport.config( fncfg )

  gps = read.table( paste(fn, "gps", sep="."), sep=",", as.is=TRUE, header=FALSE )
  sgp = read.table( paste(fn, "sgp", sep="."), sep=",", as.is=TRUE, header=FALSE )
  dlog = read.table( paste(fn, "log", sep="."), sep=",", as.is=TRUE, header=FALSE )
  
  names(gps) = c("Vessel", "Cruise", "notsure", "timestamp", "latitude", "longitude" )
  names(sgp) = c("Vessel", "Cruise", "notsure",  "timestamp", "sensor", "value" )
  names(dlog) =  c("Vessel", "Cruise", "notsure", "timestamp", "event" )

  sgp = sgp[ , c( "timestamp", "sensor", "value" ) ]
  sgp$sensor = as.character( sgp$sensor)
  
  sensors = data.frame( id= sort( unique( sgp$sensor ) )) 
  sensors = merge(sensors, cfg, by.x="id", by.y="sensorid", all.x=TRUE, all.y=FALSE )
  
  marport = gps
  rm(gps)
  gc()

  for (i in 1:nrow(sensors)) {
    sdata = NULL
    sdata = sgp[  which( sgp$sensor == sensors$id[i] ) ,]
    newvariablename = sensors$variable[i]
    names(sdata) = c( "timestamp", "sensor", newvariablename )
    marport = merge( marport, sdata[, c("timestamp", newvariablename )], by="timestamp", all.x=TRUE, all.y=FALSE )
  }

  rm(sgp)
  gc()

  marport$timestamp = mdy_hms( marport$timestamp) ## need to check if mdy or dmy ...
  dlog$timestamp = mdy_hms(dlog$timestamp) 
   
  marport$id = fn
  marport$timestamp = timestamp.fix ( marport$id, marport$timestamp, threshold.hrs=2 )
   
  return(marport)
}

