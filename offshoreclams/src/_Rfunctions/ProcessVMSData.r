
ProcessVMSData <- function(vms.data){
  
  ##############################################################################
  ## Do initial processing, fill in some missing values, 
  ##############################################################################

  names(vms.data) <- tolower(names(vms.data))
  vms.data$vmsdate <- as.POSIXct(vms.data$vmsdate,tz="GMT")  # VMS data is in UTC, assign timezone
  vms.data <- vms.data[order(vms.data$vrn, vms.data$vmsdate), ]  # Order dataframe by vrn and DateTime 
  
  ########################################
  # Clean VMS data selected from VMS_pos #
  ########################################
  
  # Shift vmsdate with a duplicate within vrn by one second (duplicate record is moved one second forward)
  vms.data$vmsdate.adj <- adjust.duplicateTimes(vms.data$vmsdate, vms.data$vrn)
  # Create date and time variables in local time
  vms.data$date <- format(strftime(vms.data$vmsdate.adj,format="%Y-%m-%d"), tz="America/Halifax",usetz=TRUE)
  vms.data$time <- format(strftime(vms.data$vmsdate.adj,format="%H:%M:%S"), tz="America/Halifax",usetz=TRUE)
  vms.data$year <- format(strftime(vms.data$vmsdate.adj,format="%Y"), tz="America/Halifax",usetz=TRUE)
  vms.data$vmsdatelocal <- as.POSIXct(paste(vms.data$date, vms.data$time), format="%Y-%m-%d %H:%M:%S",tz="America/Halifax")
  vms.data <- vms.data[!duplicated(vms.data),] # Removes any rows that are fully duplicated
  
  ########################################	
  # Cross against logs to pull out trips #
  ########################################	
  
  clam.log <- data.frame(log.data[,c("cfv","date")])
  clam.log <- unique(clam.log) #Unique vrn and date fished from logs
  
  # Select VMS data that has matching VRN and DATE from logs #
  vms.log <- merge(vms.data, clam.log, by.x = c("vrn", "date"), by.y = c("cfv","date")) #Creates dataframe with CLAM VMS ONLY: vms.log
  
  # Select log data that has matching CFV and DATE from logs #
  clam.vms <- data.frame(vms.log[,c("vrn","date")])
  clam.vms <- unique(clam.vms) #Unique vrn and date fished from VMS data

  log.vms <- merge(log.data, clam.vms, by.x = c("cfv", "date"), by.y = c("vrn","date")) #Creates dataframe with CLAM VMS ONLY: vms.log

  #Assign record_no and logrecord_id to vms.log
  vms.data2 <- merge(vms.log,log.vms[,c("logrecord_id","cfv","record_date","start_time","year","record_no","trip_no","logtrip_id","subtrip_no")], by.x = c("vrn", "date", "record_no"), by.y = c("cfv","date","record_no"),all.x=TRUE) 
  
  #Check for outliers in latitude and longitude  
  
  #Calculate distance travelled between points
  
  #Remove watches without effort
  #Note we lose catch data by removing watches without effort since there is a delay
  #log.data <- log.data[log.data$n_tows!=0,]
  
  return(vms.data)

} # end of function ProcessLogData