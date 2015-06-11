GetLogData <- function(RODBCconn){
  ##############################################################################
  # Get data from database, download whole view "Dale_log_cpue2'
  ##############################################################################
  log.data <- sqlQuery(RODBCconn, "SELECT * FROM Dale_log_cpue2")
 return(log.data)
}

