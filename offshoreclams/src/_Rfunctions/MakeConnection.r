MakeConnection <- function(){
  ##############################################################################
  #Establish connection to database
  ##############################################################################
  require(RODBC)
  RODBCconn <-odbcConnect(oracle.dsn, uid = oracle.clam.user, pwd = oracle.clam.password)
  return(RODBCconn)
}

