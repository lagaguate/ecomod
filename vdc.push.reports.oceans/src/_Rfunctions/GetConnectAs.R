connectAs <- function(dsn, user, pw) {
   connection<- odbcConnect(dsn, uid = user, pwd = pw)
   if(connection == -1) { 
     stop("
         Unable to connect to database.
         Please check your username and password")}
  return(connection)
}
