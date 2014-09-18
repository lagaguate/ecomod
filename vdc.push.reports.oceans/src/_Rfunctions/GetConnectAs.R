GetConnectAsVDC <- function() {
  connection<- odbcConnect(oracle.dsn, uid = oracle.vdc.user, pwd = oracle.vdc.password)
  return(connection)
}
connectAs <- function(schema) {
   connection<- odbcConnect(oracle.dsn, uid = oracle.vdc.user, pwd = oracle.vdc.password)
  return(connection)
}
