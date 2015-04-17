
  lobster.db = function( DS="complete.redo") {


    fn.root =  file.path( project.datadirectory("lobster"), "data", "ODBCDump")
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    
    if (DS %in% c("complete.redo", "complete") ) {

      if (DS=="complete") {
        fl = list.files( path=fn.root, pattern="*.rdata", full.names=T ) 
        for ( fny in fl ) {
          load (fny, .GlobalEnv)
        }
      }
      if (DS=="complete.redo") {
        # ODBC data dump of lobster data
        lobster.db( DS="logs.redo")
        lobster.db( DS="atSea.redo")
        lobster.db( DS="cris.redo")
        lobster.db( DS="port.redo")
        lobster.db( DS="fsra.redo")
        lobster.db( DS="scallop.redo")
      }
    }

    ## Commercial Logs and slips
    if (DS %in% c("logs.redo", "logs") ) {

     if (DS=="logs.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # logs
        logs = sqlQuery(con, "select * from marfissci.lobster_sd_log")
        save( logs, file=file.path( fn.root, "logs.rdata"), compress=T)
       
        # slips
        slips = sqlQuery(con, "select * from marfissci.lobster_sd_slip")
        save( logs, file=file.path( fn.root, "slip.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load (file.path( fn.root, "slip.rdata"), .GlobalEnv)
      load (file.path( fn.root, "logs.rdata"), .GlobalEnv)
      
    }

    ## At Sea sampling from Cheryl's view
    if (DS %in% c("atSea.redo", "atSea") ) {

     if (DS=="atSea.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # atSea
        atSea = sqlQuery(con, "select * from FRAILC.LOBSTER_ATSEA_VW")
        save( atSea, file=file.path( fn.root, "atSea.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fn.root, "atSea.rdata"), .GlobalEnv)
     }

    ## port sampling 
    if (DS %in% c("port.redo", "port") ) {

     if (DS=="port.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # port
        port = sqlQuery(con, "select a.SAMPLE_SEQ,a.SAMPLE_NO,a.SDATE,a.SEASON,a.NTRAPS,a.LATITUDE,a.LONGITUDE,a.GRADE, b.L_SIZE,b.N_MALES,b.N_FEM,b.NBF, c.LFA,c.PORT,c.COUNTY,c.STAT,c.PORT_CODE,c.LATITUDE port_lat,c.LONGITUDE port_lon from lobster.CRLENGCODE a, lobster.CRLENGFREQ b, lobster.CRLOCATIONS c where a.sample_seq = b.sample_seq and a.port = c.port and a.type = 'P' ")
        save( port, file=file.path( fn.root, "port.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fn.root, "port.rdata"), .GlobalEnv)
     }
  
  ## voluntary logs 
    if (DS %in% c("vlog.redo", "vlog") ) {

     if (DS=="vlog.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # vlog
        vlog = sqlQuery(con, "select a.FDATE,a.N_TRP,a.W_TOT,a.FCODE,a.N_L,a.W_AVG,a.PORT,a.CPTH,a.NBF,a.SEASON,a.W_C,a.CPTH_C, b.LFA,b.COUNTY,b.STAT,b.PORT_CODE,b.LATITUDE,b.LONGITUDE,b.COMMENTS from lobster.CRLOGDATA a, lobster.CRLOCATIONS b where a.port = b.port")
        save( vlog, file=file.path( fn.root, "vlog.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fn.root, "vlog.rdata"), .GlobalEnv)
     }

    ## CRIS database
    if (DS %in% c("cris.redo", "cris") ) {

     if (DS=="cris.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # cris
        cris.trips = sqlQuery(con, "select * from cris.crtrips")
        save( cris.trips, file=file.path( fn.root, "crisTrips.rdata"), compress=T)
        cris.traps = sqlQuery(con, "select * from cris.crtraps")
        save( cris.traps, file=file.path( fn.root, "crisTraps.rdata"), compress=T)
        cris.samples = sqlQuery(con, "select * from cris.crsamples")
        save( cris.samples, file=file.path( fn.root, "crisSamples.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fn.root, "crisTrips.rdata"), .GlobalEnv)       
      load(file.path( fn.root, "crisTraps.rdata"), .GlobalEnv)       
      load(file.path( fn.root, "crisSamples.rdata"), .GlobalEnv)       
     }
  
    ## FSRS traps 
    if (DS %in% c("fsrs.redo", "fsrs") ) {

     if (DS=="fsrs.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # fsrs
        fsrs = sqlQuery(con, "select * from fsrs_lobster.FSRS_LOBSTER_VW")
        save( fsrs, file=file.path( fn.root, "fsrs.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fn.root, "fsrs.rdata"), .GlobalEnv)
     }

     ## lobster catch from scallop survey traps 
    if (DS %in% c("scallop.redo", "scallop") ) {

     if (DS=="scallop.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # scallop
        scallop.catch = sqlQuery(con, "select * from SCALLSUR.SCBYCATCHES")
        save( scallop.catch, file=file.path( fn.root, "scallopCatch.rdata"), compress=T)
        scallop.tows = sqlQuery(con, "select * from SCALLSUR.SCTOWS")
        save( scallop.tows, file=file.path( fn.root, "scallopTows.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fn.root, "scallopCatch.rdata"), .GlobalEnv)
      load(file.path( fn.root, "scallopTows.rdata"), .GlobalEnv)
    }
  }



