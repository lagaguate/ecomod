
  lobster.db = function( DS="complete.redo",yrs=1995:2014) {


  require(lubridate)
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
        lobster.db( DS="vlog.redo")
        lobster.db( DS="fsra.redo")
        lobster.db( DS="scallop.redo")
        lobster.db( DS="survey.redo")
      }
    }

    ## Inshore Commercial Logs and slips
    if (DS %in% c("logs.redo", "logs") ) {

     if (DS=="logs.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # logs
        logs = sqlQuery(con, "select * from marfissci.lobster_sd_log")
        save( logs, file=file.path( fn.root, "logs.rdata"), compress=T)
       
        # slips
        slips = sqlQuery(con, "select * from marfissci.lobster_sd_slip")
        save( slips, file=file.path( fn.root, "slip.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load (file.path( fn.root, "slip.rdata"), .GlobalEnv)
      load (file.path( fn.root, "logs.rdata"), .GlobalEnv)
      
    }

    ## Offshore Commercial Logs
    if (DS %in% c("logs41.redo", "logs41") ) {

     if (DS=="logs41.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
        query41<-"select b.mon_doc_id, b.vr_number, b.vessel_name, b.captain, b.licence_id, 
                b.FV_FISHED_DATETIME, 
                round((((b.ENT_LATITUDE/100/100-TRUNC(b.ENT_LATITUDE/100/100))*100)/60)+TRUNC(b.ENT_LATITUDE/100/100),4) DDLAT,
                round((((b.ENT_LONGITUDE/100/100-TRUNC(b.ENT_LONGITUDE/100/100))*100)/60)+TRUNC(b.ENT_LONGITUDE/100/100),4) DDLON,
                b.NUM_OF_TRAPS, b.EST_WEIGHT_LOG_LBS, 
                b.EST_WEIGHT_LOG_LBS*a.ratio adjcatch,
                case        when mflib.simplepoly.inside((((b.ENT_LATITUDE/100/100-TRUNC(b.ENT_LATITUDE/100/100))*100)/60)+TRUNC(b.ENT_LATITUDE/100/100), 
                -1*((((b.ENT_LONGITUDE/100/100-TRUNC(b.ENT_LONGITUDE/100/100))*100)/60)+TRUNC(b.ENT_LONGITUDE/100/100)), 'CROWELL_BASIN_EXT')>0
                            then 'CROWELL'
                            when mflib.simplepoly.inside((((b.ENT_LATITUDE/100/100-TRUNC(b.ENT_LATITUDE/100/100))*100)/60)+TRUNC(b.ENT_LATITUDE/100/100), 
                -1*((((b.ENT_LONGITUDE/100/100-TRUNC(b.ENT_LONGITUDE/100/100))*100)/60)+TRUNC(b.ENT_LONGITUDE/100/100)), 'SW_BROWNS_EXT')>0
                            then 'SWBROWNS'
                            when mflib.simplepoly.inside((((b.ENT_LATITUDE/100/100-TRUNC(b.ENT_LATITUDE/100/100))*100)/60)+TRUNC(b.ENT_LATITUDE/100/100), 
                -1*((((b.ENT_LONGITUDE/100/100-TRUNC(b.ENT_LONGITUDE/100/100))*100)/60)+TRUNC(b.ENT_LONGITUDE/100/100)), 'SE_BROWNS_EXT')>0
                            then 'SEBROWNS'
                            when mflib.simplepoly.inside((((b.ENT_LATITUDE/100/100-TRUNC(b.ENT_LATITUDE/100/100))*100)/60)+TRUNC(b.ENT_LATITUDE/100/100), 
                -1*((((b.ENT_LONGITUDE/100/100-TRUNC(b.ENT_LONGITUDE/100/100))*100)/60)+TRUNC(b.ENT_LONGITUDE/100/100)), 'GEORGES_BASIN_EXT')>0
                            then 'GBASIN'
                            when mflib.simplepoly.inside((((b.ENT_LATITUDE/100/100-TRUNC(b.ENT_LATITUDE/100/100))*100)/60)+TRUNC(b.ENT_LATITUDE/100/100), 
                -1*((((b.ENT_LONGITUDE/100/100-TRUNC(b.ENT_LONGITUDE/100/100))*100)/60)+TRUNC(b.ENT_LONGITUDE/100/100)), 'GEORGES_BANK_EXT')>0
                            then 'GBANK'
                            else 'UNKNOWN'
                            end OFFAREA
                from (
                select slip_lbs/est_lbs ratio, mon_doc_id from (
                select a.est_lbs, sum(b.slip_weight_lbs) slip_lbs, a.mon_doc_id  from (
                select sum(est_weight_log_lbs) est_lbs, mon_doc_id 
                from marfissci.lobster_md_log
                group by mon_doc_id
                ) a, marfissci.lobster_md_slip b
                where a.mon_doc_id = b.mon_doc_id
                group by a.est_lbs, a.mon_doc_id
                )) a, marfissci.lobster_md_log b
                where a.mon_doc_id = b.mon_doc_id
                and b.licence_id in (141926, 141930)"
                
        logs41 = sqlQuery(con, query41)
        logs41$DDLON<-logs41$DDLON*-1
        save( logs41, file=file.path( fn.root, "logs41.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load (file.path( fn.root, "logs41.rdata"), .GlobalEnv)
      
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

     ## lobster catch from scallop survey  
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
    ## lobster survey  
    if (DS %in% c("survey.redo", "survey") ) {

      if (DS=="survey.redo") {
        # survey
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        surveyCatch<-sqlQuery(con, "select * from lobster.ILTSSETS_MV")
        surveyMeasurements<-sqlQuery(con, "select * from lobster.ILTSDETAILS_MV")
        with(surveyMeasurements,paste(TRIPNO,SET_NO,sep=''))->surveyMeasurements$SET_ID
        with(surveyCatch,paste(TRIP_ID,SET_NO,sep=''))->surveyCatch$SET_ID
        surveyCatch$SET_LONG<-surveyCatch$SET_LONG*-1
        surveyCatch$HAUL_LONG<-surveyCatch$HAUL_LONG*-1
        surveyCatch$YEAR<-year(surveyCatch$BOARD_DATE)
        surveyMeasurements$SET_LON<-surveyMeasurements$SET_LON*-1
        surveyMeasurements$HAUL_LON<-surveyMeasurements$HAUL_LON*-1
        save( surveyCatch, file=file.path( fn.root, "surveyCatch.rdata"), compress=T)
        save( surveyMeasurements, file=file.path( fn.root, "surveyMeasurements.rdata"), compress=T)
        gc()  # garbage collection
      }
      load(file.path( fn.root, "surveyCatch.rdata"), .GlobalEnv)
      load(file.path( fn.root, "surveyMeasurements.rdata"), .GlobalEnv)
    }
  }


# to get data from PC's .Rdata files
#
#       surveySets.lst<-list()
#        surveyLobsters.lst<-list()
#        for(i in 1:length(yrs)){
#          load(file.path( project.datadirectory("lobster"), "data", "ISDB",paste("lobster_",yrs[i],".Rdata",sep='')))
#          trips<-subset(data$istrips,select=c("TRIP_ID","BOARD_DATE","LANDING_DATE"))
#          sets<-subset(data$isfishsets,select=c("TRIP_ID","FISHSET_ID","SET_NO"))
#          sets$TRIPSET_ID<-paste(sets$TRIP_ID,sets$SET_NO,sep='.')
#          sets$YEAR<-yrs[i]
#          data$issetprofile$TRIP_ID<-NA
#          for(j in 1:nrow(trips)){
#            data$issetprofile$TRIP_ID[ data$issetprofile$SETDATE>=trips$BOARD_DATE[j]&data$issetprofile$SETDATE<=trips$LANDING_DATE[j]]<-trips$TRIP_ID[j]
#          }
#          data$issetprofile$TRIPSET_ID<-paste(data$issetprofile$TRIP_ID,data$issetprofile$SET_NO,sep='.')
#          profile.start<-subset(data$issetprofile,PNTCD_ID==2,c("TRIPSET_ID","SETDATE","SETTIME","DEPTH","LATITUDE","LONGITUDE"))
#          names(profile.start)[-1]<-paste('start',names(profile.start)[-1],sep='.')
#          profile.end<-subset(data$issetprofile,PNTCD_ID==3,c("TRIPSET_ID","SETDATE","SETTIME","DEPTH","LATITUDE","LONGITUDE"))
#          names(profile.end)[-1]<-paste('end',names(profile.end)[-1],sep='.')
#          catches<-subset(data$iscatches,SPECCD_ID==2550,c("FISHSET_ID","CATCH_ID","SET_NO","EST_NUM_CAUGHT","EST_COMBINED_WT"))
#          surveySets.lst[[i]]<-merge(merge(sets,merge(profile.start,profile.end,all=T),all=T),catches,all=T)
#          surveyLobsters.lst[[i]]<-subset(data$isfish,select=c("FISH_ID","CATCH_ID","FISH_NO","SEXCD_ID","FISH_LENGTH"))
#        }
#        surveySets<-do.call("rbind",surveySets.lst)
#        surveyLobsters<-do.call("rbind",surveyLobsters.lst)
