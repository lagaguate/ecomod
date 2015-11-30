
  lobster.db = function( DS="complete.redo") {


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
        lobster.db( DS="logs41.redo")
        lobster.db( DS="logs41jonah.redo")
        lobster.db( DS="observer41.redo")
        lobster.db( DS="atSea.redo")
        lobster.db( DS="cris.redo")
        lobster.db( DS="port.redo")
        lobster.db( DS="vlog.redo")
        lobster.db( DS="fsrs.redo")
        lobster.db( DS="scallop.redo")
        lobster.db( DS="survey.redo")
      }
    }

### Inshore Commercial Logs and slips
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

### Offshore Commercial Logs
    if (DS %in% c("logs41.redo", "logs41") ) {

     if (DS=="logs41.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
        query41<-"select b.mon_doc_id, b.vr_number, b.vessel_name, b.captain, b.licence_id, b.FV_FISHED_DATETIME, 
                round((((b.ENT_LATITUDE/100/100-TRUNC(b.ENT_LATITUDE/100/100))*100)/60)+TRUNC(b.ENT_LATITUDE/100/100)DDLAT,
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
                and b.licence_id in (141926, 141929, 141930)"
         
        slipquery41<-"select  to_char(landing_date_time, 'yyyy'), sum(slip_weight_lbs) from marfissci.lobster_md_slip where licence_id in (141926,141929,141930) group by  to_char(landing_date_time, 'yyyy')"        
        slip41 = sqlQuery(con, slipquery41)
        logs41 = sqlQuery(con, query41)
        logs41$DDLON<-logs41$DDLON*-1
        save( logs41, file=file.path( fn.root, "logs41.rdata"), compress=T)
        save( slip41, file=file.path( fn.root, "slip41.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load (file.path( fn.root, "logs41.rdata"), .GlobalEnv)
      load (file.path( fn.root, "slip41.rdata"), .GlobalEnv)
      
    }

### Offshore Commercial Logs for Jonah crab
   if (DS %in% c("logs41jonah.redo", "logs41jonah") ) {

     if (DS=="logs41jonah.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
        query41<-"select doc_id, licence_id, vr_number, vessel_name, captain_name, date_fished,
                round((((LATITUDE/100/100-TRUNC(LATITUDE/100/100))*100)/60)+TRUNC(LATITUDE/100/100),4) DDLAT,
                round((((LONGITUDE/100/100-TRUNC(LONGITUDE/100/100))*100)/60)+TRUNC(LONGITUDE/100/100),4) DDLON,
                num_of_traps, est_weight_log_lbs, pro_rated_slip_wt_lbs adjcatch_lbs, 
                case          when mflib.simplepoly.inside((((LATITUDE/100/100-TRUNC(LATITUDE/100/100))*100)/60)+TRUNC(LATITUDE/100/100), 
                -1*((((LONGITUDE/100/100-TRUNC(LONGITUDE/100/100))*100)/60)+TRUNC(LONGITUDE/100/100)), 'CROWELL_BASIN_EXT')>0
                              then 'CROWELL'
                              when mflib.simplepoly.inside((((LATITUDE/100/100-TRUNC(LATITUDE/100/100))*100)/60)+TRUNC(LATITUDE/100/100), 
                -1*((((LONGITUDE/100/100-TRUNC(LONGITUDE/100/100))*100)/60)+TRUNC(LONGITUDE/100/100)), 'SW_BROWNS_EXT')>0
                              then 'SWBROWNS'
                              when mflib.simplepoly.inside((((LATITUDE/100/100-TRUNC(LATITUDE/100/100))*100)/60)+TRUNC(LATITUDE/100/100), 
                -1*((((LONGITUDE/100/100-TRUNC(LONGITUDE/100/100))*100)/60)+TRUNC(LONGITUDE/100/100)), 'SE_BROWNS_EXT')>0
                              then 'SEBROWNS'
                              when mflib.simplepoly.inside((((LATITUDE/100/100-TRUNC(LATITUDE/100/100))*100)/60)+TRUNC(LATITUDE/100/100), 
                -1*((((LONGITUDE/100/100-TRUNC(LONGITUDE/100/100))*100)/60)+TRUNC(LONGITUDE/100/100)), 'GEORGES_BASIN_EXT')>0
                              then 'GBASIN'
                              when mflib.simplepoly.inside((((LATITUDE/100/100-TRUNC(LATITUDE/100/100))*100)/60)+TRUNC(LATITUDE/100/100), 
                -1*((((LONGITUDE/100/100-TRUNC(LONGITUDE/100/100))*100)/60)+TRUNC(LONGITUDE/100/100)), 'GEORGES_BANK_EXT')>0
                              then 'GBANK'
                    when mflib.simplepoly.inside((((LATITUDE/100/100-TRUNC(LATITUDE/100/100))*100)/60)+TRUNC(LATITUDE/100/100), 
                -1*((((LONGITUDE/100/100-TRUNC(LONGITUDE/100/100))*100)/60)+TRUNC(LONGITUDE/100/100)), 'LFA41_4W')>0
                              then '4W'
                              else 'UNKNOWN'
                              end OFFAREA
                from marfissci.marfis_crab
                where licence_id in (141929, 141931)"
         
        logs41jonah = sqlQuery(con, query41)
        logs41jonah$DDLON<-logs41jonah$DDLON*-1
        save( logs41jonah, file=file.path( fn.root, "logs41jonah.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load (file.path( fn.root, "logs41jonah.rdata"), .GlobalEnv)
      
    }

### Offshore Observer
    if (DS %in% c("observer41.redo", "observer41") ) {

     if (DS=="observer41.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
        query41<-"select TRIP_ID, 
                  CFV,
                  VESSEL_NAME,
                  LICENSE_NO,
                  NUM_HOOK_HAUL,
                  BOARD_DATE,
                  case  when to_char(board_date, 'MM') in (01,02,03)
                        then 1
                        when to_char(board_date, 'MM') in (04,05,06)
                        then 2
                        when to_char(board_date, 'MM') in (07,08,09)
                        then 3
                        when to_char(board_date, 'MM') in (10,11,12)
                        then 4
                        else null
                  end QUARTER,
                  SET_NO,
                  SOURCE,
                  SPECCD_ID,
                  COMMON,
                  EST_NUM_CAUGHT,
                  EST_KEPT_WT,
                  EST_DISCARD_WT,
                  LATITUDE,
                  LONGITUDE,
                  case when mflib.simplepoly.inside(latitude, -1*longitude, 'CROWELL_BASIN_EXT')>0
                       then '4WBROWNS'
                       when mflib.simplepoly.inside(latitude, -1*longitude, 'SW_BROWNS_EXT')>0
                       then '4WBROWNS'
                       when mflib.simplepoly.inside(latitude, -1*longitude, 'SE_BROWNS_EXT')>0
                       then '3SEBROWNS'
                       when mflib.simplepoly.inside(latitude, -1*longitude, 'GEORGES_BASIN_EXT')>0
                       then '2GBASIN'
                       when mflib.simplepoly.inside(latitude, -1*longitude, 'GEORGES_BANK_EXT')>0
                       then '1GBANK'
                       WHEN TRIP_ID = 100031557 AND SET_NO = 30
                       THEN '2GBASIN'
                       WHEN TRIP_ID = 100034606 AND SET_NO IN (8,9,12)
                       THEN '1GBANK'
                       WHEN TRIP_ID = 100036824 AND SET_NO IN (19,20)
                       THEN '1GBANK'
                       WHEN TRIP_ID = 100037267 AND SET_NO IN (54,55)
                       THEN '1GBANK'
                       WHEN TRIP_ID = 100037677 AND SET_NO = 57
                       THEN '4WBROWNS'
                       WHEN TRIP_ID = 100040377 AND SET_NO = 49
                       THEN '4WBROWNS'
                       WHEN TRIP_ID = 100042642 AND SET_NO = 43
                       THEN '4WBROWNS'
                       WHEN TRIP_ID = 100045023 AND SET_NO in (19,20)
                       THEN '1GBANK'
                       else 'UNKNOWN'
                       end OFFAREA,
                  COMMENTS
                  from (
                  select  a.trip_id, g.cfv, g.vessel_name, g.license_no, b.num_hook_haul, a.board_date, c.set_no, b.source,
                  c.speccd_id, d.common, c.est_num_caught, c.est_kept_wt, c.est_discard_wt, 
                  f.latitude, f.longitude, replace(b.comments,Chr(10),' ') comments
                   from   istrips a,
                   isfishsets b,
                   iscatches c,
                   observer.isspeciescodes d,
                   (SELECT 
                      fishset_id, 
                  (CASE 
                      WHEN pntcd_lat_3 is not null and pntcd_lon_3 is not null
                      THEN pntcd_lat_3
                      ELSE 
                        (CASE 
                         WHEN pntcd_lat_4 is not null and pntcd_lon_4 is not null
                         THEN pntcd_lat_4
                         ELSE 
                            (CASE 
                             WHEN pntcd_lat_1 is not null and pntcd_lon_1 is not null
                             THEN pntcd_lat_1
                             ELSE 
                                (CASE 
                                 WHEN pntcd_lat_2 is not null and pntcd_lon_2 is not null
                                 THEN pntcd_lat_2
                                 ELSE 
                                  NULL
                                 END)
                             END)
                         END)
                      END) latitude,
                  (CASE 
                      WHEN pntcd_lat_3 is not null and pntcd_lon_3 is not null
                      THEN pntcd_lon_3
                      ELSE 
                        (CASE 
                         WHEN pntcd_lat_4 is not null and pntcd_lon_4 is not null
                         THEN pntcd_lon_4
                         ELSE 
                            (CASE 
                             WHEN pntcd_lat_1 is not null and pntcd_lon_1 is not null
                             THEN pntcd_lon_1
                             ELSE 
                                (CASE 
                                 WHEN pntcd_lat_2 is not null and pntcd_lon_2 is not null
                                 THEN pntcd_lon_2
                                 ELSE 
                                  NULL
                                 END)
                             END)
                         END)
                      END) longitude
                  FROM (
                  SELECT
                    a.fishset_id, 
                   sum(case b.pntcd_id when 1 then latitude else null end ) pntcd_lat_1,
                   sum(case b.pntcd_id when 1 then longitude else null end ) pntcd_lon_1,
                   sum(case b.pntcd_id when 2 then latitude else null end ) pntcd_lat_2,
                   sum(case b.pntcd_id when 2 then longitude else null end ) pntcd_lon_2,
                   sum(case b.pntcd_id when 3 then latitude else null end ) pntcd_lat_3,
                   sum(case b.pntcd_id when 3 then longitude else null end ) pntcd_lon_3,
                   sum(case b.pntcd_id when 4 then latitude else null end ) pntcd_lat_4,
                   sum(case b.pntcd_id when 4 then longitude else null end ) pntcd_lon_4
                  FROM observer.isfishsets a, observer.issetprofile b
                  where a.fishset_id = b.fishset_id(+)
                  group by a.fishset_id
                  order by a.fishset_id
                  ) 
                  )F,
                   isvessels g
                   where  a.trip_id = b.trip_id
                   and  g.vess_id = a.vess_id(+)
                   and   b.fishset_id = c.fishset_id(+)
                   and   c.speccd_id = d.speccd_id(+)
                   and  b.fishset_id = f.fishset_id(+)
                   and  b.fishset_id = c.fishset_id(+)
                  and a.vess_id in (295,316,317,1891,1892,1979,1982,2096,2097,2098,2099,2152,2153,4444,10069,10141,10285,12217,13677,18620,20140)
                  and a.tripcd_id = 2550
                  and a.board_date > '2001-12-31'
                  order by trip_id, set_no
                  )"
       
        observer41 = sqlQuery(con, query41)
        save( observer41, file=file.path( fn.root, "observer41.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load (file.path( fn.root, "observer41.rdata"), .GlobalEnv)
      
    }

### At Sea sampling from Cheryl's view 
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

### port sampling 
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
  
### voluntary logs 
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

### CRIS database
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
  
### FSRS traps 
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

### lobster catch from scallop survey  
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

### lobster survey  
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

