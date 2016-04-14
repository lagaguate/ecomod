GetLogData <- function(update=T){
  ##############################################################################
  # Get data from database, download whole view "Dale_log_cpue2'
  ##############################################################################
  if(update==T){
   # open DB connection  .. need to be defined elsewhere .. private file
   RODBCconn <- MakeConnection()
   log.q <- "  SELECT M.LOGRECORD_ID,V.VESSEL_NAME,V.CFV,
       M.RECORD_DATE, M.RECORD_NO, TRIP.TRIP_NO, TRIP.LOGTRIP_ID,
       M.SUBTRIP_NO,M.LOGGEAR_ID,M.LAT_DD,
       M.LON_DD, M.NAFO_AREA NAFO, ST.SPEED,ST.AVE_TIME,
       NT.N_TOWS,BW.B_WIDTH,C.BLANCHED,
       C.RAW_CLAMS,C.WHOLE,
       ((6.51 * C.BLANCHED)+ (5.37* C.RAW_CLAMS)+ C.WHOLE)  ROUND_CATCH,
       (ST.SPEED * 1000. * ST.AVE_TIME * BW.B_WIDTH * NT.N_TOWS/60.) AREA_TOWED
FROM CLAMCOMLOGSRECORD M,
( SELECT LOGRECORD_ID,SUM(TB) BLANCHED,SUM(TR) RAW_CLAMS,
         SUM(TW) WHOLE
   FROM (SELECT LOGRECORD_ID,
        NVL(DECODE(PRODUCT_ID,16,(DECODE(ITIS_CODE,80983,WEIGHT))),0) TB, 
        NVL(DECODE(PRODUCT_ID,17,(DECODE(ITIS_CODE,80983,WEIGHT))),0) TR, 
        NVL(DECODE(PRODUCT_ID,18,(DECODE(ITIS_CODE,80983,WEIGHT))),0) TW
        FROM CLAMCOMLOGSPRODUCT )
   GROUP BY LOGRECORD_ID
) C,
(SELECT LOGRECORD_ID,
    SUM(DECODE(MEASUREMENT,'AVERAGE SPEED',
    DECODE(UNIT,'kph',MEASUREMENT_VALUE,'kn',
    1.852*MEASUREMENT_VALUE))) SPEED,
    SUM(DECODE(MEASUREMENT,'AVERAGE TOW TIME',
    MEASUREMENT_VALUE)) AVE_TIME
 FROM CLAMCOMLOGSRECORDMEASUREMENT
 WHERE MEASUREMENT IN ('AVERAGE SPEED','AVERAGE TOW TIME')
 AND MEASUREMENT_VALUE IS NOT NULL
 GROUP BY LOGRECORD_ID
) ST,
(SELECT LOGRECORD_ID,
        SUM(DREDGES_USED * TOWS_MADE) N_TOWS
  FROM CLAMCOMLOGSRECORDPROFILE
  GROUP BY LOGRECORD_ID
) NT,
(SELECT loggear_id,
     DECODE(UNIT, 'in', FEATURE_VALUE*0.0254,'m',FEATURE_VALUE) B_WIDTH
  FROM clamcomlogsgearfeature
  WHERE gearfeature = 'BLADE WIDTH'
) BW,
CLAMVESSEL V, CLAMCOMLOGSTRIP TRIP,CLAMCOMLOGSGEAR GEAR
WHERE M.LOGRECORD_ID = C.LOGRECORD_ID(+)
AND M.LOGRECORD_ID = ST.LOGRECORD_ID(+)
AND M.LOGRECORD_ID = NT.LOGRECORD_ID(+)
AND M.LOGGEAR_ID = BW.LOGGEAR_ID(+)
AND V.VESSEL_ID = TRIP.VESSEL_ID
AND TRIP.LOGTRIP_ID = GEAR.LOGTRIP_ID
AND GEAR.LOGGEAR_ID = M.LOGGEAR_ID" 
    log.data <- sqlQuery(RODBCconn, log.q)
    odbcClose(RODBCconn)
    save(log.data,file=file.path( project.datadirectory("offshoreclams"), "data", "Logdata.Rdata" ))
  }
  else {
  	load(file.path( project.datadirectory("offshoreclams"), "data", "Logdata.Rdata" ))
  }
 return(log.data)
}
