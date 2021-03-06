groundfish.track.selector<-function(start.year,sp.code){
  #'Mike McMahon May 2016
  #'For a given species code and start year, this script will extract all of the sets from the 
  #'RV survey where that species was caught, as well as all of the other (i.e. 
  #'null) sets.
  #'This is similar to:
  #' capechidley.track.selector.R and 
  #' redfish.track.selector.R
  library(RODBC)
  channel <-  odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
  query=paste0("SELECT GROUNDFISH.GSINF.MISSION,
  GROUNDFISH.GSINF.SETNO,
  to_char(GROUNDFISH.GSINF.SDATE,'YYYY') YEAR,
  GROUNDFISH.GSINF.SDATE,
  (SUBSTR(GROUNDFISH.GSINF.SLONG,1,2)+ROUND(((GROUNDFISH.GSINF.SLONG-(SUBSTR(GROUNDFISH.GSINF.SLONG,1,2)*100))/60),2))*-1 SLONG,
  (SUBSTR(GROUNDFISH.GSINF.SLAT,1,2)+ROUND(((GROUNDFISH.GSINF.SLAT-(SUBSTR(GROUNDFISH.GSINF.SLAT,1,2)*100))/60),2)) SLAT,
  (SUBSTR(GROUNDFISH.GSINF.ELONG,1,2)+ROUND(((GROUNDFISH.GSINF.ELONG-(SUBSTR(GROUNDFISH.GSINF.ELONG,1,2)*100))/60),2))*-1 ELONG,
  (SUBSTR(GROUNDFISH.GSINF.ELAT,1,2)+ROUND(((GROUNDFISH.GSINF.ELAT-(SUBSTR(GROUNDFISH.GSINF.ELAT,1,2)*100))/60),2)) ELAT,
  GROUNDFISH.GSCAT.TOTNO,
  GROUNDFISH.GSCAT.TOTWGT
  FROM GROUNDFISH.GSINF
  INNER JOIN GROUNDFISH.GSCAT
  ON GROUNDFISH.GSINF.MISSION = GROUNDFISH.GSCAT.MISSION
  AND GROUNDFISH.GSINF.SETNO  = GROUNDFISH.GSCAT.SETNO
  WHERE GROUNDFISH.GSINF.TYPE = 1
  AND GROUNDFISH.GSCAT.SPEC   = ",sp.code,"
  AND to_char(GROUNDFISH.GSINF.SDATE,'YYYY') >= ",start.year)
  data <- sqlQuery(channel,query)
  
  nulls_query=paste0("SELECT GROUNDFISH.GSINF.MISSION,
  GROUNDFISH.GSINF.SETNO,
  to_char(GROUNDFISH.GSINF.SDATE,'YYYY') YEAR,
  GROUNDFISH.GSINF.SDATE,
  (SUBSTR(GROUNDFISH.GSINF.SLONG,1,2)+ROUND(((GROUNDFISH.GSINF.SLONG-(SUBSTR(GROUNDFISH.GSINF.SLONG,1,2)*100))/60),2))*-1 SLONG,
  (SUBSTR(GROUNDFISH.GSINF.SLAT,1,2)+ROUND(((GROUNDFISH.GSINF.SLAT-(SUBSTR(GROUNDFISH.GSINF.SLAT,1,2)*100))/60),2)) SLAT,
  (SUBSTR(GROUNDFISH.GSINF.ELONG,1,2)+ROUND(((GROUNDFISH.GSINF.ELONG-(SUBSTR(GROUNDFISH.GSINF.ELONG,1,2)*100))/60),2))*-1 ELONG,
  (SUBSTR(GROUNDFISH.GSINF.ELAT,1,2)+ROUND(((GROUNDFISH.GSINF.ELAT-(SUBSTR(GROUNDFISH.GSINF.ELAT,1,2)*100))/60),2)) ELAT,
  0 TOTNO,
  0 TOTWGT
  FROM GROUNDFISH.GSINF
  WHERE GROUNDFISH.GSINF.TYPE = 1
  AND to_char(GROUNDFISH.GSINF.SDATE,'YYYY') >= ",start.year)
  nulls <- sqlQuery(channel,  nulls_query)
  
all.recs=merge(nulls,data, by=c("MISSION", "SETNO","YEAR","SDATE","ELONG", "ELAT", "SLONG","SLAT"), all.x=T)
all.recs$TOTNO.x = NULL
all.recs$TOTWGT.x = NULL
colnames(all.recs)[which(names(all.recs) == "TOTWGT.y")] = "TOTWGT"
colnames(all.recs)[which(names(all.recs) == "TOTNO.y")] = "TOTNO"
return(all.recs)
}
          