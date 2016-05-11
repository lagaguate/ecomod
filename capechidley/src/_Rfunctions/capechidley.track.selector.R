capechidley.track.selector<-function(sp.code){
  #'Mike McMahon May 2016
  #'For a given species code, this script will extract all of the sets from the 
  #'Cape Chidley dataset where that species was caught, as well as all of the 
  #'other (i.e. null) sets.
  #'This is similar to:
  #' groundfish.track.selector.R (which also takes a start.year) and 
  #' redfish.track.selector.R
library(RODBC)
channel <-  odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
query = paste0("SELECT 
  BONDS.DSINF.CRUNO,
  BONDS.DSINF.SETNO,
  BONDS.DSINF.DMAX,
  BONDS.DSINF.WIND,
  BONDS.DSINF.TYPE,
  BONDS.DSINF.DMIN,
  BONDS.DSINF.SDATE,
  BONDS.DSINF.TIME,
  BONDS.DSINF.STRAT,
  (SUBSTR(BONDS.DSINF.SLONG,1,2)+ROUND(((BONDS.DSINF.SLONG-(SUBSTR(BONDS.DSINF.SLONG,1,2)*100))/60),2))*-1 SLONG,
  (SUBSTR(BONDS.DSINF.SLAT,1,2)+ROUND(((BONDS.DSINF.SLAT-(SUBSTR(BONDS.DSINF.SLAT,1,2)*100))/60),2)) SLAT,
  (SUBSTR(BONDS.DSINF.ELONG,1,2)+ROUND(((BONDS.DSINF.ELONG-(SUBSTR(BONDS.DSINF.ELONG,1,2)*100))/60),2))*-1 ELONG,
  (SUBSTR(BONDS.DSINF.ELAT,1,2)+ROUND(((BONDS.DSINF.ELAT-(SUBSTR(BONDS.DSINF.ELAT,1,2)*100))/60),2)) ELAT,
  BONDS.DSINF.DIST,
  BONDS.DSCAT.TOTWGT,
  BONDS.DSCAT.TOTNO
FROM BONDS.DSCAT, BONDS.DSINF
WHERE BONDS.DSINF.SETNO = BONDS.DSCAT.SETNO
AND  BONDS.DSINF.CRUNO = BONDS.DSCAT.CRUNO
AND BONDS.DSCAT.SPEC = ",sp.code)
data <- sqlQuery(channel,query)

null.query = "SELECT 
  BONDS.DSINF.CRUNO,
  BONDS.DSINF.SETNO,
  BONDS.DSINF.DMAX,
  BONDS.DSINF.WIND,
  BONDS.DSINF.TYPE,
  BONDS.DSINF.DMIN,
  BONDS.DSINF.SDATE,
  BONDS.DSINF.TIME,
  BONDS.DSINF.STRAT,
  (SUBSTR(BONDS.DSINF.SLONG,1,2)+ROUND(((BONDS.DSINF.SLONG-(SUBSTR(BONDS.DSINF.SLONG,1,2)*100))/60),2))*-1 SLONG,
  (SUBSTR(BONDS.DSINF.SLAT,1,2)+ROUND(((BONDS.DSINF.SLAT-(SUBSTR(BONDS.DSINF.SLAT,1,2)*100))/60),2)) SLAT,
  (SUBSTR(BONDS.DSINF.ELONG,1,2)+ROUND(((BONDS.DSINF.ELONG-(SUBSTR(BONDS.DSINF.ELONG,1,2)*100))/60),2))*-1 ELONG,
  (SUBSTR(BONDS.DSINF.ELAT,1,2)+ROUND(((BONDS.DSINF.ELAT-(SUBSTR(BONDS.DSINF.ELAT,1,2)*100))/60),2)) ELAT,
  BONDS.DSINF.DIST,
  0 TOTWGT,
  0 TOTNO
FROM BONDS.DSINF"
null.data <- sqlQuery(channel,null.query)

all.recs=merge(null.data,data, by=c("CRUNO", "SETNO","DMAX","WIND","TYPE", "DMIN", "SDATE","TIME","STRAT","SLAT","SLONG","ELAT","ELONG","DIST"), all.x=T)
all.recs$TOTNO.x = NULL
all.recs$TOTWGT.x = NULL
colnames(all.recs)[which(names(all.recs) == "TOTWGT.y")] = "TOTWGT"
colnames(all.recs)[which(names(all.recs) == "TOTNO.y")] = "TOTNO"
return(all.recs)
}
