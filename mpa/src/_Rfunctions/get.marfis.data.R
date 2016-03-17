get.marfissci.data<-function(write.csv=T){
  library(RODBC)
  channel <-  odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
#'MMM 2016
#'Bulk taken from SQL behind mflib.marfis_catch_effort
#'
#'OMITTED FIELDS
#'ps.VR_NUMBER_FISHING,
#'v.VESSEL_NAME,
#'v.GROSS_TONNAGE,
#'v.LOA,
#'to_char(ps.DATE_FISHED,'YYYY') YEAR_FISHED,
#'to_char(ps.DATE_FISHED,'MM') MONTH_FISHED,
#'le.DET_NAFO_UNIT_AREA_ID,
#'a.AREA,
#'s.DESC_ENG species,
#'ps.SPECIES_SIZE_CODE,
#'hi.OBSERVER_FLAG,
#'md.TRIP_DMP_COMPANY_ID,
#'ps.RND_WEIGHT_KGS val,
#'md.TRIP_ID,
#'ps.LANDED_DATE,
#'#'OMITTED TABLES
#'marfissci.HAIL_IN_CALLS hi,
#'marfissci.MON_DOCS md,
#'marfissci.AREAS a,
#'marfissci.VESSELS v,
#'marfissci.SPECIES s 
#'#'OMITTED JOINS
#'#'s.SPECIES_CODE=ps.SPECIES_CODE AND
#'le.MON_DOC_ID = md.MON_DOC_ID AND
#'hi.HAIL_IN_CALL_ID = md.HAIL_IN_CALL_ID AND
#'md.MON_DOC_ID = ps.MON_DOC_ID AND 
#'le.DET_NAFO_UNIT_AREA_ID = a.AREA_ID AND
#'md.VR_NUMBER = v.VR_NUMBER;
#'
#'--9724 records where landed_date is earlier than date_fished vs
#'--3316690 records  where date_fished is earlier than landed_date
#'--assuming that date_fished more likely represents the moment of catch 
#'--date_fished has slightly less records with times (1849136 vs 1883136)
#'Using date_fished
query = "SELECT 
le.LOG_EFRT_STD_INFO_ID setid,
ps.GEAR_CODE,
le.LOG_EFRT_STD_INFO_ID,
ps.DATE_FISHED,
le.ENT_LATITUDE,
le.ENT_LONGITUDE,
ROUND(SUBSTR(le.ENT_LATITUDE,1,2)+
        SUBSTR(le.ENT_LATITUDE,3,2)/60+
        SUBSTR(le.ENT_LATITUDE,5,2)/3600,4) lat, 
ROUND(-1*(SUBSTR(le.ENT_LONGITUDE,1,2)+
            SUBSTR(le.ENT_LONGITUDE,3,2)/60+
            SUBSTR(le.ENT_LONGITUDE,5,2)/3600),4) lon, 
ps.SPECIES_CODE,
ps.LANDED_FORM_CODE,
le.FV_DURATION_IN_HOURS,
le.FV_NUM_OF_GEAR_UNITS,
ps.RND_WEIGHT_KGS
FROM  
marfissci.LOG_EFRT_STD_INFO le,
marfissci.PRO_SPC_INFO ps
WHERE 
le.LOG_EFRT_STD_INFO_ID = ps.LOG_EFRT_STD_INFO_ID"
the.data = sqlQuery(channel, query)
if (write.csv) {
  extractStamp <-  strftime(Sys.time(),"%Y%m%d_%H%M")
  write.csv2(the.data, paste0(project.datadirectory("mpa"),"/get_marfis_",extractStamp,".csv"))
}
return(the.data)
}
