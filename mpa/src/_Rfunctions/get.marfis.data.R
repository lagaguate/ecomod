get.marfissci.data<-function(write.csv=T, agg=0.09){
  library(RODBC)
  channel <-  odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
#'MMM 2016

query = "
SELECT 
ROWNUM,
LOG_EFRT_STD_INFO_ID,
DATE_FISHED, 
to_char(DATE_FISHED, 'YYYY') YEAR_FISHED,
GEAR_CODE,
SPECIES_CODE,
SPECIES_SIZE_CODE,
CATCH_USAGE_CODE,
FV_DURATION_IN_HOURS,
FV_NUM_OF_GEAR_UNITS,
UNIT_OF_MEASURE_ID,
SUM(RND_WEIGHT_KGS) AS RND_WEIGHT_KGS,
SUM(RPT_WEIGHT_KGS) AS RPT_WEIGHT_KGS,
LAT, 
LON
FROM
(
  /*Get all catch where CATCH_USAGE_CODE= 'LANDED' */
  SELECT 
  P.LOG_EFRT_STD_INFO_ID,
  P.DATE_FISHED, 
  P.GEAR_CODE,
  P.SPECIES_CODE,
  P.LANDED_FORM_CODE,
  P.SPECIES_SIZE_CODE,
  P.CATCH_USAGE_CODE,
  E.FV_DURATION_IN_HOURS,
  E.FV_NUM_OF_GEAR_UNITS,
  -999 UNIT_OF_MEASURE_ID,  --must assume that CDD got it right
  P.RND_WEIGHT_KGS,
  P.RPT_WEIGHT_KGS,
  /*try desperately to position data*/
  ROUND(
  CASE WHEN E.ENT_LATITUDE IS NOT NULL
  THEN SUBSTR(E.ENT_LATITUDE, 1, 2) + SUBSTR(E.ENT_LATITUDE, 3, 2) / 60 + SUBSTR(E.ENT_LATITUDE, 5, 2) / 3600
  WHEN E.DET_LATITUDE IS NOT NULL
  THEN SUBSTR(E.DET_LATITUDE, 1, 2) + SUBSTR(E.DET_LATITUDE, 3, 2) / 60 + SUBSTR(E.DET_LATITUDE, 5, 2) / 3600
  WHEN P.LATITUDE IS NOT NULL
  THEN SUBSTR(P.LATITUDE, 1, 2) + SUBSTR(P.LATITUDE, 3, 2) / 60 + SUBSTR(P.LATITUDE, 5, 2) / 3600
  WHEN E.DET_NAFO_UNIT_AREA_ID IS NOT NULL
  THEN c.LAT
  ELSE 35
  END, 4) LAT, 
  ROUND(
  CASE WHEN E.ENT_LONGITUDE IS NOT NULL
  THEN (-1 * (SUBSTR(E.ENT_LONGITUDE, 1, 2) + SUBSTR(E.ENT_LONGITUDE, 3, 2) / 60 + SUBSTR(E.ENT_LONGITUDE, 5, 2) / 3600))
  WHEN E.DET_LONGITUDE IS NOT NULL
  THEN (-1 * (SUBSTR(E.DET_LONGITUDE, 1, 2) + SUBSTR(E.DET_LONGITUDE, 3, 2) / 60 + SUBSTR(E.DET_LONGITUDE, 5, 2) / 3600))
  WHEN P.LONGITUDE IS NOT NULL
  THEN (-1 * (SUBSTR(P.LONGITUDE, 1, 2) + SUBSTR(P.LONGITUDE, 3, 2) / 60 + SUBSTR(P.LONGITUDE, 5, 2) / 3600))
  WHEN E.DET_NAFO_UNIT_AREA_ID IS NOT NULL
  THEN c.LON
  ELSE -50
  END, 4) LON
  FROM MARFISSCI.pro_spc_info P
  INNER JOIN MARFISSCI.LOG_EFRT_STD_INFO E
  ON P.LOG_EFRT_STD_INFO_ID = E.LOG_EFRT_STD_INFO_ID
  LEFT JOIN (
  SELECT a.AREA_ID, NAFO_centroids.lat, NAFO_centroids.lon
  FROM MARFISSCI.nafo_unit_areas a
  INNER JOIN mcmahonm.NAFO_centroids
  ON a.area = NAFO_centroids.NAFO_BEST) c
  ON E.DET_NAFO_UNIT_AREA_ID = c.area_id
  UNION
  /*Get all catch where CATCH_USAGE_CODE <> 'LANDED'*/
  SELECT 
  S1.LOG_EFRT_STD_INFO_ID,
  E1.FV_FISHED_DATETIME DATE_FISHED,
  E1.FV_GEAR_CODE GEAR_CODE,
  S1.SSF_SPECIES_CODE AS SPECIES_CODE,
  S1.SSF_LANDED_FORM_CODE AS LANDED_FORM_CODE,
  S1.SSF_SPECIES_SIZE_CODE AS SPEC_SIZE_CODE,
  S1.CATCH_USAGE_CODE,
  E1.FV_DURATION_IN_HOURS,
  E1.FV_NUM_OF_GEAR_UNITS,
  S1.UNIT_OF_MEASURE_ID,
  ROUND(
  CASE
  WHEN S1.UNIT_OF_MEASURE_ID = 10
  THEN S1.WEIGHT
  WHEN S1.UNIT_OF_MEASURE_ID = 20
  THEN S1.WEIGHT * 0.453592
  WHEN S1.UNIT_OF_MEASURE_ID = 30
  THEN S1.WEIGHT / 1000
  END, 2) WEIGHT_KG,
  -999 RPT_WEIGHT_KG, --don't have a reported weight
  /*try desperately to position data*/
  ROUND(
  CASE WHEN E1.ENT_LATITUDE IS NOT NULL
  THEN SUBSTR(E1.ENT_LATITUDE, 1, 2) + SUBSTR(E1.ENT_LATITUDE, 3, 2) / 60 + SUBSTR(E1.ENT_LATITUDE, 5, 2) / 3600
  WHEN E1.DET_LATITUDE IS NOT NULL
  THEN SUBSTR(E1.DET_LATITUDE, 1, 2) + SUBSTR(E1.DET_LATITUDE, 3, 2) / 60 + SUBSTR(E1.DET_LATITUDE, 5, 2) / 3600
  WHEN E1.DET_NAFO_UNIT_AREA_ID IS NOT NULL
  THEN c1.LAT
  ELSE 35
  END, 4) LAT,
  ROUND( CASE WHEN E1.ENT_LONGITUDE IS NOT NULL
  THEN (-1 * (SUBSTR(E1.ENT_LONGITUDE, 1, 2) + SUBSTR(E1.ENT_LONGITUDE, 3, 2) / 60 + SUBSTR(E1.ENT_LONGITUDE, 5, 2) / 3600))
  WHEN E1.DET_LONGITUDE IS NOT NULL
  THEN (-1 * (SUBSTR(E1.DET_LONGITUDE, 1, 2) + SUBSTR(E1.DET_LONGITUDE, 3, 2) / 60 + SUBSTR(E1.DET_LONGITUDE, 5, 2) / 3600))
  WHEN E1.DET_NAFO_UNIT_AREA_ID IS NOT NULL
  THEN c1.LON
  ELSE -50
  END, 4) LON
  FROM MARFISSCI.LOG_SPC_STD_INFO S1
  INNER JOIN MARFISSCI.LOG_EFRT_STD_INFO E1
  ON S1.LOG_EFRT_STD_INFO_ID = E1.LOG_EFRT_STD_INFO_ID
  LEFT JOIN (
  SELECT a.AREA_ID, NAFO_centroids.lat, NAFO_centroids.lon
  FROM MARFISSCI.nafo_unit_areas a
  INNER JOIN mcmahonm.NAFO_centroids
  ON a.area = NAFO_centroids.NAFO_BEST) c1
  ON E1.DET_NAFO_UNIT_AREA_ID = c1.area_id
  WHERE S1.CATCH_USAGE_CODE <> 10 
)
WHERE to_char(date_fished,'YYYY') = 2010
GROUP BY
ROWNUM,
LOG_EFRT_STD_INFO_ID,
DATE_FISHED, 
GEAR_CODE,
SPECIES_CODE,
SPECIES_SIZE_CODE,
CATCH_USAGE_CODE,
FV_DURATION_IN_HOURS,
FV_NUM_OF_GEAR_UNITS,
UNIT_OF_MEASURE_ID,
LAT, 
LON 
;"
raw.data = sqlQuery(channel, query)
#'this gives huge amounts of data - round the coordinates so that the next and 
#'group by the new 'points' so the aggregation fn has a lot less data to handle
#'
grp.data<-raw.data
grp.data$LAT = round(grp.data$LAT/agg)*agg
grp.data$LON = round(grp.data$LON/agg)*agg
grp.data = aggregate(grp.data[,c("RND_WEIGHT_KGS","RPT_WEIGHT_KGS")], by=list(YEAR_FISHED = grp.data$YEAR_FISHED, 
                                                                   SPECIES_CODE = grp.data$SPECIES_CODE, 
                                                                   SPECIES_SIZE_CODE = grp.data$SPECIES_SIZE_CODE,
                                                                   CATCH_USAGE_CODE = grp.data$CATCH_USAGE_CODE,
                                                                   LAT = grp.data$LAT, 
                                                                   LON = grp.data$LON), "sum")
if (write.csv) {
  extractStamp <-  strftime(Sys.time(),"%Y%m%d_%H%M")
  write.csv2(raw.data, paste0(project.datadirectory("mpa"),"/get_marfis_raw_",extractStamp,".csv"))
  write.csv2(grp.data, paste0(project.datadirectory("mpa"),"/get_marfis_grp_",extractStamp,".csv"))
}
return(grp.data)
}
