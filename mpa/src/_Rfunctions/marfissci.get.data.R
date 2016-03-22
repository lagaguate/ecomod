marfissci.get.data <- function(spp = NULL, years = NULL) {
  library(RODBC)
  channel <-
    odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
  #'MMM March 22, 2016
  #'This is a basic extraction limited only be species and year.  It gets all
  #'marfis catch of interest to the MPA project (i.e. landed, bait, discards, and
  #'dead discards)
  #'
  #'If the "entered" coordinate is not available, then the "determined" coordinate
  #'is used.
  #'If no value for latitude is available, if becomes 35.
  #'If no value for longitude is available, if becomes -50.
  #'Catches without assigned locations will ultimately be distributed against all
  #'observered catch locations proportionately.
  
  if (!is.null(spp)) {
    spp.tweak = paste0("AND SPECIES_CODE IN (",paste(spp, collapse = ","),")")
  }else{
    spp.tweak = ""
  }
  if (!is.null(years)) {
    years.tweak = paste0("AND to_char(DATE_FISHED,'YYYY') IN (",paste(years, collapse =","),")")
  }else{
    years.tweak = ""
  }
  
  query.raw = paste0(
    "
    SELECT * FROM
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
    -999 UNIT_OF_MEASURE_ID, --allkg
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
    --WHEN E.DET_NAFO_UNIT_AREA_ID IS NOT NULL
    --THEN c.LAT
    ELSE 35
    END, 4) LAT,
    ROUND(
    CASE WHEN E.ENT_LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(E.ENT_LONGITUDE, 1, 2) + SUBSTR(E.ENT_LONGITUDE, 3, 2) / 60 + SUBSTR(E.ENT_LONGITUDE, 5, 2) / 3600))
    WHEN E.DET_LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(E.DET_LONGITUDE, 1, 2) + SUBSTR(E.DET_LONGITUDE, 3, 2) / 60 + SUBSTR(E.DET_LONGITUDE, 5, 2) / 3600))
    WHEN P.LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(P.LONGITUDE, 1, 2) + SUBSTR(P.LONGITUDE, 3, 2) / 60 + SUBSTR(P.LONGITUDE, 5, 2) / 3600))
    --WHEN E.DET_NAFO_UNIT_AREA_ID IS NOT NULL
    --THEN c.LON
    ELSE -50
    END, 4) LON
    FROM MARFISSCI.pro_spc_info P
    INNER JOIN MARFISSCI.LOG_EFRT_STD_INFO E
    ON P.LOG_EFRT_STD_INFO_ID = E.LOG_EFRT_STD_INFO_ID
    --LEFT JOIN (
    --  SELECT a.AREA_ID, NAFO_centroids.lat, NAFO_centroids.lon
    --  FROM MARFISSCI.nafo_unit_areas a
    --  INNER JOIN mcmahonm.NAFO_centroids
    --  ON a.area = NAFO_centroids.NAFO_BEST) c
    --  ON E.DET_NAFO_UNIT_AREA_ID = c.area_id
    WHERE P.CATCH_USAGE_CODE = 10
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
    --don't have a reported weight
    -999 RPT_WEIGHT_KG,
    /*try desperately to position data*/
    ROUND(
    CASE WHEN E1.ENT_LATITUDE IS NOT NULL
    THEN SUBSTR(E1.ENT_LATITUDE, 1, 2) + SUBSTR(E1.ENT_LATITUDE, 3, 2) / 60 + SUBSTR(E1.ENT_LATITUDE, 5, 2) / 3600
    WHEN E1.DET_LATITUDE IS NOT NULL
    THEN SUBSTR(E1.DET_LATITUDE, 1, 2) + SUBSTR(E1.DET_LATITUDE, 3, 2) / 60 + SUBSTR(E1.DET_LATITUDE, 5, 2) / 3600
    --WHEN E1.DET_NAFO_UNIT_AREA_ID IS NOT NULL
    --THEN c1.LAT
    ELSE 35
    END, 4) LAT,
    ROUND(
    CASE WHEN E1.ENT_LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(E1.ENT_LONGITUDE, 1, 2) + SUBSTR(E1.ENT_LONGITUDE, 3, 2) / 60 + SUBSTR(E1.ENT_LONGITUDE, 5, 2) / 3600))
    WHEN E1.DET_LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(E1.DET_LONGITUDE, 1, 2) + SUBSTR(E1.DET_LONGITUDE, 3, 2) / 60 + SUBSTR(E1.DET_LONGITUDE, 5, 2) / 3600))
    --WHEN E1.DET_NAFO_UNIT_AREA_ID IS NOT NULL
    --THEN c1.LON
    ELSE -50
    END, 4) LON
    FROM MARFISSCI.LOG_SPC_STD_INFO S1
    INNER JOIN MARFISSCI.LOG_EFRT_STD_INFO E1
    ON S1.LOG_EFRT_STD_INFO_ID = E1.LOG_EFRT_STD_INFO_ID
    --LEFT JOIN (
    --  SELECT a.AREA_ID, NAFO_centroids.lat, NAFO_centroids.lon
    --  FROM MARFISSCI.nafo_unit_areas a
    --  INNER JOIN mcmahonm.NAFO_centroids
    --  ON a.area = NAFO_centroids.NAFO_BEST) c1
    --  ON E1.DET_NAFO_UNIT_AREA_ID = c1.area_id
    -- not 'landed' or 'live discard'
    WHERE S1.CATCH_USAGE_CODE NOT IN (10,50)
    )
    WHERE 1 = 1
    ", spp.tweak,"
    ", years.tweak,"
    "
  )
  data.raw = sqlQuery(channel,query.raw)
  
  #make a descriptive name so we know what we've got
  if (range(spp)[1] == range(spp)[2]) {
    spp.file = range(spp)[1]
  }else{
    spp.file = paste(range(spp),collapse = "_")
  }
  if (range(years)[1] == range(years)[2]) {
    years.file = range(years)[1]
  }else{
    years.file = paste(range(years),collapse = "_")
  }
  file.output = paste0(project.datadirectory("mpa"),"/csv/raw_",years.file,"_",spp.file,".csv")
  write.csv(data.raw, file.output, row.names = F)
  message = paste0("CSV written to ",file.output)
  rm(data.raw)
  odbcClose(channel)
  return(message)
}
#e.g. marfissci.get.data(spp= c(612), years=c(2010))