marfissci.get.data <- function(spp = NULL, gear=NULL, years = NULL, get.nonlandings=T, save.csv=T) {
  library(RODBC)
  channel <- odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
  #'MMM March 31, 2016
  #'This is a marfissci extraction that can limit results by species, gear(s) 
  #'and year(s).  By default, it gets all removed biomass, which includes catch
  #'that is defined seperately from typical "landings" (i.e. bait, discards, and
  #'dead discards).  This can be disabled by setting get.nonlandings to F.  Also 
  #'by default, this saves the output to a csv.  If this is not desired, you can 
  #'change save.csv to F.
  #'
  #'Marfis is valid from 2002 forwards.
  #'
  #'If the "entered" coordinate is not available, then the "determined" coordinate
  #'is used as the position for the data.  If no value for latitude is 
  #'available, if becomes 0, and if no value for longitude is available, if 
  #'becomes 0.  
  
  if (!is.null(spp)) {
    spp.tweak = paste0("AND SPECIES_CODE IN (",paste(spp, collapse = ","),")")
  }else{
    spp.tweak = ""
  }
  if (!is.null(gear)) {
    gear.tweak = paste0("AND GEAR_CODE IN (",paste(gear, collapse = ","),")")
  }else{
    gear.tweak = ""
  }
  if (!is.null(years)) {
    years.tweak = paste0("AND to_char(DATE_FISHED,'YYYY') IN (",paste(years, collapse =","),")")
  }else{
    years.tweak = ""
  }
  if (get.nonlandings==T) {
    catch.usage.tweak = "UNION
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
    THEN S1.WEIGHT * 0.453592 #lbs->kg
    WHEN S1.UNIT_OF_MEASURE_ID = 30
    THEN S1.WEIGHT / 1000    #metric tons->kg
    END, 2) WEIGHT_KG,
    -999 RPT_WEIGHT_KG,
    /*try to position data*/
    ROUND(
    CASE WHEN E1.ENT_LATITUDE IS NOT NULL
    THEN SUBSTR(E1.ENT_LATITUDE, 1, 2) + SUBSTR(E1.ENT_LATITUDE, 3, 2) / 60 + SUBSTR(E1.ENT_LATITUDE, 5, 2) / 3600
    WHEN E1.DET_LATITUDE IS NOT NULL
    THEN SUBSTR(E1.DET_LATITUDE, 1, 2) + SUBSTR(E1.DET_LATITUDE, 3, 2) / 60 + SUBSTR(E1.DET_LATITUDE, 5, 2) / 3600
    ELSE 0
    END, 4) LAT,
    ROUND(
    CASE WHEN E1.ENT_LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(E1.ENT_LONGITUDE, 1, 2) + SUBSTR(E1.ENT_LONGITUDE, 3, 2) / 60 + SUBSTR(E1.ENT_LONGITUDE, 5, 2) / 3600))
    WHEN E1.DET_LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(E1.DET_LONGITUDE, 1, 2) + SUBSTR(E1.DET_LONGITUDE, 3, 2) / 60 + SUBSTR(E1.DET_LONGITUDE, 5, 2) / 3600))
    --WHEN E1.DET_NAFO_UNIT_AREA_ID IS NOT NULL
    --THEN c1.LON
    ELSE 0
    END, 4) LON
    FROM MARFISSCI.LOG_SPC_STD_INFO S1
    INNER JOIN MARFISSCI.LOG_EFRT_STD_INFO E1
    ON S1.LOG_EFRT_STD_INFO_ID = E1.LOG_EFRT_STD_INFO_ID
    -- not 'landed' or 'live discard'
    WHERE S1.CATCH_USAGE_CODE NOT IN (10,50)"
  }else{
    catch.usage.tweak = ""
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
    /*try to position data*/
    ROUND(
    CASE WHEN E.ENT_LATITUDE IS NOT NULL
    THEN SUBSTR(E.ENT_LATITUDE, 1, 2) + SUBSTR(E.ENT_LATITUDE, 3, 2) / 60 + SUBSTR(E.ENT_LATITUDE, 5, 2) / 3600
    WHEN E.DET_LATITUDE IS NOT NULL
    THEN SUBSTR(E.DET_LATITUDE, 1, 2) + SUBSTR(E.DET_LATITUDE, 3, 2) / 60 + SUBSTR(E.DET_LATITUDE, 5, 2) / 3600
    WHEN P.LATITUDE IS NOT NULL
    THEN SUBSTR(P.LATITUDE, 1, 2) + SUBSTR(P.LATITUDE, 3, 2) / 60 + SUBSTR(P.LATITUDE, 5, 2) / 3600
    ELSE 0
    END, 4) LAT,
    ROUND(
    CASE WHEN E.ENT_LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(E.ENT_LONGITUDE, 1, 2) + SUBSTR(E.ENT_LONGITUDE, 3, 2) / 60 + SUBSTR(E.ENT_LONGITUDE, 5, 2) / 3600))
    WHEN E.DET_LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(E.DET_LONGITUDE, 1, 2) + SUBSTR(E.DET_LONGITUDE, 3, 2) / 60 + SUBSTR(E.DET_LONGITUDE, 5, 2) / 3600))
    WHEN P.LONGITUDE IS NOT NULL
    THEN (-1 * (SUBSTR(P.LONGITUDE, 1, 2) + SUBSTR(P.LONGITUDE, 3, 2) / 60 + SUBSTR(P.LONGITUDE, 5, 2) / 3600))
    ELSE 0
    END, 4) LON
    FROM MARFISSCI.pro_spc_info P
    INNER JOIN MARFISSCI.LOG_EFRT_STD_INFO E
    ON P.LOG_EFRT_STD_INFO_ID = E.LOG_EFRT_STD_INFO_ID
    WHERE P.CATCH_USAGE_CODE = 10
    ", catch.usage.tweak , "
    )
    WHERE 1 = 1
    ", spp.tweak,"
    ", gear.tweak,"
    ", years.tweak,"
    "
  )
  data.raw = sqlQuery(channel,query.raw)
  
  if (save.csv==T){
    #make a descriptive name so we know what we've got
    if (is.null(spp)){
      spp.file = ""
    }else if(range(spp)[1] == range(spp)[2]) {
      spp.file = paste0("_",range(spp)[1])
    }else{
      spp.file = paste0("_",paste(range(spp),collapse = "_"))
    }
    if (is.null(gear)){
      gear.file = ""
    }else if (range(gear)[1] == range(gear)[2]) {
      gear.file = paste0("_",range(gear)[1])
    }else{
      gear.file = paste0("_",paste(range(gear),collapse = "_"))
    }
    if (is.null(years)){
      years.file = ""
    }else if (range(years)[1] == range(years)[2]) {
      years.file = paste0("_",range(years)[1])
    }else{
      years.file = paste0("_",paste(range(years),collapse = "_"))
    }
    file.output = paste0(project.datadirectory("mpa"),"/marfissci/raw_data/",years.file,gear.file,spp.file,".csv")
    write.csv(data.raw, file.output, row.names = F)
    print(paste0("CSV written to ",file.output))
  }
  odbcClose(channel)
  return(data.raw)
}
#e.g. marfissci.get.data(spp=NULL, gear=51, years=2008, get.nonlandings=T, save.csv=T)
# for (i in 2002:2016){
#   marfissci.get.data(years=i)
# }