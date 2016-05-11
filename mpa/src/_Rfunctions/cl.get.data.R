cl.get.data = function(spp = NULL, gear=NULL, years = NULL, save.csv=T){
  library(RODBC)
  channel <- odbcConnect("PTRAN",uid = oracle.cl.username,pwd = oracle.cl.password)
  
  if (!is.null(spp)) {
    spp.tweak = paste0("AND SPECIES IN (",paste(spp, collapse = ","),")")
  }else{
    spp.tweak = ""
  }
  if (!is.null(gear)) {
    gear.tweak = paste0("AND GEARCODE IN (",paste(gear, collapse = ","),")")
  }else{
    gear.tweak = ""
  }
  if (!is.null(years)) {
    years.tweak = paste0("AND substr(DATELAND,1, 4) IN (",paste(years, collapse =","),")")
  }else{
    years.tweak = ""
  }
  
  query.raw = paste0(
    "SELECT Z.LATITUDE LAT,
  Z.LONGITUDE LON,
  Z.CTCHDATE,
  Z.YEAR,
  Z.LIVE_WT,
  Z.SPECIES SPECIES_CODE,
  Z.GEARCODE GEAR_CODE
  FROM ZIFDB Z
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
    file.output = paste0(project.datadirectory("mpa"),"/cl/raw_data/cl_raw",years.file,gear.file,spp.file,".csv")
    write.csv(data.raw, file.output, row.names = F)
    print(paste0("CSV written to ",file.output))
  }
  odbcClose(channel)
  return(data.raw)
  
}
# years=seq(from = 1986, to = 2002) #2002 represents an overlap with marfissci
# #years=c(1986,1987)
# for (i in 1:length(years)){
#   cl.get.data(years=years[i])
# }