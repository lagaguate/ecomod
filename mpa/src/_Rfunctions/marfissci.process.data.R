marfissci.process.data <- function(df, write.shp=T, agg=0.05){
  #' MMM - March 22, 2016
  #' This function takes the input raw MARFIS data and summarizes it at "agg" 
  #' fraction of a degree.  "agg" should correspond with the final desired 
  #' aggregation level (or some smaller, whole fraction of it).  
  #' 
  #' Because of the aggregation that occurs in this function, much detail will 
  #' be lost - the original data is rounded and grouped so that there should 
  #' only be one data point per cell.  If details beyond species and year are 
  #' required (e.g. Gear types, effort, catch_usage, etc), those analyses should
  #' be done first.
  #'
  #'For Mike's convenience (and Jae's inconvenience), the function currently 
  #'generates a shapefile on command
  #'
  writeLines(paste0("Aggregating data to ",agg," of a degree"))
  library(lubridate) 
  library(sp)
  loadfunctions("coastline")
  df$YEAR_FISHED=year(df$DATE_FISHED)
  
  writeLines("Identifying problematic data...")
  #identify, capture and remove data with unknown positions (assigned 35N,-50W)
  nocoord.catch = df[df$LAT == 35 & df$LON == -50,] 
  nocoord.catch$INLAND = -999 #add this column for later
  
  writeLines(paste0(nrow(nocoord.catch)," of ",nrow(df)," records did not have valid coordinates.\nThese represent ",sum(nocoord.catch$RND_WEIGHT_KGS)," kgs."))
  df = df[!(df$LOG_EFRT_STD_INFO_ID %in% nocoord.catch$LOG_EFRT_STD_INFO_ID),]
  
  #SPATIAL IDENTIFICATION OF IMPROPERLY POSITIONED CATCH (i.e. on land)
  #use coastline to find those points that are inland 
  #Anything other than NA is inland
  writeLines(paste0("Verifying that the remaining ",nrow(df)," reported positions are in the ocean..."))
  coast = coastline.db( DS="gshhg coastline highres", crs="+proj=longlat +datum=WGS84", p=NULL, level=1, xlim=NULL, ylim=NULL )
    
  df.sp = df #create temporary spatial df for terrestrial check
  coordinates(df.sp) = c("LON", "LAT")
  proj4string(df.sp) = CRS("+proj=longlat +datum=WGS84")
  df.sp@data$INLAND =over(df.sp,coast)
  df = cbind(df,INLAND = df.sp@data$INLAND)
  terr.catch = df[!is.na(df$INLAND),] 
  writeLines(paste0(nrow(terr.catch), " of ", nrow(df) ," records were on land.\nThese represent ",sum(terr.catch$RND_WEIGHT_KGS)," kgs."))
  
  df = df[!(df$LOG_EFRT_STD_INFO_ID %in% terr.catch$LOG_EFRT_STD_INFO_ID),]
  
  invalidcoord.catch = rbind(nocoord.catch, terr.catch)
  rm(nocoord.catch)
  rm(terr.catch)
  
  writeLines(paste0("Summarizing  remaining ",nrow(df)," of (valid) data to ",agg," of a degree..."))
  #  round the coordinates to the desired aggregation so we can group by the new 
  # 'points', giving a lot less data to process
  df$LATAGG = (round(df$LAT/agg)*agg)+0.5*agg
  df$LONAGG = (round(df$LON/agg)*agg)-0.5*agg
  
  #Beyond this step, we will lose all initial details - e.g. catch usages, 
  #reported units, gear types etc will be combined
  df.agg = aggregate(list("RND_WEIGHT_KGS"=df[,c("RND_WEIGHT_KGS")]), 
                       by=list(YEAR_FISHED = df$YEAR_FISHED, 
                               #                   GEAR_CODE = df$GEAR_CODE,
                               #                   CATCH_USAGE_CODE = df$CATCH_USAGE_CODE,
                               #                   UNIT_OF_MEASURE_ID = df$UNIT_OF_MEASURE_ID,
                               SPECIES_CODE = df$SPECIES_CODE, 
                               LAT = df$LATAGG, 
                               LON = df$LONAGG), 
                       FUN="sum")
  rm(df)
  writeLines(paste0("Adding the ",sum(invalidcoord.catch$RND_WEIGHT_KGS)," kg of catch from poorly positioned data proportionately\nto the ",sum(df.agg$RND_WEIGHT_KGS)," kgs from valid catch locations"))
  #determine the proportion of the rnd weight in each cell
  df.agg$PROP = prop.table(as.table(as.matrix(df.agg[,5])))
  df.agg$PROP_RND_WEIGHT_KGS = (sum(invalidcoord.catch$RND_WEIGHT_KGS)*df.agg$PROP)+df.agg$RND_WEIGHT_KGS
  df.agg$PROP = df.agg$PROP[,1]
  df.agg$PROP_RND_WEIGHT_KGS = df.agg$PROP_RND_WEIGHT_KGS[,1]
  #make a descriptive name so we know what we've got
  if (range(df.agg$SPECIES_CODE)[1] == range(df.agg$SPECIES_CODE)[2]) {
    spp.file = range(df.agg$SPECIES_CODE)[1]
  }else{
    spp.file = paste(range(df.agg$SPECIES_CODE),collapse = "_")
  }
  if (range(df.agg$YEAR_FISHED)[1] == range(df.agg$YEAR_FISHED)[2]) {
    years.file = range(df.agg$YEAR_FISHED)[1]
  }else{
    years.file = paste(range(df.agg$YEAR_FISHED),collapse = "_")
  }
  agg.file=gsub("\\.","_",agg)
  the.filename = paste0("pts_",agg.file,"_",years.file,"_",spp.file)
  
  if (write.shp) {
    library(rgdal)
    #browser()
    df.agg.sp = df.agg
    coordinates(df.agg.sp) = c("LON", "LAT")
    proj4string(df.agg.sp) = CRS("+proj=longlat +datum=WGS84")
     writeOGR(df.agg.sp, dsn = paste0(project.datadirectory("mpa"),"/shapes"), layer = the.filename, driver = "ESRI Shapefile", overwrite_layer = T)
    message = paste0("Shapefile written to ",project.datadirectory("mpa"),"/shapes/",the.filename,".shp")
  }else{
    write.csv(df.agg, paste0(project.datadirectory("mpa"),"/csv/",the.filename,".csv"), row.names = F)
    message = paste0("CSV written to ",project.datadirectory("mpa"),"/csv/",the.filename,".csv")
  }
  rm(df.agg)
  return(message)
}
#df = read.csv(paste0(project.datadirectory("mpa"),"/csv/raw_2010_612.csv"))
#marfissci.process.data(df, write.shp=T, agg = 0.05)