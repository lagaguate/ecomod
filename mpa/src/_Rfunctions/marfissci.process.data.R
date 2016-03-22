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
  library(lubridate) 
  library(sp) 
  df$YEAR_FISHED=year(df$DATE_FISHED)
  df.sp = df
  
  #SPATIAL IDENTIFICATION OF IMPROPERLY POSITIONED CATCH (i.e. 
  #on land)
  #use coastline to find those points that are inland 
  #Assign it to 35N,-50W (and apply agg rounding)
  print("Verifying that data is a valid, marine location...")
  coast = coastline.db( p=p, DS=" gshhg coastline highres", no.clip=TRUE )
  coast = spTransform(coast, CRS("+proj=longlat +datum=WGS84"))
  coordinates(df.sp) = c("LON", "LAT")
  proj4string(df.sp) = CRS("+proj=longlat +datum=WGS84")
  df.sp@data$INLAND =over(df.sp,coast) #Anything other than NA is inland
  df = cbind(df,INLAND = df.sp@data$INLAND)
  df[!is.na(df$INLAND),]$LAT = -50
  df[!is.na(df$INLAND),]$LON = 35
  rm(df.sp)
  print("...Continuing Analysis")
  
  
  #  round the coordinates to the desired aggregation so we can group by the new 
  # 'points', giving a lot less data to process
  df$LATAGG = (round(df$LAT/agg)*agg)+0.5*agg
  df$LONAGG = (round(df$LON/agg)*agg)-0.5*agg
  
  #Beyond this step, we will lose all initial details - e.g. catch usages, 
  #reported units, gear types etc will be combined
  
  data.agg = aggregate(list("RND_WEIGHT_KGS"=df[,c("RND_WEIGHT_KGS")]), 
                       by=list(YEAR_FISHED = df$YEAR_FISHED, 
                               #                   GEAR_CODE = df$GEAR_CODE,
                               #                   CATCH_USAGE_CODE = df$CATCH_USAGE_CODE,
                               #                   UNIT_OF_MEASURE_ID = df$UNIT_OF_MEASURE_ID,
                               SPECIES_CODE = df$SPECIES_CODE, 
                               LAT = df$LATAGG, 
                               LON = df$LONAGG), 
                       FUN="sum")
 # rm(df)
   
  #identify, capture and remove data with unknown positions (assigned 35N,-50W)
  orphaned.weight=data.agg[data.agg$LAT == (round(35/agg)*agg)+0.5*agg & data.agg$LON == (round(-50/agg)*agg)-0.5*agg,]$RND_WEIGHT_KGS 
  data.agg=data.agg[!(data.agg$LAT == (round(35/agg)*agg)+0.5*agg & data.agg$LON == (round(-50/agg)*agg)-0.5*agg),] 
  
  #determine the proportion of the rnd weight in each cell
  data.agg$PROP = prop.table(as.table(as.matrix(data.agg[,5])))
  data.agg$PROP_RND_WEIGHT_KGS = (orphaned.weight*data.agg$PROP)+data.agg$RND_WEIGHT_KGS
 
  #make a descriptive name so we know what we've got
  if (range(data.agg$SPECIES_CODE)[1] == range(data.agg$SPECIES_CODE)[2]) {
    spp.file = range(data.agg$SPECIES_CODE)[1]
  }else{
    spp.file = paste(range(data.agg$SPECIES_CODE),collapse = "_")
  }
  if (range(data.agg$YEAR_FISHED)[1] == range(data.agg$YEAR_FISHED)[2]) {
    years.file = range(data.agg$YEAR_FISHED)[1]
  }else{
    years.file = paste(range(data.agg$YEAR_FISHED),collapse = "_")
  }
  agg.file=gsub("\\.","_",agg)
  the.filename = paste0("pts_",agg.file,"_",years.file,"_",spp.file)
  
  if (write.shp) {
    library(rgdal)
    
    data.agg.sp = data.agg
    coordinates(data.agg.sp) = c("LON", "LAT")
    proj4string(data.agg.sp) = CRS("+proj=longlat +datum=WGS84")
     writeOGR(data.agg.sp, dsn = paste0(project.datadirectory("mpa"),"/shapes"), layer = the.filename, driver = "ESRI Shapefile", overwrite_layer = T)
    message = paste0("Shapefile written to ",filename,".shp")
  }else{
    write.csv(data.agg, paste0(the.filename,".csv"), row.names = F)
    message = paste0("CSV written to ",the.filename,".csv")
  }
  rm(data.agg)
  return(message)
}
#df = read.csv(paste0(project.datadirectory("mpa"),"/csv/raw_2010_612.csv"))
#marfissci.process.data(df, write.shp=T, agg = 0.05)