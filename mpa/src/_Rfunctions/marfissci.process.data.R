marfissci.process.data <- function(df, agg.minutes=2, 
                                   agg.field = "RND_WEIGHT_KGS",
                                   agg.by = "SPECIES_CODE",
                                   agg.by.year =T,
                                   save.RDS = T){
  #' MMM - Apr 1, 2016
  #' This function takes the input raw MARFIS data and summarizes it at "agg" 
  #' fraction of a degree.  "agg" should correspond with the final desired 
  #' aggregation level (or some smaller, whole fraction of it).  
  #' 
  #' Because of the aggregation that occurs in this function, much detail will 
  #' be lost - the original coordinates are rounded and the data contained in 
  #' the column 'agg.field' is grouped by the field specified in "agg.by".
  #' The mean, count annd sum is calculated for all "agg.field" values that
  #' contributed to the aggregated position.  
  
  agg = agg.minutes/60
  
  writeLines(paste0("Aggregating data to ",agg.minutes," minutes of a degree...
                    "))
  if (agg.by.year){
    library(lubridate) 
    df$YEAR_FISHED=year(df$DATE_FISHED)
  }
  df$CNT = 1
  df$VALIDITY = "VALID"  #default validity assumed good - overwrite if otherwise
  
  writeLines("Identifying positionless/non-marine data...
             ")
  #flag data with unknown positions (assigned 0N,0W by marfissci.get.data)
  df$VALIDITY[which(df$LAT == 0 & df$LON == 0)] = "NO COORDS" 
  
  #SPATIAL IDENTIFICATION OF IMPROPERLY POSITIONED CATCH (i.e. on land)
  #use coastline to find those points that are inland 
  #Anything other than NA is inland
 
  proj.metric = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 
                 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'
  
  if (!exists("coast")) {
    loadfunctions("coastline")
    writeLines("Building the coastline...
               ")
    coast <<- coastline.db( DS="gshhg coastline highres", 
                          crs="+proj=longlat +datum=WGS84", 
                          p=NULL, level=1, xlim=NULL, ylim=NULL )
    coast.aea<<-spTransform(coast,CRS(proj.metric))
    
  }
  buff = agg*78847  #meters in 1 degree of longitude @45N 
  writeLines(paste0("Buffering the coastline to ",round(buff/1000,1), " km... 
"))
  coast.aea.buff <- gBuffer(coast.aea, byid=TRUE, width=-1*buff)
  writeOGR(spatpoly_to_spatpolydf(coast.aea.buff),dsn='.', layer='buffCheck', driver='ESRI Shapefile', overwrite_layer=TRUE)
  
  df.sp = df #create temporary spatial df for terrestrial check
  coordinates(df.sp) = c("LON", "LAT")
  proj4string(df.sp) = CRS("+proj=longlat +datum=WGS84")
  df.sp.proj.metric= spTransform(df.sp,CRS(proj.metric))
  writeLines("Finding points that exceed the allowable distance from the sea for this 
aggregation level, and marking them as 'INVALID'.  At least some part of the 
final cell must overlay the ocean, and beyond this distance, none will.
            ")
  df.sp.proj.metric@data$INLAND =over(df.sp.proj.metric,coast.aea.buff)
  df = cbind(df,INLAND = df.sp.proj.metric@data$INLAND)
  df$VALIDITY[which(!is.na(df$INLAND))] = "INLAND" 
  df$INLAND = NULL

  #  round the coordinates to the desired aggregation so we can group by the new 
  # 'points', giving a lot less data to process
  df$LATAGG = (round(df$LAT/agg)*agg)-0.25*agg
  df$LONAGG = (round(df$LON/agg)*agg)+0.25*agg

  if(NROW(df[df$VALIDITY!="VALID",])>0) {
    writeLines(paste0(
NROW(df[df$VALIDITY!="VALID",]), " of ", NROW(df)," the total records (a sum of ",sum(df[df$VALIDITY!="VALID",][,c(agg.field)])," of the 
aggregated field) were poorly positioned.  These had either no positions, or were 
determined to be terrestrial.  This total amount will be applied proportionately 
across all other validly positioned data in a new field called 'WEIGHTED'
"))
  }  
  #Beyond this step, we will lose all initial details - e.g. catch usages, 
  #reported units, gear types etc will be combined
  if (agg.by.year){
  df.agg = as.data.frame(as.list(aggregate(list("AGG_FIELD"=df[,c(agg.field)]), 
                            by=list(
                              YEAR_FISHED = df$YEAR_FISHED, 
                              AGG_BY_FIELD = df[,c(agg.by)], 
                              VALIDITY = df$VALIDITY, 
                              LAT = df$LATAGG, 
                              LON = df$LONAGG), 
                            FUN=function(x) c(MEAN =round(mean(x),4), 
                                              CNT=round(length(x),4), 
                                              SUM = round(sum(x),4))
                    )))
  }else{
    df.agg = as.data.frame(as.list(aggregate(list("AGG_FIELD"=df[,c(agg.field)]), 
                                             by=list(
                                               AGG_BY_FIELD = df[,c(agg.by)], 
                                               VALIDITY = df$VALIDITY, 
                                               LAT = df$LATAGG, 
                                               LON = df$LONAGG), 
                                             FUN=function(x) c(MEAN =round(mean(x),4), 
                                                               CNT=round(length(x),4), 
                                                               SUM = round(sum(x),4))
    )))
  }
  #rm(df)
  valid.data=df.agg[df.agg$VALIDITY=="VALID",]
  invalid.data = df.agg[!df.agg$VALIDITY=="VALID",]
  
  #get the proportion of the total sum of all attributable to each (valid) loc
  valid.data$PROP = prop.table(as.table(as.matrix(valid.data$AGG_FIELD.SUM)))[,1]
  
  invalid.data$PROP = 0
  invalid.total = sum(invalid.data$AGG_FIELD.SUM)
     
  valid.data$WEIGHTED = (valid.data$PROP * invalid.total) + valid.data$AGG_FIELD.SUM
  invalid.data$WEIGHTED = 0
  
  df.agg = rbind(valid.data,invalid.data)
  df.agg$PROP = NULL
  rm(valid.data)
  rm(invalid.data)
  
  if (save.RDS) {
    #make a descriptive name so we know what we've got
    if (range(df.agg$AGG_BY_FIELD)[1] == range(df.agg$AGG_BY_FIELD)[2]) {
      agg.file = range(df.agg$AGG_BY_FIELD)[1]
    }else{
      agg.file = paste(range(df.agg$AGG_BY_FIELD),collapse = "_")
    }
    if (agg.by.year){
      if (range(df.agg$YEAR_FISHED)[1] == range(df.agg$YEAR_FISHED)[2]) {
        years.file = range(df.agg$YEAR_FISHED)[1]
      }else{
        years.file = paste(range(df.agg$YEAR_FISHED),collapse = "_")
      }
    }else{
      years.file =""
    }
    agg.amt=gsub("\\.","_",agg)
    the.filename = paste0("pts_",agg.amt,"_",years.file,"_",agg.file)
    
    df.agg.sp = df.agg
    coordinates(df.agg.sp) = c("LON", "LAT")
    proj4string(df.agg.sp) = CRS("+proj=longlat +datum=WGS84")
    saveRDS(df.agg.sp, file=paste0(project.datadirectory("mpa"),"/rds/",the.filename,".rds"))
    writeLines(paste0("rds file written to ",project.datadirectory("mpa"),"/rds/",the.filename,".rds"))
    }
  return(df.agg)
}
#df = read.csv(paste0(project.datadirectory("mpa"),"/csv/raw_2010_612.csv"))
#test = marfissci.process.data(df, agg.minutes=2, agg.by="SPECIES_CODE", save.RDS=F)