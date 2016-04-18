marfissci.process.data <- function(df, agg.minutes=2, 
                                   agg.field = "RND_WEIGHT_KGS",
                                   agg.by = "SPECIES_CODE",
                                   agg.by.year =T,
                                   save.RDS = T,
                                   save.CSV = F,
                                   save.SHP = F,
                                   output = "RDS"){
  #' MMM - Apr 6, 2016
  #' This function takes the input raw MARFIS data and summarizes it at 
  #' "agg.minutes" of a degree.  The default output is a (spatially-enabled) rds file 
  #' which can be plotted in R without any additional processing.  By default, 
  #' the rds file is saved locally, and csv and shapefiles can also be generated 
  #' if desired. Alternative, a csv file can be output instead.
  #' 
  #' Because of the aggregation that occurs in this function, much detail will 
  #' be lost - the original coordinates are rounded and the data contained in 
  #' the column 'agg.field' is grouped by the field specified in "agg.by".
  #' The mean, count annd sum is calculated for all "agg.field" values that
  #' contributed to the aggregated position.  
  
  agg = agg.minutes/60
  writeLines(paste0("Aggregating data to ",agg.minutes," minutes of a degree...
                    "))
  #SPATIAL IDENTIFICATION OF IMPROPERLY POSITIONED CATCH (i.e. on land)
  #use coastline to find those points that are inland 
  #Anything other than NA is inland
  proj.metric = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 
                 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m'
  if (!exists("coast.aea")) {
    loadfunctions("coastline")
    writeLines("Building the coastline...
               ")
    coast.aea <<- coastline.db( DS="gshhg coastline highres", 
                            crs=proj.metric, 
                            p=NULL, level=1, xlim=NULL, ylim=NULL )
    #coast.aea<<-spTransform(coast,CRS(proj.metric))
  }
  buff = agg*78847  #meters in 1 degree of longitude @45N 
  if (!exists("coast.aea.buff")) {
    writeLines(paste0("With the selected aggregation level (",agg.minutes," minutes), the coastline is being 
buffered internally by ",round(buff/1000,1), " km.  Data for this project is centered around ~45N, 
and at this latitude, the aggregate values for any points within this buffer 
will still overlay the ocean.  Beyond this distance, aggregate points will lie 
entirely on the land. 
"))
    coast.aea.buff <<- gBuffer(coast.aea, byid=TRUE, width=-1*buff)
  }
 
    library(lubridate) 
    df$YEAR_FISHED=year(df$DATE_FISHED)

  df$VALIDITY = "VALID"  #default validity assumed good - overwrite if otherwise
  #flag data with unknown positions (assigned 0N,0W by marfissci.get.data
  noNA = c(c("LAT","LON"),agg.field) # fields where NA will be replaced with 0
  df[noNA][is.na(df[noNA])] <- 0
  
  df$VALIDITY[which(df$LAT == 0 | df$LON == 0)] = "NO COORDS"
#   df$VALIDITY[which(is.null(df$LAT) | is.null(df$LON))] = "NO COORDS" 
#   df$VALIDITY[which(is.na(df$LAT) | is.na(df$LON))] = "NO COORDS" 
  #create temporary spatial df for terrestrial check - this loses the lat and 
  #lon fields, so we add them back in for later convenience
  df.sp = df[df$VALIDITY=='VALID',] 
  coordinates(df.sp) = c("LON", "LAT")
  proj4string(df.sp) = CRS("+proj=longlat +datum=WGS84")
  df.sp@data$LON = coordinates(df.sp)[,1]
  df.sp@data$LAT = coordinates(df.sp)[,2]
  df.sp.proj.metric= spTransform(df.sp,CRS(proj.metric))
  writeLines(paste0("Finding points that exceed the allowable distance from the sea for this 
aggregation level (",round(buff/1000,1), " km), and marking them as 'INVALID'.
"))
   df.sp.proj.metric@data$INLAND =over(df.sp.proj.metric,coast.aea.buff)
#   df = cbind(df,INLAND = df.sp.proj.metric@data$INLAND)

  df.sp.proj.metric@data$VALIDITY[which(!is.na(df.sp.proj.metric@data$INLAND))] = "INLAND" 
  df.sp.proj.metric@data$INLAND = NULL
  #reorder columns for imminent rbind
  df.sp.proj.metric@data = df.sp.proj.metric@data[,c(1:12,16,15,13,14)]
  df = rbind(df[!df$VALIDITY=='VALID',],df.sp.proj.metric@data)
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
  }else{
    writeLines(paste0("No invalid data was found (i.e. all positions were reasonably close to
the ocean). Despite this, a new field called 'WEIGHTED' will be output, but will 
simply be populated with identical values as ",agg.field))
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
  
  
  if(NROW(df[df$VALIDITY!="VALID",])>0) { 
    valid.data=df.agg[df.agg$VALIDITY=="VALID",]
    #get the proportion of the total sum of all attributable to each (valid) loc
    valid.data$PROP = prop.table(as.table(as.matrix(valid.data$AGG_FIELD.SUM)))[,1]
    invalid.data = df.agg[!df.agg$VALIDITY=="VALID",]
    invalid.data$PROP = 0
    invalid.total = sum(invalid.data$AGG_FIELD.SUM)
    valid.data$WEIGHTED = (valid.data$PROP * invalid.total) + valid.data$AGG_FIELD.SUM
    invalid.data$WEIGHTED = 0
    df.agg = rbind(valid.data,invalid.data)
    df.agg$PROP = NULL
    rm(valid.data)
    rm(invalid.data)
  } else {
    df.agg$PROP = df.agg$AGG_FIELD.SUM
  }
  
  
  #make a descriptive name so we know what we've got
  if (range(df.agg$AGG_BY_FIELD)[1] == range(df.agg$AGG_BY_FIELD)[2]) {
    agg.file = range(df.agg$AGG_BY_FIELD)[1]
  }else{
    agg.file = paste(range(df.agg$AGG_BY_FIELD),collapse = "_")
  }
  if (agg.by.year){
    if (range(df.agg$YEAR_FISHED)[1] == range(df.agg$YEAR_FISHED)[2]) {
      years.file = paste0(range(df.agg$YEAR_FISHED)[1],"_")
    }else{
      years.file = paste0(paste(range(df.agg$YEAR_FISHED),collapse = "_"),"_")
    }
  }else{
    years.file =""
  }
  agg.amt=paste0(agg.minutes,"min_")
  the.filename = paste0("pts_",agg.amt,years.file,agg.file)

  #do any field renaming before generating files
  colnames(df.agg)[colnames(df.agg) == 'AGG_BY_FIELD'] <- agg.by
  colnames(df.agg) <- gsub("AGG_FIELD\\.", "", colnames(df.agg))
  colnames(df.agg)[colnames(df.agg) == 'MEAN'] <- paste0("MEAN_",agg.field)
  colnames(df.agg)[colnames(df.agg) == 'CNT'] <- paste0("CNT_",agg.field)
  colnames(df.agg)[colnames(df.agg) == 'SUM'] <- paste0("SUM_",agg.field) 
  
  df.agg.sp = df.agg
  coordinates(df.agg.sp) = c("LON", "LAT")
  proj4string(df.agg.sp) = CRS("+proj=longlat +datum=WGS84")

  
  if (save.RDS) {
    saveRDS(df.agg.sp, file=paste0(project.datadirectory("mpa"),"/rds/",the.filename,".rds"))
    writeLines(paste0("rds file written to ",project.datadirectory("mpa"),"/rds/",the.filename,".rds"))
  }
  if (save.CSV) {
    write.csv(df.agg,file=paste0(project.datadirectory("mpa"),"/csv/aggregated/",the.filename,".csv"))
    writeLines(paste0("csv file written to ",project.datadirectory("mpa"),"/csv/aggregated/",the.filename,".csv"))
  }
  if(save.SHP) {
    writeOGR(df.agg.sp,dsn=paste0(project.datadirectory("mpa"),"/shapes"), layer=the.filename, driver='ESRI Shapefile', overwrite_layer=TRUE)
    writeLines(paste0("shp file written to ",project.datadirectory("mpa"),"/shapes/",the.filename,".*"))
  }
  if (output == "RDS"){
    the.output = df.agg.sp
  } else{
    the.output = df.agg
  }
    return(the.output)
}
#df = read.csv(paste0(project.datadirectory("mpa"),"/csv/raw_2010_612.csv")) #gets raw data
#test = marfissci.process.data(df, agg.minutes=2, agg.by="SPECIES_CODE", save.RDS=F) #aggregates it