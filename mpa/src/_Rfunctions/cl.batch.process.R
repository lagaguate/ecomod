cl.batch.process <- function(folder=file.path(project.datadirectory("mpa"),"cl","raw_data"), 
                                    out.folder="cl",
                                    combine=F){
  #' The purpose of this batch process function is to facilitate the mass
  #' generation of data products from marfis data.  It assumes that data has 
  #' been extracted via marfissci.get.data(), and the resultant csv file(s) are
  #' saved locally.
  #' 
  #' When called, this batch function will allow the user to select what data 
  #' (i.e. years, species, and/or gears) should be aggregated together.  It will
  #' then generate rds files and figures for all the data.
  start.time <- Sys.time()  
  
  do.it = function(all.data){
    
    # if (range(all.data$YEAR)[1] == range(all.data$YEAR)[2]) {
      years.file = median(all.data$YEAR)[1]
#     }else{
#       years.file = paste(range(all.data$YEAR),collapse = "_")
#     }
    
    agg.by=c("SPECIES_CODE","GEAR_CODE")
    
    
    library(RODBC)
    channel <- odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
    for (a in 1:length(agg.by)){
      #get all of the unique values for the field we want to aggregate by
      combos = unique(all.data[agg.by[a]])
      if(agg.by[a] == "GEAR_CODE"){
        query = "SELECT GEAR_TYPE_CODE GEAR_CODE, GEAR_TYPE_DESC FROM CL.GEAR_TYPES;"
      }else{
        query = "SELECT SPECIES_CODE, SPECIES_NAME FROM CL.SPECIES;"
      }
      the.codes = sqlQuery(channel,query)
      combos=merge(combos,the.codes)
      
      for (i in 1:nrow(combos)){
        writeLines(paste0("Analysing: ", combos[i,2]))
        #print(paste0("working on ",all.data$DESC_ENG[all.data[agg.by]==combos[i,]]))
        this <- marfissci.process.data(all.data[which(all.data[agg.by[a]]==combos[i,1]),],
                                       agg.field = "LIVE_WT",
                                       agg.by =agg.by[a],
                                       save.RDS = T,
                                       save.CSV = T,
                                       save.SHP = T,
                                       agg.by.year =F,
                                       name.det=paste0(years.file,"_"),
                                       out.folder=out.folder,
                                       output="RDS")
        if (i==1) gearKeep<<-this
        if (!is.null(this)){
          writeLines("Generating a plot...")
          if(agg.by[a] == "GEAR_CODE") {
            colour.by = "CNT_LIVE_WT"
          }else{
            colour.by = "SUM_LIVE_WT"
          }
          marfissci.simple.map(this, agg.by = agg.by[a], 
                               colour.by = colour.by, 
                               save.plot = T, 
                               out.folder=out.folder,
                               name.det=years.file, 
                               plot.title=paste0(combos[i,2]," ",  years.file))
        }else{
          writeLines(paste0("Insufficient data to plot a figure for ",combos[i,2]))
        }
      }
      
    }
  } 
  
  
  if (combine){
    writeLines("Combining all of the csv files into a single one")
    all.data=do.call(rbind,lapply(file.path(folder,list.files(path=folder, pattern="\\.csv$")), 
                                  read.csv, header=TRUE, sep=","))
    do.it(all.data)
  } else {
    file.names <- dir(folder, pattern ="\\.csv$")
    for(i in 1:length(file.names)){
      all.data <- read.csv(file.path(folder,file.names[i]),header=TRUE, sep=",")
      do.it(all.data)
    }
  }
  
  diff=difftime(Sys.time(),start.time, units = "secs")
  diff = format(.POSIXct(diff,tz="GMT"), "%H:%M:%S")
  writeLines(paste0(diff, " elapsed"))
  return(NULL)
}
#loadfunctions("mpa")




