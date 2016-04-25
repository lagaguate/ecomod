marfissci.batch.process <- function(folder=file.path(project.datadirectory("mpa"),"test"), 
                                    agg.by="SPECIES_CODE"){
  #' The purpose of this batch process function is to facilitate the mass
  #' generation of data products from marfis data.  It assumes that data has 
  #' been extracted via marfissci.get.data(), and the resultant csv file(s) are
  #' saved locally.
  #' 
  #' When called, this batch function will allow the user to select what data 
  #' (i.e. years, species, and/or gears) should be aggregated together.  It will
  #' then generate rds files and figures for all the data.

  
  writeLines("Combining all of the csv files into a single one")
  all.data=do.call(rbind,lapply(file.path(folder,list.files(path=folder, pattern="\\.csv$")), 
                                read.csv, header=TRUE, sep=","))
  
  #get all of the unique values for the field we want to aggregate by
  combos = unique(all.data[agg.by])
 
  library(RODBC)
  channel <- odbcConnect("PTRAN",uid = oracle.personal.username,pwd = oracle.personal.password)
 if(agg.by == "GEAR_CODE"){
   query = "SELECT GEAR_CODE, DESC_ENG FROM MARFISSCI.GEARS;"
 }else{
   query = "SELECT SPECIES_CODE, DESC_ENG FROM MARFISSCI.SPECIES;"
 }
  the.codes = sqlQuery(channel,query)
  combos=merge(combos,the.codes)
  
  for (i in 1:nrow(combos)){
    writeLines(paste0("Analysing: ", combos[i,2]))
    #print(paste0("working on ",all.data$DESC_ENG[all.data[agg.by]==combos[i,]]))
     this <- marfissci.process.data(all.data[all.data[agg.by]==combos[i,1],],
                                   save.RDS = T,
                                   save.CSV = F,
                                   save.SHP = T,
                                   agg.by.year =F,
                                   output="RDS")
     writeLines("Generating a plot...")
     #SPECIES_CODE
        marfissci.simple.map(this, agg.by = agg.by, colour.by = "SUM_RND_WEIGHT_KGS", save.plot = T, plot.title=combos[i,2])
  }
  return(all.data)
}
#loadfunctions("mpa")




