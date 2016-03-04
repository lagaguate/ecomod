process.marfis.mpa = function(df, save.csv=F){
  if (F) {
    # load required ecomod functions
    loadfunctions("marfissci/src/_RFunctions")
  }
  #'MMM - Mar 2016
  #'This function is intended for use with MPA network planning and relies 
  #'almost entirely on the aggregate.marfis.R function in the marfissci folder.
  #'It accepts a df of marfis data, and processes it into a number of products 
  #'by the available unique combinations of year and species code.  It 
  #'aggregates the data, and calculates the total weight caught in each cell.  A
  #'byproduct of using aggregate.marfis.R is that it also requires a 
  #'"privacy.field" upon which derives a count of the number of unique values of 
  #'this field within each cell.
  #'
  #'For the purposes of this analysis, a parameter called "ruleOf" is set to 1, 
  #'which ensure that all data is returned, no matter how few unique occurences
  #'happen in each cell
  #'
  #'Outputs for each year/species combo:
  #'  Data:  with save.csv=T, csv files of the subsetted data will be retained
  #'  SpatialPolygonsDataFrame:  an rds file is saved for each
  #'  Figures: a plot of each aggregation is saved*
  #'  * Note that while a figure will not be generated in cases where the number 
  #'  of records is less than the number of nclasses assigned (default is 10),
  #'  the rds file will still be created 
  #'  
  
  library(lubridate) 
  df$YEAR=year(df$DATE_FISHED)
  combo=unique(test.df[c("YEAR","SPECIES_CODE")])
  for (j in 1:NROW(combo)){
    this.df=test.df[with(test.df, which(test.df$YEAR==combo[j,1] & test.df$SPECIES_CODE==combo[j,2])),]
    namebit=paste0(combo[j,1],"_",combo[j,2])
    print(paste0("Working on marfis_",namebit))
    if(save.csv){
      write.csv2(this.df, paste0(project.datadirectory("mpa"),"/",namebit,"_marfis.csv"))
      print(paste0("CSV of ",combo[j,1]," spec ", combo[j,2]," written to ", 
                   project.datadirectory("mpa"),"/csv/",namebit,"_marfis.csv"))
    }
    saveRDS(aggregate.marfis(this.df,  
                     xlim=c(-74,-42), ylim=c(36,50), gridres=0.375, 
                     anal.fn = "sum", anal.field = "RND_WEIGHT_KGS",
                     privacy.field = c("SETID"), ruleOf=1, 
                     nclasses= 10, class.style="jenks",
                     show.pts=F,show.restricted=T, show.legend = T,
                     save.plot= T, figuredir = "mpa",
                     title=paste0("marfis ",namebit)),
                     paste0(project.datadirectory("mpa"),"/polygons/",namebit,"_sp_polygon.rds"))
    print(paste0("SpatialPolygonsDataFrame saved to ",
                 project.datadirectory("mpa"),"/polygons/",namebit,"_sp_polygon.rds"))
    rm(this.df)
  }
}

#get the source data
  #df = read.csv2(paste0(project.datadirectory("mpa"),"/csv/get_marfis_20160303_1515.csv"))
  #df = read.csv2(paste0(project.datadirectory("mpa"),"/csv/2002_marfis.csv"))
#use first x rows as test data
#   test.df=head(df,10000)
#   test.df$YEAR=year(df$DATE_FISHED)
  
#run the script
  #process.marfis.mpa(test.df,F)

#read in a resultant spatial polygon and plot it
  #test=readRDS(paste0(project.datadirectory("mpa"),"/2002_sp_polygon.rds"))
  #plot(test, col = test@data$colcode, border = "gray90")