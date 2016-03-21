process.marfis.mpa = function(df, save.csv=F, just.make.rds=T,gridres=1, nclasses=5){
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
  library(rgdal)
  #df$YEAR=year(df$DATE_FISHED)
  combo=unique(df[c("YEAR_FISHED","SPECIES_CODE")])
  for (j in 1:NROW(combo)){
    this.df=df[with(df, which(df$YEAR==combo[j,1] & df$SPECIES_CODE==combo[j,2])),]
    namebit=paste0(combo[j,1],"_",combo[j,2])
    print(paste0("Working on marfis_",namebit))
    if(save.csv){
      write.csv2(this.df, paste0(project.datadirectory("mpa"),"/",namebit,"_marfis.csv"))
      print(paste0("CSV of ",combo[j,1]," spec ", combo[j,2]," written to ", 
                   project.datadirectory("mpa"),"/csv/",namebit,"_marfis.csv"))
    }
    #35,50 42,44
    #-74,-42
    the.rds = aggregate.marfis(this.df,  
                               xlim=c(-74,-42), ylim=c(35,50), gridres=gridres,
                               just.make.rds=F, 
                               anal.fn = "sum", anal.field = "RND_WEIGHT_KGS",
                               privacy.field = c("ROWNUM"), ruleOf=1, 
                               nclasses= nclasses, class.style="jenks",
                               show.pts=F,show.restricted=T, show.legend = T,
                               save.plot= T, figuredir = "mpa",
                               title=paste0("marfis ",namebit))


      if (!is.character(the.rds)){
        saveRDS(the.rds,paste0(project.datadirectory("mpa"),"/polygons/",namebit,"_sp_polygon.rds"))
        print(paste0("SpatialPolygonsDataFrame saved to ", project.datadirectory("mpa"),"/polygons/",namebit,"_sp_polygon.rds"))
        writeOGR(the.rds, dsn = paste0(project.datadirectory("mpa"),"/shapes"), layer = paste0(namebit,'_poly'), driver = "ESRI Shapefile", overwrite_layer = T)
    } else {
      print("no polygon generated")
    }
      rm(this.df)
  }
}

#get the source data
#df = read.csv2(paste0(project.datadirectory("mpa"),"/get_marfis_grp_20160321_0835.csv"))
#it would be better if ROWNUM was embedded in the output
#colnames(df)[colnames(df)=="X"] = "ROWNUM"
#use first x rows as test data
#   test.df=head(df,400)
#process.marfis.mpa(df,save.csv=F,nclasses=2,just.make.rds=T,gridres=0.1)
  
#test the data on scallop initially
#test.df=df[df$SPECIES_CODE==612,]
##############
#read in a resultant spatial polygon and plot it
#test=readRDS(paste0(project.datadirectory("mpa"),"/polygons/2010_100_sp_polygon.rds"))
#library(rgdal)
#writeOGR(test, dsn = '.', layer = 'poly', driver = "ESRI Shapefile")
  #plot(test, col = test@data$colcode, border = "gray90")