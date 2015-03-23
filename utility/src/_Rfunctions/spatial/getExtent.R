getExtent <- function(area){
  #this function checks some simple csv files to grab the extents necessary 
  #to show a number of regions, including NAFO, strata, and a number of others
  #review the csv files themselves to see available values
  
  #prevent case sensitivity errors
  area<-toupper(area)
  
  #read in the csv files
  FishingExtentsList<-read.csv(find.ecomod.gis("Fishing_areas_extents"))
  StrataExtentsList<-read.csv(find.ecomod.gis("Strata_extents"))
  MgmtExtentsList<-read.csv(find.ecomod.gis("Management_Area_extents"))
  NAFOExtentsList<-read.csv(find.ecomod.gis("NAFO_extents"))
  extentsList<-rbind(FishingExtentsList,StrataExtentsList)
  extentsList<-rbind(extentsList,MgmtExtentsList)
  extentsList<-rbind(extentsList,NAFOExtentsList)
  
  #grab the coords for those features that match selection
  theselimits<-data.frame(c(extentsList[extentsList$FEATUREID==area,][3:6] ) )
  #if there are no matches, just return the name of the failure so we can alert user
  if (nrow(theselimits)<1){
    return(area)
  }
  xlim=c(theselimits[1],theselimits[2])
  ylim=c(theselimits[3],theselimits[4])
  limits<-data.frame(c(id=as.character(area), xlim,ylim))
  return(limits)
}