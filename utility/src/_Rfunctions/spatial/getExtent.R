getExtent <- function(area){
  #prevent case sensitivity errors
  area<-toupper(area)
  FishingExtentsList<-read.csv(find.ecomod.gis("Fishing_areas_extents"))
  StrataExtentsList<-read.csv(find.ecomod.gis("Strata_extents"))
  MgmtExtentsList<-read.csv(find.ecomod.gis("Management_Area_extents"))
  #...moreextents?
  extentsList<-rbind(FishingExtentsList,StrataExtentsList)
  theselimits<-extentsList[extentsList$FEATUREID==area,][3:6]  
  
  xlim=c(theselimits[1],theselimits[2])
  ylim=c(theselimits[3],theselimits[4])
  limits<-c(xlim,ylim)
#   #Snowcrab Areas originally added by Ben
#   if(area=='NENS') 			{ xlim=c(-61,-58.2); ylim=c(45.9,47.5) }
#   if(area=='SENS') 			{ xlim=c(-63.5,-57); ylim=c(42.5,46.1)   }
#   if(area=='4X')   			{ xlim=c(-67,-63.1); ylim=c(42.5,45)     }
#   if(area=='23')   			{ xlim=c(-60.5,-57); ylim=c(43,46.2)   }
#   if(area=='24')   			{ xlim=c(-63.5,-59); ylim=c(42.5,45.5)   }
#   if(area=='not4X')   	{ xlim=c(-63.5,-57); ylim=c(42.5,47.5)   } 
  return(limits)
}