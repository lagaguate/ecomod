
#stolen from https://github.com/jae0/ecomod/ -- "na.zero.r"
na.zero<-function(x){
  for(i in 1:length(x[1,])){
    if(length(which(is.na(x[,i])))>0){
      x[which(is.na(x[,i])),i]<-0}
  }
  return(x)
} 

tows<-read.csv(paste0(project.datadirectory("offshoreclams"),"/Combined/Combined_Tow_dataMMM.csv"))
catch<-read.csv(paste0(project.datadirectory("offshoreclams"),"/Combined/Combined_Catch_dataMMM.csv"))
bycatch<-read.csv(paste0(project.datadirectory("offshoreclams"),"/Combined/Combined_ByCatch_DataMMM.csv"))
bycatch<-bycatch[bycatch$ITIS_CODE==80983,] #only surfclams

catchtow<-merge(tows, catch, by=c("INDX"), all.x = T)
catchtow$SURVEY.y<-NULL
catchtow$BLADE_WIDTH<-0
type1<-c("CK2003-01","CK2004-01","CK2006-01","T12008-01","T12009-01","T12010-01") #69.5 blades
catchtow[catchtow$SURVEY.x %in% type1,]$BLADE_WIDTH<-69.5/39.36996	
type2<-c("AD1996-01","AD1997-01")  #70 blades
catchtow[catchtow$SURVEY.x %in% type2,]$BLADE_WIDTH<-70/39.36996	
catchtow[catchtow$SURVEY.x == "MD2002-01",]$BLADE_WIDTH<-1.4	
catchtow[catchtow$SURVEY.x == "MO2006-01",]$BLADE_WIDTH<-47.0/39.36996
catchtow[catchtow$SURVEY.x == "BA2007-01",]$BLADE_WIDTH<-36.0/39.36996
#What about these surveys?
# "AD1996-01" 
# "AD1997-01" 

#catchtow$TOW.y<-NULL
#catchtow<-catchtow[catchtow$SURVEY.x=="CK2006-01",]
catchtow<-merge(catchtow, bycatch, by="INDX", all.x=T)
catchtow$WEIGHT_KG<-as.numeric(catchtow$WEIGHT_KG)

catchtow<-na.zero(catchtow)



catchtow$CATCHFACTOR<-catchtow$TOTAL_CATCH_KG/(catchtow$KG_SAMPLED_MAIN+catchtow$KG_SAMPLED_BYCATCH)
catchtow$ADJCATCH<-(catchtow$ARCTIC_SURF_KG+catchtow$WEIGHT_KG)*catchtow$CATCHFACTOR 
catchtow$STDFACT<-1000/(catchtow$BLADE_WIDTH*catchtow$DIST_M) #some DIST_M are zero!
catchtow$STDCATCH<-catchtow$STDFACT*catchtow$ADJCATCH
  
catch_analysis<-  catchtow[,c("INDX","SURVEY.x","TOW.x","SLAT","SLON","ELAT",
                              "ELON","TOWTYPE","TOWQUALITY","TOTAL_CATCH_KG",
                              "KG_SAMPLED_MAIN","KG_SAMPLED_BYCATCH",
                              "ARCTIC_SURF_KG","WEIGHT_KG","BLADE_WIDTH","DIST_M",
                              "CATCHFACTOR","ADJCATCH","STDFACT","STDCATCH")]
# catch_analysis$FLAG="Good"
# catch_analysis[is.infinite(catch_analysis$STDFACT),]$FLAG<-"Bad"
# GBSurveys<-c("CK2006-01","T12008-01","T12009-01")
# catch_analysis[!catch_analysis$SURVEY.x %in% GBSurveys,]$FLAG<-"Bad"
# catch_analysis$CATCHFACTOR<-NULL
# catch_analysis$ADJCATCH<-NULL
# catch_analysis$STDFACT<-NULL
# catch_analysis$STDCATCH<-NULL
# catch_analysis$DIST_M<-NULL
# catch_analysis$BLADE_WIDTH<-NULL
# catch_analysis$WEIGHT_KG<-NULL

catch_analysis<-na.zero(catch_analysis)
write.csv(catch_analysis, paste0(project.datadirectory("offshoreclams"),"/out/catch_analysis.csv"))