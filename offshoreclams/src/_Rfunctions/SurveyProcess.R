SurveyProcess <- function(bins=1:200,species=80983){

  tows<-read.csv(  file.path(project.datadirectory("offshoreclams"),"data","Combined","Combined_Tow_dataMMM.csv"))
  catch<-read.csv(  file.path(project.datadirectory("offshoreclams"),"data","Combined","Combined_Catch_dataMMM.csv"))
  bycatch<-read.csv(  file.path(project.datadirectory("offshoreclams"),"data","Combined","Combined_ByCatch_DataMMM.csv"))
  bycatch<-subset(bycatch,ITIS_CODE==species) #only surfclams

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

  catchtow$DIST_M[catchtow$SURVEY.x == "T12010-01"] <- catchtow$DIST_M[catchtow$SURVEY.x == "T12010-01"] * 1852

  catchtow$CATCHFACTOR<-catchtow$TOTAL_CATCH_KG/(catchtow$KG_SAMPLED_MAIN+catchtow$KG_SAMPLED_BYCATCH)
  catchtow$ADJCATCH<-(catchtow$ARCTIC_SURF_KG+catchtow$WEIGHT_KG)*catchtow$CATCHFACTOR 
    
  surveyData <-  catchtow[,c("INDX","SURVEY.x","DATE","STARTTIME","END_TIME","TOW.x","SLAT","SLON","ELAT",
                                "ELON","TOWTYPE","TOWQUALITY","TOTAL_CATCH_KG",
                                "KG_SAMPLED_MAIN","KG_SAMPLED_BYCATCH",
                                "ARCTIC_SURF_KG","WEIGHT_KG","BLADE_WIDTH","DIST_M",
                                "CATCHFACTOR","ADJCATCH")]
  # surveyData$FLAG="Good"
  # surveyData[is.infinite(surveyData$STDFACT),]$FLAG<-"Bad"
  # GBSurveys<-c("CK2006-01","T12008-01","T12009-01")
  # surveyData[!surveyData$SURVEY.x %in% GBSurveys,]$FLAG<-"Bad"
  # surveyData$CATCHFACTOR<-NULL
  # surveyData$ADJCATCH<-NULL
  # surveyData$STDFACT<-NULL
  # surveyData$STDCATCH<-NULL
  # surveyData$DIST_M<-NULL
  # surveyData$BLADE_WIDTH<-NULL
  # surveyData$WEIGHT_KG<-NULL

  surveyData<-na.zero(surveyData)

  surveyData$DATE <- as.Date(surveyData$DATE,"%d/%m/%Y")
  surveyData$YEAR <- year(surveyData$DATE)


  # errors
  surveyData$SLAT[surveyData$SURVEY.x == "T12010-01" & surveyData$TOW.x == 115]<-44.50738
  		
  #surveyData$X<-with(surveyData,apply(cbind(ELON,SLON),1,mean)) # too many end locations are erroneous
  #surveyData$Y<-with(surveyData,apply(cbind(ELAT,SLAT),1,mean))
  surveyData$X<-surveyData$SLON
  surveyData$Y<-surveyData$SLAT
  surveyData$EID<-1:nrow(surveyData)


  x <- with(surveyData,merge(data.frame(PID=EID,POS=1,X=SLON,Y=SLAT),data.frame(PID=EID,POS=2,X=ELON,Y=ELAT),all=T))
  x <-x[order(x$PID),]
  attr(x,"projection") = "LL"
  surveyData <- merge(surveyData,with(subset(calcLength(x),length>0&length<1),data.frame(EID=PID,length=length*1000)),all=T)

  surveyData$DIST_M[surveyData$DIST_M==0] <- surveyData$length[surveyData$DIST_M==0] #some DIST_M are zero! replace with length calculated from start and end position

  surveyData$STDFACT<-1000/(surveyData$BLADE_WIDTH*surveyData$DIST_M) 
  surveyData$STDCATCH<-surveyData$STDFACT*surveyData$ADJCATCH
  surveyData$STDCATCH[surveyData$ADJCATCH==0] <- 0 

  write.csv(surveyData,   file.path(project.datadirectory("offshoreclams"),"R","SurveyData.csv"))


  ##### Length - Frequency #####

  lenfreqsamp<-read.csv(  file.path(project.datadirectory("offshoreclams"),"data","Combined","Combined_Freq_sample_dataPBH.csv"))
  lenfreqsamp<-subset(lenfreqsamp,Species.Code==species) #only surfclams
  names(lenfreqsamp)[4:5]<-c("N_measured","Wt_measured")

  lenfreq.a<-read.csv(  file.path(project.datadirectory("offshoreclams"),"data","Combined","Len_freq_data_a.csv"))
  lenfreq.b<-read.csv(  file.path(project.datadirectory("offshoreclams"),"data","Combined","Len_freq_data_b.csv"))
  lenfreq.c<-read.csv(  file.path(project.datadirectory("offshoreclams"),"data","Combined","Len_freq_data_c.csv"))

  lenfreq <- rbind(lenfreq.a,lenfreq.b,lenfreq.c)
  lenfreq <- subset(lenfreq,Species==species) #only surfclams
  lenfreq$TowID = paste(lenfreq$Survey,lenfreq$Tow,sep='.')
  lenfreqsamp$TowID = paste(lenfreqsamp$Survey,lenfreqsamp$Tow,sep='.')
  LenFreq <- merge(lenfreqsamp[,c("TowID","N_measured","Wt_measured")],lenfreq)
  
  Morphs<-read.csv(  file.path(project.datadirectory("offshoreclams"),"data","Combined","Combined_Morphometrics_dataPBH.csv"))
  Morphs<-subset(Morphs,Species==species) #only surfclams
  Morphs$TowID = paste(Morphs$Survey,Morphs$Set,sep='.')


return(list(surveyData,LenFreq.Morphs))

}


