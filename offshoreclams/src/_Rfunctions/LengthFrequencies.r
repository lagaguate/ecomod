LengthFrequencies<-function(Data, DS="Survey", bins=seq(0,200,1), Yrs=2005:2014, fn='',... ) {

    ### Carapace Length Frequencies (CLF)

    rootdir=file.path(project.datadirectory('offshoreclams'),'figures')

    if(DS=='Survey'){

        # Gets  Survey Data
        surveys34<-SurveyProcess(lfa="34",yrs=Yrs,mths=c("Jul","Jun"),size.range=range(bins),bin.size=diff(bins)[1])

        # Limit data to 32 selected index stations
        LS32stns<-read.csv(file.path(project.datadirectory('offshoreclams'),"data","products","survey32Stations.csv"))

        # Construct CLF
        SurveyCLF<-t(sapply(Yrs,function(y){colMeans(subset(surveys34,YEAR==y&SID%in%LS32stns$SID,paste0("CL",bins[-length(bins)])),na.rm=T)}))

        # plot
        BarPlotCLF(list(SurveyCLF),yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("SurveyLengthFrequency",fn,".pdf")), ...)
        return(SurveyCLF)
    }

    if(DS=='Fishery'){

        ### Scallop Survey seperate SPA 3 and SFA 29
        # SFA 29 : Aug-Oct 2000-
        # SPA 3 : Aug-Sep 1991-2003; May-Jul 2004-

        hist(rep(RLENGTH,NUMBER_AT_LENGTH),breaks=c(min(RLENGTH),max(RLENGTH)))
        for(i in 1:length(Yrs)){
            CLF[[i]]<-t(sapply(yrs,function(y){with(subset(Data,YEAR==y&ID==IDs[i]&LENGTH>=min(bins)&LENGTH<max(bins)),hist(LENGTH,breaks=bins,plot=F)$count)}))
        }
        names(CLF)<-IDs

        SCALSURV3.dat<-ScallopSurveyProcess(SPA="3",Yrs=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
        SCALSURV29.dat<-ScallopSurveyProcess(SPA="29",Yrs=Yrs,size.range=range(bins),bin.size=diff(bins)[1])

        # Construct CLF
        FisheryCLF<-list()
        FisheryCLF$ScallopSurvey3<-t(sapply(Yrs,function(y){colMeans(subset(SCALSURV3.dat,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))
        FisheryCLF$ScallopSurvey29<-t(sapply(Yrs,function(y){colMeans(subset(SCALSURV29.dat,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))

        # plot
        BarPlotCLF(FisheryCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("FisheryLengthFrequency",fn,".pdf")), ...)
        #BubblePlotCLF(FisheryCLF,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="FisheryCLFLFA34",prop=T)
        return(FisheryCLF)
    }




}

