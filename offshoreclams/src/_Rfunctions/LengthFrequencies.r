LengthFrequencies=function(DataList, DS="Survey", bins=seq(0,200,1), Yrs=2005:2014, wal, fn='',... ) {

    ### Carapace Length Frequencies (CLF)

    rootdir=file.path(project.datadirectory('offshoreclams'),'figures')

    if(DS=='Survey'){

        # Gets  Survey Data
        surveys34=SurveyProcess(lfa="34",yrs=Yrs,mths=c("Jul","Jun"),size.range=range(bins),bin.size=diff(bins)[1])

        # Limit data to 32 selected index stations
        LS32stns=read.csv(file.path(project.datadirectory('offshoreclams'),"data","products","survey32Stations.csv"))

        # Construct CLF
        SurveyCLF=t(sapply(Yrs,function(y){colMeans(subset(surveys34,year==y&SID%in%LS32stns$SID,paste0("CL",bins[-length(bins)])),na.rm=T)}))

        # plot
        BarPlotCLF(list(SurveyCLF),yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("SurveyLengthFrequency",fn,".pdf")), ...)
        return(SurveyCLF)
    }

    if(DS=='Fishery'){

        lftrips = unique(DataList$LenFreq$logtrip_id)
        iCLF = list()
        FisheryCLF = list()
        WLF = list()
        tripCatch = list()

        for (y in 1:length(Yrs)) {

            logtrips = with(subset(DataList$Logs,year==Yrs[y]),unique(logtrip_id))
            samptrips = lftrips[lftrips%in%logtrips]

browser()
            iCLF[[y]] = t(sapply(samptrips,function(i){with(subset(DataList$LenFreq,logtrip_id==samptrips[i]&length>=min(bins)&length<max(bins)),hist(rep(rlength,number_at_length),breaks=bins,plot=F)$count)}))
            tripCatch[[y]] = with(subset(DataList$Logs,year==Yrs[y]&logtrip_id%in%samptrips),tapply(round_catch,logtrip_id,sum,na.rm=T))
            LFweight[[y]] = rowSums(sweep(iCLF[[y]],1,FUN='*',wal))
            FisheryCLF[[y]] = sweep(iCLF[[y]],1,FUN='*',tripCatch[[y]]/LFweight[[y]])
browser()

        }
        # plot
        BarPlotCLF(FisheryCLF,yrs=Yrs,bins=bins,col='grey',filen=file.path(rootdir,paste0("FisheryLengthFrequency",fn,".pdf")), ...)
        #BubblePlotCLF(FisheryCLF,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="FisheryCLFLFA34",prop=T)
        return(FisheryCLF)
    }




}

