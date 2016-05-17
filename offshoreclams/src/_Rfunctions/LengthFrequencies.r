LengthFrequencies=function(DataList, DS="Survey", bins=seq(0,200,1), Yrs=2005:2014, wal, fn='',... ) {

    ### Carapace Length Frequencies (CLF)

    rootdir=file.path(project.datadirectory('offshoreclams'),'figures')

    if(DS=='Survey'){

        LF = merge(DataList$LenFreq,DataList$surveyData[,c('survey.x','tow.x','stdcatch')],by.x=c('survey','tow'),by.y=c('survey.x','tow.x'),all.x=T)

       lftrips = unique(DataList$lf.data$logtrip_id)
        iCLF = list()
        tripCLF = list()
        WLF = list()
        tripCatch = list()
        n = c()

        for (y in 1:length(Yrs)) {

            logtrips = with(subset(DataList$log.data,year==Yrs[y]),unique(logtrip_id))
            samptrips = lftrips[lftrips%in%logtrips]
            n[y] = length(samptrips)
            iCLF[[y]] = t(sapply(samptrips,function(i){with(subset(DataList$lf.data,logtrip_id==i&rlength>=min(bins)&rlength<max(bins)),hist(rep(rlength,number_at_length),breaks=bins,plot=F)$count)}))
            tripCatch[[y]] = with(subset(DataList$log.data,year==Yrs[y]&logtrip_id%in%samptrips),tapply(round_catch,logtrip_id,sum,na.rm=T))
            WLF[[y]] = rowSums(sweep(iCLF[[y]],2,FUN='*',wal))
            tripCLF[[y]] = sweep(iCLF[[y]],1,FUN='*',tripCatch[[y]]/WLF[[y]])

        }
        
        SurveyCLF = do.call("rbind",lapply(tripCLF,colSums))
        
        # plot          
        BarPlotCLF(list(SurveyCLF),yrs=Yrs,bins=bins,col='grey',LS=NULL,sample.size=n,xlab="Shell Length (mm)",filen=file.path(rootdir,paste0("FisheryLengthFrequency",fn,".pdf")), ...)
        #BubblePlotCLF(FisheryCLF,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen=file.path(rootdir,paste0("FisheryLengthFrequency",fn,".pdf")),prop=T)
        return(SurveyCLF)
    }

    if(DS=='Fishery'){

        lftrips = unique(DataList$lf.data$logtrip_id)
        iCLF = list()
        tripCLF = list()
        WLF = list()
        tripCatch = list()
        n = c()

        for (y in 1:length(Yrs)) {

            logtrips = with(subset(DataList$log.data,year==Yrs[y]),unique(logtrip_id))
            samptrips = lftrips[lftrips%in%logtrips]
            n[y] = length(samptrips)
            iCLF[[y]] = t(sapply(samptrips,function(i){with(subset(DataList$lf.data,logtrip_id==i&rlength>=min(bins)&rlength<max(bins)),hist(rep(rlength,number_at_length),breaks=bins,plot=F)$count)}))
            tripCatch[[y]] = with(subset(DataList$log.data,year==Yrs[y]&logtrip_id%in%samptrips),tapply(round_catch,logtrip_id,sum,na.rm=T))
            WLF[[y]] = rowSums(sweep(iCLF[[y]],2,FUN='*',wal))
            tripCLF[[y]] = sweep(iCLF[[y]],1,FUN='*',tripCatch[[y]]/WLF[[y]])

        }
        
        FisheryCLF = do.call("rbind",lapply(tripCLF,colSums))
        
        # plot          
        BarPlotCLF(list(FisheryCLF),yrs=Yrs,bins=bins,col='grey',LS=NULL,sample.size=n,xlab="Shell Length (mm)",filen=file.path(rootdir,paste0("FisheryLengthFrequency",fn,".pdf")), ...)
        #BubblePlotCLF(FisheryCLF,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen=file.path(rootdir,paste0("FisheryLengthFrequency",fn,".pdf")),prop=T)
        return(FisheryCLF)
    }




}

