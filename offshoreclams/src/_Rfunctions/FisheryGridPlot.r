FisheryGridPlot <- function(fisheryList, p, boundPoly, vms=FALSE, fn='',cpue=TRUE, aspr='calculate', ...){

  
  ## Grid Plots
  b=ifelse(p$bank=="Ban",1,2)
  yrs=p$yrs

  effortgrids=list()
  catchgrids=list()
  grid.polyData=list()

  if(vms){

  logdata = merge(fisheryList$vms.data,subset(fisheryList$log.data, bank==b&area>p$effort.threshold[1]&area<p$effort.threshold[2]&round_catch>p$catch.threshold[1]&round_catch<p$catch.threshold[2],  c("logrecord_id","round_catch","area")))
  vmsperwatch=with(logdata,tapply(logrecord_id,logrecord_id,length))
  logdata = merge(logdata,data.frame(logrecord_id=as.numeric(names(vmsperwatch)),vmspw=vmsperwatch),all=T)
  logdata$A = logdata$area/logdata$vmspw
  logdata$C = logdata$round_catch/logdata$vmspw
  logdata$EID = 1:nrow(logdata)
  logdata = logdata[,c("year","EID","lon","lat","C","A")]
  names(logdata)<-c("year","EID","X","Y","C","A")

  }
  else {
    logdata = na.omit(subset(fisheryList$log.data, bank==b&area>p$effort.threshold[1]&area<p$effort.threshold[2]&round_catch>p$catch.threshold[1]&round_catch<p$catch.threshold[2],  c("year","logrecord_id","lon_dd","lat_dd","round_catch","area")))
    names(logdata)<-c("year","EID","X","Y","C","A")
  }

  # boundPoly
  logdata = subset(logdata,EID%in%findPolys(logdata,boundPoly, maxRows = 1e+06)$EID)

    # EFFORT

    grid.polyData[[1]]<-list()
    pdf(file.path( project.datadirectory("offshoreclams"), "figures",paste0(fn,p$bank,"Effort.pdf")),11,8)

     
     for(y in 1:length(yrs)){

      grid.dat=subset( logdata ,year%in%yrs[[y]],c("EID","X","Y","A"))
      print(paste(y,Sys.time()))
      print(summary(grid.dat))
      #browser()
      if(nrow(grid.dat)>0){
       
       effortgrids[[y]]<-gridData(grid.dat,lvls=p$effort.levels,bcol=p$catch.cols,FUN=sum,border=NA,grid.size=p$grid.size,aspr=aspr,sx=p$Min_lon,sy=p$Min_lat,ex=p$Max_lon,ey=p$Max_lat)
       grid.polyData[[1]][[y]] = effortgrids[[y]][[2]]
       
       titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))
       
       ClamMap2('Ban',poly.lst=effortgrids[[y]][1:2],title=paste(titleyr,"Surf Clam Effort"),...)
       ContLegend("bottomright",lvls=effortgrids[[y]]$lvls/10^4,Cont.data=effortgrids[[y]],title="Area Fished (ha)",inset=0.02,cex=0.8,bg='white')
       }
     
     }

    dev.off()


    # CATCH

    grid.polyData[[2]]<-list()
    pdf(file.path( project.datadirectory("offshoreclams"), "figures",paste0(fn,p$bank,"Catch.pdf")),11,8)
     
     for(y in 1:length(yrs)){
     
      grid.dat=subset( logdata ,year%in%yrs[[y]],c("EID","X","Y","C"))
      print(paste(y,Sys.time()))
      print(summary(grid.dat))
      if(nrow(grid.dat)>0){
       
       catchgrids[[y]]<-gridData(grid.dat,lvls=p$catch.levels,bcol=p$catch.cols,FUN=sum,border=NA,grid.size=p$grid.size,aspr=aspr,sx=p$Min_lon,sy=p$Min_lat,ex=p$Max_lon,ey=p$Max_lat)
       grid.polyData[[2]][[y]] = catchgrids[[y]][[2]]
       
       titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))

       ClamMap2(p$bank,poly.lst=catchgrids[[y]][1:2],title=paste(titleyr,"Surf Clam Catch"),...)
       ContLegend("bottomright",lvls=catchgrids[[y]]$lvls/10^3,Cont.data=catchgrids[[y]],title="Catch (t)",inset=0.02,cex=0.8,bg='white')
       }
     
     }

    dev.off()



    # CPUE
    grid.polyData[[3]]<-list()
    cpuegrids = catchgrids


    pdf(file.path( project.datadirectory("offshoreclams"), "figures",paste0(fn,p$bank,"CPUE.pdf")),11,8)
     
     for(y in which(!unlist(lapply(effortgrids,is.null)))){
      print(paste(y,Sys.time()))

        cpuegrids[[y]][[2]]$Z <- catchgrids[[y]][[2]]$Z / effortgrids[[y]][[2]]$Z
        cpuegrids[[y]][[2]]$Z[is.infinite(cpuegrids[[y]][[2]]$Z)] <- NA
        cpuegrids[[y]][[2]]$Z[cpuegrids[[y]][[2]]$Z==0] <- NA
        cpuegrids[[y]][[2]]$Z[effortgrids[[y]][[2]]$Z<p$cpue.threshold[1]] <- NA
      
        cols   <- brewer.pal(length(p$cpue.levels),p$cpue.cols) 
        pdata  <- makeProps(na.omit(cpuegrids[[y]][[2]][,1:3]), c(p$cpue.levels,max(p$cpue.levels)*100), "col", cols) 
        pdata$border  <- NA
        cpuegrids[[y]][[2]] <- pdata
       
       grid.polyData[[3]][[y]] = cpuegrids[[y]][[2]]
       
       titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))

       ClamMap2(p$bank,poly.lst=cpuegrids[[y]][1:2],title=paste(titleyr,"Surf Clam CPUE"),...)
       ContLegend("bottomright",lvls=p$cpue.levels*1000,Cont.data=cpuegrids[[y]],title=expression(CPUE (t/km^2)),inset=0.02,cex=0.8,bg='white')
       }
     
     

    dev.off()

  # EXPLOITATION
    grid.polyData[[3]]<-list()
    exploitgrids = catchgrids


    pdf(file.path( project.datadirectory("offshoreclams"), "figures",paste0(fn,p$bank,"Exploitation.pdf")),11,8)
     
     for(y in which(!unlist(lapply(effortgrids,is.null)))){
 
       titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))
        
       ClamMap2(p$bank,poly.lst=effortgrids[[y]][1:2],title=paste(titleyr,"Surf Clam Exploitation"),...)
       ContLegend("bottomright",lvls=p$effort.levels/10^6/p$grid.size^2,Cont.data=effortgrids[[y]],title="Exploitation Rate",inset=0.02,cex=0.8,bg='white')
       }
     
     

    dev.off()



     return(list(grid=catchgrids[[length(yrs)]][[1]], grid.polyData=grid.polyData))
  }

