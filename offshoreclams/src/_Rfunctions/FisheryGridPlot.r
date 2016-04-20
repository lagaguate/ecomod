FisheryGridPlot <- function(logdata, p, cpue=T, aspr='calculate', ...){

  
  ## Grid Plots
  b=ifelse(p$bank=="Ban",1,2)
  yrs=p$yrs

  effortgrids=list()
  catchgrids=list()
  grid.polyData=list()

  logdata = na.omit(subset(logdata, BANK==b&AREA>p$effort.threshold[1]&AREA<p$effort.threshold[2]&ROUND_CATCH>p$catch.threshold[1]&ROUND_CATCH<p$catch.threshold[2],  c("Year","LOGRECORD_ID","LON_DD","LAT_DD","ROUND_CATCH","AREA")))

    # Effort

    grid.polyData[[1]]<-list()
    pdf(file.path( project.datadirectory("offshoreclams"), "figures",paste0(p$bank,"Effort.pdf")),11,8)

     
     for(y in 1:length(yrs)){

      grid.dat=subset( logdata ,Year%in%yrs[[y]],c("LOGRECORD_ID","LON_DD","LAT_DD","AREA"))
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


    # Catch

    grid.polyData[[2]]<-list()
    pdf(file.path( project.datadirectory("offshoreclams"), "figures",paste0(p$bank,"Catch.pdf")),11,8)
     
     for(y in 1:length(yrs)){
     
      grid.dat=subset( logdata ,Year%in%yrs[[y]],c("LOGRECORD_ID","LON_DD","LAT_DD","ROUND_CATCH"))
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
    if(cpue){
    grid.polyData[[3]]<-list()
    cpuegrids = catchgrids


    pdf(file.path( project.datadirectory("offshoreclams"), "figures",paste0(p$bank,"CPUE.pdf")),11,8)
     
     for(y in which(!unlist(lapply(effortgrids,is.null)))){
      print(paste(y,Sys.time()))

        cpuegrids[[y]][[2]]$Z <- catchgrids[[y]][[2]]$Z / effortgrids[[y]][[2]]$Z
        cpuegrids[[y]][[2]]$Z[is.infinite(cpuegrids[[y]][[2]]$Z)] <- NA
        cpuegrids[[y]][[2]]$Z[cpuegrids[[y]][[2]]$Z==0] <- NA
      
        cols   <- brewer.pal(length(p$cpue.levels),p$cpue.cols) 
        pdata  <- makeProps(na.omit(cpuegrids[[y]][[2]][,1:3]), c(p$cpue.levels,max(p$cpue.levels)*100), "col", cols) 
        pdata$border  <- NA
        cpuegrids[[y]][[2]] <- pdata
       
       grid.polyData[[3]][[y]] = cpuegrids[[y]][[2]]
       
       titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))

       ClamMap2(p$bank,poly.lst=cpuegrids[[y]][1:2],title=paste(titleyr,"Surf Clam CPUE"),...)
       ContLegend("bottomright",lvls=p$cpue.levels,Cont.data=cpuegrids[[y]],title=expression(CPUE (kg/m^2)),inset=0.02,cex=0.8,bg='white')
       }
     }
     
     

    dev.off()

    return(list(grid=catchgrids[[length(yrs)]][[1]], grid.polyData=grid.polyData))
  }

