###############################################################################
##
##
##  Artic Surf Clam Framework 
##
##  June 2016
##
##  Brad Hubley
##  Susan Heaslip
##
##
##
###############################################################################
# To run in ecomod, run the following commands

loadfunctions(c("offshoreclams","lobster","utility"))

RLibrary( "PBSmapping" ) # Load required packages


## Load Data

update.data=FALSE # TRUE accesses data from database if on a DFO windows machine


  # log data
  log.data <- GetLogData(update=update.data)
  processed.log.data <- ProcessLogData(log.data)

  # length frequency data
  lf.data <- GetLFData(update=update.data)

  # survey data
  loadfunctions( "offshoreclams", functionname="survey.process.r") 



####### Mapping

ClamMap2('all',isobath=seq(50,500,50))

 with(subset(processed.log.data,AREA>0),points(LON_DD,LAT_DD,pch=16,cex=0.1,col=rgb(0,0,0,0.1)))
 with(subset(processed.log.data,Year==2015&AREA>0),points(LON_DD,LAT_DD,pch=16,cex=0.2,col=rgb(1,0,0,0.2)))
 with(catch_analysis,points(SLON,SLAT,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))
 rect(Min_long,Min_lat,Max_long,Max_lat)

ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all')

 with(subset(processed.log.data,Year==2015&AREA>0),points(LON_DD,LAT_DD,pch=16,cex=0.5,col=rgb(1,0,0,0.2)))
 with(subset(processed.log.data,Year==2014&AREA>0),points(LON_DD,LAT_DD,pch=16,cex=0.5,col=rgb(0,1,0,0.2)))
 with(subset(processed.log.data,Year==2013&AREA>0),points(LON_DD,LAT_DD,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
 with(catch_analysis,points(SLON,SLAT,pch=16,cex=0.2,col=rgb(0,0,0,0.2)))


ClamMap2('Grand',isobath=seq(50,500,50))

 rect(Min_long,Min_lat,Max_long,Max_lat)
 with(subset(processed.log.data,Year==2013&AREA>0),points(LON_DD,LAT_DD,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
 with(catch_analysis,points(SLON,SLAT,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))


# explore distribution of catch and effort data in order to set appropriate bounds to censor the data

par(mfrow=c(2,1))#,mar=c(0.2,0.2,0.2,0.2))  
with(subset(processed.log.data,ROUND_CATCH>0&ROUND_CATCH<40000),hist(ROUND_CATCH,breaks=100,xlim=c(0,40000),xlab="Reported Catch by Watch (kg)",main=''))
abline(v=c(1500,30000),col='red',lwd=2)

with(subset(processed.log.data,AREA>0&AREA<400000),hist(AREA,breaks=100,xlim=c(0,400000),xlab="Reported Effort by Watch (m2)",main=''))
abline(v=c(15000,200000),col='red',lwd=2)

     
## Grid Plots

p=list()
p$bank= "Ban"
p$yrs= 1988:2015
p$effort.threshold = c(15000,200000)
p$catch.threshold = c(1500,30000)
p$effort.levels = c(1000,50000,100000,200000,500000,1000000,2000000)
p$catch.levels = c(100,5000,10000,20000,50000,100000,200000)
p$cpue.levels = c(0,0.02,0.04,0.06,0.08,0.1,0.12)
p$effort.cols = "YlGnBu"
p$catch.cols = "YlGnBu"
p$cpue.cols = "YlGnBu"
p$Min_lon = -60.0
p$Max_lon = -57.0
p$Min_lat = 44.0
p$Max_lat = 45.25
p$grid.size = 2

FisheryGridPlot(processed.log.data,p,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')

  # Effort
  effortgrids=list()

  pdf(file.path( project.datadirectory("offshoreclams"), "figures","BanEffort.pdf"),11,8)

   
   for(y in 1:length(yrs)){

    grid.dat=na.omit(subset( processed.log.data ,BANK==1&Year%in%yrs[y],c("LOGRECORD_ID","LON_DD","LAT_DD","AREA")))
    print(paste(y,Sys.time()))
    print(summary(grid.dat))
    if(nrow(grid.dat)>0){
     
     effortgrids[[y]]<-gridData(grid.dat,lvls=c(1000,50000,100000,200000,500000,1000000,2000000),FUN=sum,border=NA,grid.size=2,sx=Min_long,sy=Min_lat,ex=Max_long,ey=Max_lat)
     
     ClamMap2('Ban',poly.lst=effortgrids[[y]][1:2],title=paste(yrs[y],"Surf Clam Effort"),isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
     ContLegend("bottomright",lvls=effortgrids[[y]]$lvls/10^4,Cont.data=effortgrids[[y]],title="Area Fished (ha)",inset=0.02,cex=0.8,bg='white')
     }
   
   }

  dev.off()


  # Catch
  catchgrids=list()

  pdf(file.path( project.datadirectory("offshoreclams"), "figures","BanCatch.pdf"),11,8)
   
   for(y in 1:length(yrs)){
   
    grid.dat=na.omit(subset( processed.log.data ,BANK==1&Year%in%yrs[y],c("LOGRECORD_ID","LON_DD","LAT_DD","ROUND_CATCH")))
    print(paste(y,Sys.time()))
    print(summary(grid.dat))
    if(nrow(grid.dat)>0){
     
     catchgrids[[y]]<-gridData(grid.dat,lvls=c(100,5000,10000,20000,50000,100000,200000),FUN=sum,border=NA,grid.size=2,sx=Min_long,sy=Min_lat,ex=Max_long,ey=Max_lat)
     
     ClamMap2('Ban',poly.lst=catchgrids[[y]][1:2],title=paste(yrs[y],"Surf Clam Catch"),isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
     ContLegend("bottomright",lvls=catchgrids[[y]]$lvls/10^3,Cont.data=catchgrids[[y]],title="Catch (t)",inset=0.02,cex=0.8,bg='white')
     }
   
   }

  dev.off()



  # CPUE
  cpuegrids = catchgrids


  pdf(file.path( project.datadirectory("offshoreclams"), "figures","BanCPUE.pdf"),11,8)
   
   for(y in which(yrs!=1992)){

      cpuegrids[[y]][[2]]$Z <- catchgrids[[y]][[2]]$Z / effortgrids[[y]][[2]]$Z
      cpuegrids[[y]][[2]]$Z[is.infinite(cpuegrids[[y]][[2]]$Z)] <- NA
      cpuegrids[[y]][[2]]$Z[cpuegrids[[y]][[2]]$Z==0] <- NA
    
      cpuegrids[[y]]$lvls = c(0,0.025.05,0.075,0.1,0.125,0.15,0.175,0.2)

      cols   <- brewer.pal(length(lvls),"YlGnBu") 
      pdata  <- makeProps(na.omit(cpuegrids[[y]][[2]][,1:3]), c(lvls,max(lvls)*100), "col", cols) 
      pdata$border  <- NA
      cpuegrids[[y]][[2]] <- pdata
     
     ClamMap2('Ban',poly.lst=cpuegrids[[y]][1:2],title=paste(yrs[y],"Surf Clam CPUE"),isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
     ContLegend("bottomright",lvls=cpuegrids[[y]]$lvls,Cont.data=cpuegrids[[y]],title=expression(CPUE (kg/m^2)),inset=0.02,cex=0.8,bg='white')
     }
   
   }

  dev.off()








      # depletion test
      test.poly=data.frame(PID=1,POS=1:4,X=c(-59.53,-59.4,-59.53,-59.4),Y=c(44.45,44.45,44.55,44.55))

      test.logs=with(subset(processed.log.data,Year==2013),na.omit(data.frame(X=LON_DD,Y=LAT_DD,C=ROUND_CATCH,E=AREA_TOWED,T=RECORD_DATE)))
      test.logs$EID=1:nrow(test.logs)

      key=findPolys(test.logs,test.poly)

      test.logs=subset(test.logs,EID%in%key$EID)
      test.logs=test.logs[order(test.logs$T),]

      test.logs$CPUE=test.logs$C/test.logs$E
      test.logs$cumC=cumsum(test.logs$C)
      plot(CPUE~cumC,test.logs)

      mod=lm(CPUE~cumC,test.logs)
      abline(mod)

      N1=coef(mod)[1]/-coef(mod)[2]

      u=test.logs$cumC[nrow(test.logs)]/N1


   for(y in which(yrs!=1992)){

    grid.dat=na.omit(subset( processed.log.data ,BANK==1&Year%in%yrs[y],c("LOGRECORD_ID","LON_DD","LAT_DD","RECORD_DATE","ROUND_CATCH","AREA")))
    names(grid.dat)[1:3] <- c("EID","X","Y")
    locData<- findCells(grid.dat, effortgrids[[y]][[1]])
    
    subset(effortgrids[[y]][[2]],Z>threshold)






