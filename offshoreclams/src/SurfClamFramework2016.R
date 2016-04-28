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
##  (◦)
##    ^ |)  #E.T.
##  (◦)
##
###############################################################################
# To run in ecomod, run the following commands

loadfunctions(c("offshoreclams","lobster","utility"))

RLibrary( "PBSmapping", "lubridate", "trip" ) # Load required packages


## Load Data

update.data=FALSE # TRUE accesses data from database if on a DFO windows machine


  # log data
  log.data <- GetLogData(update=update.data)
  processed.log.data <- ProcessLogData(log.data)
  #names(processed.log.data) <- tolower(names(processed.log.data))

  #VMS data
  vms.data <- GetVMSData(update=update.data)
  processed.vms.data <- ProcessVMSData(vms.data,processed.log.data)


  load(file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryData.Rdata" ))

  # length frequency data
  lf.data <- GetLFData(update=update.data)
  

  # survey data
  surveyList <- ProcessSurveyData()


####### Mapping

c100 <- read.table(file.path( project.datadirectory("polygons"), "data","Basemaps","Marine","Bathymetry","CHS100.ll"),header=T)
Banq100 <- na.omit(subset(c100,SID==2392)) # 100m isobath for Banqureau

ClamMap2('all',isobath=seq(50,500,50))

 with(subset(processed.log.data,area>0),points(lon_dd,lat_dd,pch=16,cex=0.1,col=rgb(0,0,0,0.1)))
 with(subset(processed.log.data,Year==2015&area>0),points(lon_dd,lat_dd,pch=16,cex=0.2,col=rgb(1,0,0,0.2)))
 with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))
 rect(Min_long,Min_lat,Max_long,Max_lat)

ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all')

 with(subset(processed.log.data,Year==2015&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(1,0,0,0.2)))
 with(subset(processed.log.data,Year==2014&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,1,0,0.2)))
 with(subset(processed.log.data,Year==2013&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
 with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,0,0,0.2)))


ClamMap2('Grand',isobath=seq(50,500,50))

 rect(Min_long,Min_lat,Max_long,Max_lat)
 with(subset(processed.log.data,Year==2013&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
 with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))


# explore distribution of catch and effort data in order to set appropriate bounds to censor the data

par(mfrow=c(2,1))#,mar=c(0.2,0.2,0.2,0.2))  
with(subset(processed.log.data,round_catch>0&round_catch<40000),hist(round_catch,breaks=100,xlim=c(0,40000),xlab="Reported Catch by Watch (kg)",main=''))
abline(v=c(1500,30000),col='red',lwd=2)

with(subset(processed.log.data,area>0&area<400000),hist(area,breaks=100,xlim=c(0,400000),xlab="Reported Effort by Watch (m2)",main=''))
abline(v=c(15000,200000),col='red',lwd=2)

     
## Grid Plots

p=list()
p$bank= "Ban"
p$yrs= 1988:2015
#p$yrs= list(1986:2010,2000:2010,2009:2010)
p$effort.threshold = c(15000,200000)
p$catch.threshold = c(1500,30000)
p$effort.levels = c(1000,50000,100000,200000,500000,1000000,2000000,5000000)
p$catch.levels = c(100,5000,10000,20000,50000,100000,200000,500000)
p$cpue.levels = c(0,0.02,0.04,0.06,0.08,0.1,0.12,0.15,0.2)
p$effort.cols = "YlGnBu"
p$catch.cols = "YlGnBu"
p$cpue.cols = "YlGnBu"
p$Min_lon = -60.0
p$Max_lon = -57.0
p$Min_lat = 44.0
p$Max_lat = 45.25
p$grid.size = 2
#p$grid.size = 1.852

grid.out <- FisheryGridPlot(processed.log.data,p,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
save(grid.out,file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryData.Rdata" ))

 
## summary table of catch and effort data
Years=1986:2015

Ban.E = with(subset(processed.log.data,bank==1),tapply(area,year,sum,na.rm=T))
Ban.C = with(subset(processed.log.data,bank==1),tapply(round_catch,year,sum,na.rm=T))
Ban = data.frame(Year=as.numeric(names(Ban.C)),Ban.Catch = Ban.C/10^3, Ban.Effort = Ban.E/10^6, Ban.CPUE = Ban.C/Ban.E)

Grand.E = with(subset(processed.log.data,bank==2),tapply(area,year,sum,na.rm=T))
Grand.C = with(subset(processed.log.data,bank==2),tapply(round_catch,year,sum,na.rm=T))
Grand = data.frame(Year=as.numeric(names(Grand.C)),Grand.Catch = Grand.C/10^3, Grand.Effort = Grand.E/10^6, Grand.CPUE = Grand.C/Grand.E)

write.csv(merge(Ban,Grand,all=T),file.path( project.datadirectory("offshoreclams"), "R","CatchEffort.csv"),row.names=F)

# Seasonal fishing patterns
p$yrs= 2007:2015
par(mfrow=c(3,3),mar=c(0,0,0,0))
for (i in 1:length(p$yrs)) {
    fishing.season(subset(processed.log.data,year%in%p$yrs[[i]]&bank==b,c('record_date','area')),smooth=0.01,title="")
    mtext("Relative effort",3,-2,cex=1.2,outer=T) 
  }
  # Apparently they fish pretty much all year round except for the winter of 2015, when presumably Banquereau was under 15ft of snow like everywhere else

# distribution of surf clams catch
p$yrs=list(2004:2010,2011:2015)
b=1
pdf(file.path( project.datadirectory("offshoreclams"), "figures","TotalRemovals.pdf"),8,11)

for(i in 1:length(p$yrs)){
  
  # interpolate abundance
  interp.data <- na.omit(subset(processed.log.data,year%in%p$yrs[[i]]&bank==b&lat_dd>Min_lat[b]&lat_dd<Max_lat[b]&lon_dd>Min_long[b]&lon_dd<Max_long[b],c('logrecord_id','lon_dd','lat_dd','round_catch')))
  clam.contours <- interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=F,res=0.01,smooth=T,smooth.fun=sum)

  # define contour lines
  print(clam.contours$str.def)
  # 0.000    9998.709   28722.120   82390.560  202708.380 2950358.365
  lvls=c(5000, 10000, 50000, 100000, 200000, 500000, 1000000)

  # generate contour lines
  cont.lst<-contour.gen(clam.contours$image.dat,lvls,Banq100,col="YlGn",colorAdj=1)

  # plot Map
  ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all',contours=cont.lst,title=paste("Banqureau Surf Clam Removals",min(p$yrs[[i]]),'-',max(p$yrs[[i]])))
  #points(lat_dd~lon_dd,interp.data,pch=16,cex=0.1,col=rgb(0,0,0,0.1))
  ContLegend("bottomright",lvls=lvls/1000,Cont.data=cont.lst$Cont.data,title=expression(t/NM^2),inset=0.02,cex=0.8,bty='n')
}
dev.off()

########### Survey ############

ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
with(subset(surveyList$surveyData,year==2010),segments(slon, slat, elon, elat,col='red'))
with(subset(surveyList$surveyData,year==2010),points(slon, slat,pch=16,cex=0.3,col='red'))
with(subset(surveyList$surveyData,year==2004),segments(slon, slat, elon, elat,col='green'))
with(subset(surveyList$surveyData,year==2004),points(slon, slat,pch=16,cex=0.3,col='green'))

# comparing recorded tow distance with the distance between start and end points
plot(length~dist_m,surveyList$surveyData)
abline(0,1)


# Length - Weight relationship
# 2004
LenWt.data = subset(surveyList$Morphs,Survey=="CK2004-01",c("TowID","Length","Total.Weight"))
names(LenWt.data)[3] <- "weight"
LenWt2004.fit<-LengthWeight.lme(LenWt.data,random.effect='TowID',b.par='estimate')
LengthWeight.plt(LenWt2004.fit,lw=3,ht=8,wd=8,cx=1.5)

# 2010
LenWt.data = subset(surveyList$Morphs,Survey=="T12010-01",c("TowID","Length","Total.Weight"))
names(LenWt.data)[3] <- "weight"
LenWt2010.fit<-LengthWeight.lme(LenWt.data,random.effect='TowID',b.par='estimate')
LengthWeight.plt(LenWt2010.fit,lw=3,ht=8,wd=8,cx=1.5)

l = 1:200
wal = l^LenWt2010.fit$B * LenWt2010.fit$A 

# LengthFrequencies
FisheryDataList = list(Logs=processed.log.data,LenFreq=lf.data)
LengthFrequencies(FisheryDataList, DS="Fishery", bins=seq(0,200,1), Yrs=2005:2014, wal = wal, fn='Banq') {


# distribution of surf clams from survey

pdf(file.path( project.datadirectory("offshoreclams"), "figures","SurveyDensity.pdf"),11,8)

for(i in c(2004,2010)){
  
  # interpolate abundance
  interp.data <- na.omit(subset(surveyList$surveyData,YEAR==i,c('EID','X','Y','stdcatch')))
  clam.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=5,blank.dist=0.1)

  # define contour lines
  print(clam.contours$str.def)
  lvls=c(1, 30, 75, 150, 300, 600)

  # generate contour lines
  cont.lst<-contour.gen(clam.contours$image.dat,lvls,col="YlGn",colorAdj=1)

  # plot Map
  ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all',contours=cont.lst,title=paste("Banqureau Surf Survey Density",i))
  points(Y~X,interp.data,pch=16,cex=0.5,col=rgb(0,0,0,0.5))
  ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="t/km2",inset=0.02,cex=0.8,bty='n')
}
dev.off()




      # depletion test
      test.poly=data.frame(PID=1,POS=1:4,X=c(-59.53,-59.4,-59.53,-59.4),Y=c(44.45,44.45,44.55,44.55))

      test.logs=with(subset(processed.log.data,Year==2013),na.omit(data.frame(X=lon_dd,Y=lat_dd,C=round_catch,E=area_towed,T=record_date)))
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

  min.n=10
   for(y in which(p$yrs!=1992)){
   logCE = na.omit(subset( processed.log.data ,bank==1&Year%in%p$yrs[[y]]&round_catch>0&area>0,c("logrecord_id","lon_dd","lat_dd","record_date","round_catch","area")))
    names(logCE)[1:3] <- c("EID","X","Y")
    locData = findCells(logCE, grid.out$grid)
    logCE = merge(logCE,locData,all=T) 
    logCE = logCE[order(logCE$record_date),]

    logCE$gridID = paste( logCE$PID,logCE$SID,sep='.') 

    Depletion.data = split(logCE,logCE$gridID)
    Depletion.data = Depletion.data[which( unlist(lapply(Depletion.data,nrow))>min.n)]
    for (i in 1:length(Depletion.data)) {
      Depletion.data[[i]]$cumC = cumsum(Depletion.data[[i]]$round_catch)
      Depletion.data[[i]]$CPUE = Depletion.data[[i]]$round_catch/Depletion.data[[i]]$area
    }

    x11()
    par(mfrow=c(6,6),mar=c(0,0,0,0))
    for (i in 1:length(Depletion.data)) {
      plot(CPUE~cumC,Depletion.data[[i]],axes=F)
      mod=lm(CPUE~cumC,Depletion.data[[i]])
      abline(mod)
    }


    stn.lst = assignStation(logCE[,c("EID","X","Y")])
    ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
    addPolys(stn.lst$polys,col=rgb(0,0,1,0.3))
    addPoints(stn.lst$events,pch='.',col='red')

    with(logCE,tapply(round_catch,gridID,length)) 
    polyData = subset(grid.out$grid.polyData[[1]][[28]],Z>p$effort.threshold[1])




##### habitat model
