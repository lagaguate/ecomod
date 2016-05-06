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

loadfunctions(c("offshoreclams","lobster","utility","spacetime","model.fishery.general"))

RLibrary( "PBSmapping", "lubridate", "trip" ) # Load required packages


## Load Data

update.data=FALSE # TRUE accesses data from database if on a DFO windows machine


  # log data
  #log.data <- GetLogData(update=update.data)
  processed.log.data <- ProcessLogData(GetLogData(update=update.data))
  # save(processed.log.data,file=file.path( project.datadirectory("offshoreclams"), "data", "processedLogdata.Rdata" ))

  #VMS data
  #vms.data <- GetVMSData(update=update.data)
  fisheryList <- ProcessVMSData(GetVMSData(update=update.data),processed.log.data)
  #processed.log.data = fisheryList$log.data
  #processed.vms.data = fisheryList$vms.data
  #load(file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryData.Rdata" ))

  # length frequency data
  lf.data <- GetLFData(update=update.data)
  

  # survey data
  surveyList <- ProcessSurveyData()

####### Mapping

c100 <- read.table(file.path( project.datadirectory("polygons"), "data","Basemaps","Marine","Bathymetry","CHS100.ll"),header=T)
Banq100 <- na.omit(subset(c100,SID==2392)) # 100m isobath for Banqureau

ClamMap2('all',isobath=seq(50,500,50))

 with(subset(processed.log.data,area>0),points(lon_dd,lat_dd,pch=16,cex=0.1,col=rgb(0,0,0,0.1)))
 with(subset(processed.log.data,year==2015&area>0),points(lon_dd,lat_dd,pch=16,cex=0.2,col=rgb(1,0,0,0.2)))
 with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))
 rect(Min_long,Min_lat,Max_long,Max_lat)


ClamMap2('Ban',isobath=seq(50,500,50),title=i,bathy.source='bathy',nafo='all')
 with(subset(processed.log.data,year==2014&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,1,0,0.2)))
 with(subset(processed.log.data,year==2013&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
 with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,0,0,0.2)))


ClamMap2('Grand',isobath=seq(50,500,50))

 rect(Min_long,Min_lat,Max_long,Max_lat)
 with(subset(processed.log.data,year==2013&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
 with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))


 # VMS Data
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","FishingLocations.pdf"),11,8)
  for (i in 2002:2015) {
  ClamMap2('Ban',isobath=seq(50,500,50),title=i,bathy.source='bathy',nafo='all')

   with(subset(processed.log.data,year==i&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(1,0,0,0.2)))
   with(subset(processed.vms.data,year==i),points(lon,lat,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
   legend('bottomright',c("logs","vms"),pch=16,col=c(rgb(1,0,0,0.2),rgb(0,0,0,.1)),cex=c(0.5,0.2))
  }
  dev.off()

  # VMS GIF!!!
  VMSgif(fisheryList,yrs=2013,tail=7,pie.scale=7000,wd=800,ht=600,xlim=c(-60,-57.2),ylim=c(44,45.1))

pdf(file.path( project.datadirectory("offshoreclams"), "figures","VMSLocations.pdf"),11,8)
ClamMap2(xlim=c(-60,-57.2),ylim=c(44.1,45),isobath=seq(50,500,50),bathy.source='bathy')
 with(fisheryList$vms.data,points(lon,lat,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
 dev.off()



# explore distribution of catch and effort data in order to set appropriate bounds to censor the data
pdf(file.path( project.datadirectory("offshoreclams"), "figures","CatchEffortDist.pdf"),8,8)

par(mfrow=c(2,1))#,mar=c(0.2,0.2,0.2,0.2))  
with(subset(processed.log.data,round_catch>0&round_catch<40000),hist(round_catch,breaks=100,xlim=c(0,40000),xlab="Reported Catch by Watch (kg)",main=''))
abline(v=c(1500,30000),col='red',lwd=2)

with(subset(processed.log.data,area>0&area<400000),hist(area,breaks=100,xlim=c(0,400000),xlab="Reported Effort by Watch (m2)",main=''))
abline(v=c(15000,200000),col='red',lwd=2)

  dev.off()
    
## Grid Plots

p=list()
p$bank= "Ban"
p$yrs= list(2004:2015)
#p$yrs= list(1986:2010,2000:2010,2009:2010)
p$effort.threshold = c(15000,200000)
p$catch.threshold = c(1500,30000)
p$cpue.threshold = c(15000)
p$effort.levels = c(1000,50000,100000,200000,500000,1000000)#,2000000,5000000)
p$catch.levels = c(100,5000,10000,20000,50000,100000,200000,500000)
p$cpue.levels = c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.2)
p$effort.cols = "YlGnBu"
p$catch.cols = "YlGnBu"
p$cpue.cols = "YlGnBu"
p$Min_lon = -60.0
p$Max_lon = -57.0
p$Min_lat = 44.0
p$Max_lat = 45.25
p$grid.size = 2
#p$grid.size = 1.852

# Banquereau
Totalgrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='totalVMS',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
p$yrs= list(2004:2006,2005:2007,2006:2008,2007:2009,2008:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
grid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='3yrVMS',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)

p$yrs= 2004:2015

#grid.out <- FisheryGridPlot(fisheryList,p,fn='annualLog',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
AnnGrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='annualVMS',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
write.csv(data.frame(Year=p$yrs,summarize.gridout(AnnGrid.out)),file=file.path( project.datadirectory("offshoreclams"), "R", "SpatialExploitationSummary.csv"),row.names=F )

save(AnnGrid.out,file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryData.Rdata" ))

 
# GrandBank
p$yrs= list(2004:2015)
p$bank= "Grand"
p$Min_lon = -51.5
p$Max_lon = -48.5
p$Min_lat = 43.0
p$Max_lat = 46.5
GrandTotalgrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='GrandTotalVMS',isobath=seq(50,500,50),nafo='all',lg.place="topleft",ht=8,wd=6,outsideBorder=T)#,aspr=1)

## summary table of catch and effort data
Years=1986:2015

Ban.E = with(subset(processed.log.data,bank==1),tapply(area,year,sum,na.rm=T))
Ban.C = with(subset(processed.log.data,bank==1),tapply(round_catch,year,sum,na.rm=T))
Ban = data.frame(Year=as.numeric(names(Ban.C)),Ban.Catch = Ban.C/10^3, Ban.Effort = Ban.E/10^6, Ban.CPUE = Ban.C/Ban.E*1000)

Grand.E = with(subset(processed.log.data,bank==2),tapply(area,year,sum,na.rm=T))
Grand.C = with(subset(processed.log.data,bank==2),tapply(round_catch,year,sum,na.rm=T))
Grand = data.frame(Year=as.numeric(names(Grand.C)),Grand.Catch = Grand.C/10^3, Grand.Effort = Grand.E/10^6, Grand.CPUE = Grand.C/Grand.E*1000)

write.csv(merge(Ban,Grand,all=T),file.path( project.datadirectory("offshoreclams"), "R","CatchEffort.csv"),row.names=F)


pdf(file.path( project.datadirectory("offshoreclams"), "figures","SeasonalFishingPattern.pdf"),8,11)

# Seasonal fishing patterns
p$yrs= 2007:2015
par(mfrow=c(3,3),mar=c(0,0,0,0))
for (i in 1:length(p$yrs)) {
    fishing.season(subset(fisheryList$log.data,year%in%p$yrs[[i]]&bank==1,c('record_date','area')),smooth=0.01,title="")
    mtext("Relative effort",3,-2,cex=1.2,outer=T) 
  }
  # Apparently they fish pretty much all year round except for the winter of 2015, when presumably Banquereau was under 15ft of snow like everywhere else
dev.off()


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
LenWt.data = subset(surveyList$Morphs,survey=="CK2004-01",c("towid","length","total.weight"))
names(LenWt.data)[3] <- "weight"
LenWt2004.fit<-LengthWeight.lme(LenWt.data,random.effect='towid',b.par='estimate')
LengthWeight.plt(LenWt2004.fit,lw=3,ht=8,wd=8,cx=1.5)

# 2010
LenWt.data = subset(surveyList$Morphs,survey=="T12010-01",c("towid","length","total.weight"))
names(LenWt.data)[3] <- "weight"
LenWt2010.fit<-LengthWeight.lme(LenWt.data,random.effect='towid',b.par='estimate')
LengthWeight.plt(LenWt2010.fit,lw=3,ht=8,wd=8,cx=1.5)

l = seq(2.5,200,5)
wal = l^LenWt2010.fit$B * LenWt2010.fit$A 

# LengthFrequencies
FisheryDataList = c(fisheryList,list(lf.data=lf.data))
LengthFrequencies(FisheryDataList, DS="Fishery", bins=seq(0,200,5), Yrs=2009:2014, wal = wal, fn='BanqCatch', rel=F, ymax=40000,ylab="Number of Clams") 


# distribution of surf clams from survey

pdf(file.path( project.datadirectory("offshoreclams"), "figures","SurveyDensity.pdf"),11,8)

for(i in c(2004,2010)){
  
  # interpolate abundance
  interp.data <- na.omit(subset(surveyList$surveyData,year==i&towtype%in%c(1)&towquality%in%c(1,2),c('EID','X','Y','stdcatch')))
  clam.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=5,blank.dist=0.1)

  # define contour lines
  print(clam.contours$str.def)
  lvls=c(1, 15, 35, 75, 150, 300)

  # generate contour lines
  cont.lst<-contour.gen(clam.contours$image.dat,lvls,Banq100,col="YlGn",colorAdj=1)

  # plot Map
  ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all',contours=cont.lst,title=paste("Banqureau Surf Survey Density",i))
  points(Y~X,interp.data,pch=16,cex=0.5,col=rgb(0,0,0,0.5))
  ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="t/km2",inset=0.02,cex=0.8,bty='n')
}
dev.off()


pdf(file.path( project.datadirectory("offshoreclams"), "figures","SurveyCPUEcompare.pdf"),11,8)

for(i in c(2004,2010)){

  # compare survey to fishery catch rates in 2010

  grid.data <- na.omit(subset(surveyList$surveyData,year==i&towtype%in%c(1,4)&towquality%in%c(1,2),c('EID','X','Y','stdcatch')))
  surveygrids<-gridData(grid.data,lvls=c(1, 30, 75, 150, 300, 600),bcol="YlGn",FUN=mean,border=NA,grid.size=p$grid.size,sx=p$Min_lon,sy=p$Min_lat,ex=p$Max_lon,ey=p$Max_lat)

  fisherycpue = AnnGrid.out$grid.polyData$cpue[[as.character(i)]]

  fisherycpue$FisheryClamDensity = fisherycpue$Z*1000
  surveycpue = surveygrids$polyData
  surveycpue$SurveyClamDensity = surveycpue$Z
  comparison.data = merge(fisherycpue[,c("PID","SID","FisheryClamDensity")],surveycpue[,c("PID","SID","SurveyClamDensity")])
  Compare.points = subset(grid.data,EID%in%subset(findPolys(grid.data,surveygrids$polys),paste(PID,SID)%in%with(comparison.data,paste(PID,SID)))$EID)

  plot.comparison = function(comparison.data){
    plot(SurveyClamDensity~FisheryClamDensity,comparison.data,xlim=c(0,515),ylim=c(0,515))
    #abline(lm(SurveyClamDensity~FisheryClamDensity-1,comparison.data))
    abline(a=0,b=1)
  }

  ClamMap2(ylim=c(43.6,45.1), xlim=c(-60.2,-56.8),poly.lst=list(AnnGrid.out$grid,fisherycpue),title=paste("Surf Clam CPUE Comparison",i),isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
  #ContLegend("topleft",lvls=p$cpue.levels*1000,Cont.data=list(AnnGrid.out$grid,fisherycpue2010),title=expression(CPUE (t/km^2)),inset=0.02,cex=0.8,bg='white')
  points(Y~X,Compare.points,pch=21,bg='red')
  points(Y~X,grid.data)
 subplot(plot.comparison(comparison.data),x=-57.3,y=44.08,size=c(2,1.8))
}
dev.off()

 
############## Production model ################



  interp.data <- na.omit(subset(surveyList$surveyData,year==i&towtype%in%c(1,4)&towquality==1,c('EID','X','Y','stdcatch')))

  res = with(interp.data,spacetime.variogram(data.frame(X,Y),stdcatch,methods="gstat")) 
 
  params = c(r=1, K=300000, q=0.1 , B0=200000 )

  # basic Surplus production
  SPmodel(params,1986)



useGrids = with(Totalgrid.out,subset(grid,paste(PID,SID)%in%with(subset(grid.polyData$effort[[1]],Z>100000),paste(PID,SID))))
gridPoints = calcCentroid(useGrids)
ncells = nrow(gridPoints)

r = 5
circles = data.frame(PID=sort(rep(1:ncells,100)),POS=rep(1:100,ncells))

for (i in 1:ncells) {
  bufcircs = bufferCircle(c(gridPoints$X[i],gridPoints$Y[i]),r)
  circles$X[1:100+100*(i-1)] = bufcircs$lon[-101]
  circles$Y[1:100+100*(i-1)] = bufcircs$lat[-101]
}

vmslogdata = assignLogData2VMS(fisheryList, p)
vmslogdata$X = vmslogdata$lon
vmslogdata$Y = vmslogdata$lat

ClamMap2("Ban")
addPolys(circles,col=rgb(1,0,0,0.1),border=rgb(0,0,0,0.1))
with(vmslogdata,points(lon,lat,pch=16,cex=0.2,col=rgb(0,0,0,.1)))


key = findPolys(vmslogdata,circles,maxRows=1e+07)










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
