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

RLibrary( "PBSmapping", "lubridate" ) # Load required packages


## Load Data

update.data=FALSE # TRUE accesses data from database if on a DFO windows machine


  # log data
  log.data <- GetLogData(update=update.data)
  processed.log.data <- ProcessLogData(log.data)

  load(file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryData.Rdata" ))

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
Years=1986:1015

Ban.E = with(subset(processed.log.data,BANK==1),tapply(AREA,Year,sum,na.rm=T))
Ban.C = with(subset(processed.log.data,BANK==1),tapply(ROUND_CATCH,Year,sum,na.rm=T))
Ban = data.frame(Year=as.numeric(names(Ban.C)),Ban.Catch = Ban.C/10^3, Ban.Effort = Ban.E/10^6, Ban.CPUE = Ban.C/Ban.E)

Grand.E = with(subset(processed.log.data,BANK==2),tapply(AREA,Year,sum,na.rm=T))
Grand.C = with(subset(processed.log.data,BANK==2),tapply(ROUND_CATCH,Year,sum,na.rm=T))
Grand = data.frame(Year=as.numeric(names(Grand.C)),Grand.Catch = Grand.C/10^3, Grand.Effort = Grand.E/10^6, Grand.CPUE = Grand.C/Grand.E)

write.csv(merge(Ban,Grand,all=T),file.path( project.datadirectory("offshoreclams"), "R","CatchEffort.csv"),row.names=F)

# distribution of surf clams catch
p$yrs=list(2004:2010,2011:2015)
b=1
pdf(file.path( project.datadirectory("offshoreclams"), "figures","TotalRemovals.pdf"),8,11)

for(i in 1:length(yrs)){
  
  # interpolate abundance
  interp.data <- na.omit(subset(processed.log.data,Year%in%p$yrs[[i]]&BANK==b&LAT_DD>Min_lat[b]&LAT_DD<Max_lat[b]&LON_DD>Min_long[b]&LON_DD<Max_long[b],c('LOGRECORD_ID','LON_DD','LAT_DD','ROUND_CATCH')))
  clam.contours <- interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=F,res=0.01,smooth=T,smooth.fun=sum)

  # define contour lines
  print(clam.contours$str.def)
  # 0.000    9998.709   28722.120   82390.560  202708.380 2950358.365
  lvls=c(5000, 10000, 50000, 100000, 200000, 500000, 1000000)

  # generate contour lines
  cont.lst<-contour.gen(clam.contours$image.dat,lvls,col="YlGn",colorAdj=1)

  # plot Map
  ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all',contours=cont.lst,title=paste("Banqureau Surf Clam Removals",min(p$yrs[[i]]),'-',max(p$yrs[[i]])))
  #points(LAT_DD~LON_DD,interp.data,pch=16,cex=0.1,col=rgb(0,0,0,0.1))
  ContLegend("bottomright",lvls=lvls/1000,Cont.data=cont.lst$Cont.data,title=expression(t/NM^2),inset=0.02,cex=0.8,bty='n')
}
dev.off()

# Seasonal patterns

fishing.season(subset(processed.log.data,Year%in%p$yrs[[i]]&BANK==b))

########### Survey ############

ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
with(subset(catch_analysis,YEAR==2010),segments(SLON, SLAT, ELON, ELAT,col='red'))
with(subset(catch_analysis,YEAR==2010),points(SLON, SLAT,pch=16,cex=0.3,col='red'))
with(subset(catch_analysis,YEAR==2004),segments(SLON, SLAT, ELON, ELAT,col='green'))
with(subset(catch_analysis,YEAR==2004),points(SLON, SLAT,pch=16,cex=0.3,col='green'))


# distribution of surf clams from survey

pdf(file.path( project.datadirectory("offshoreclams"), "figures","SurveyDensity.pdf"),8,11)

for(i in c(2004,2010)){
  
  # interpolate abundance
  lob.contours<-interpolation(subset(lobdat,YEAR==i,c('EID','X','Y','STDCATCH')),ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.03)

  # define contour lines
  print(lob.contours$str.def)
  lvls=c(1, 2, 5, 10, 20, 50)

  # generate contour lines
  cont.lst<-contour.gen(lob.contours$image.dat,lvls,col="YlGn",colorAdj=1)

  # plot Map
  LobsterMap(ylim=c(44.4,45.2),xlim=c(-67.2,-66.3),mapRes="UR",contours=cont.lst,title=paste("SPA 6 Lobster Density",i),isobath=seq(10,500,10),bathcol=rgb(0,0,1,0.2),bathy.source='bathy',boundaries='scallop',poly.lst=list(ScallopAreas,data.frame(PID=c(16,18))))
  points(lat~lon,lobdat,subset=YEAR==i,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
  ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bty='n')
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

  min.n=10
   for(y in which(p$yrs!=1992)){

    logCE = na.omit(subset( processed.log.data ,BANK==1&Year%in%p$yrs[[y]]&ROUND_CATCH>0,c("LOGRECORD_ID","LON_DD","LAT_DD","RECORD_DATE","ROUND_CATCH","AREA")))
    names(logCE)[1:3] <- c("EID","X","Y")
    locData = findCells(logCE, grid.out$grid)
    logCE = merge(logCE,locData,all=T) 
    logCE = logCE[order(logCE$RECORD_DATE),]
    logCE$gridID = paste( logCE$PID,logCE$SID,sep='.') 

    Depletion.data = split(logCE,logCE$gridID)
    Depletion.data = Depletion.data[which( unlist(lapply(Depletion.data,nrow))>min.n)]


    with(logCE,tapply(ROUND_CATCH,gridID,length)) 
    polyData = subset(grid.out$grid.polyData[[1]][[28]],Z>p$effort.threshold[1])






