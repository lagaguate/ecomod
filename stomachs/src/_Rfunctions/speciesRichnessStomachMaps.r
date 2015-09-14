#TO make a map of the fish species and length classes represented in each location this starts using the bathy1.Rdata workspace in the R-manuals folder
#Making the base map

###########################RUN THE FOLLOWING CODE PRIOR TO FUNCTION   #######################################################################################
########				options(stringsAsFactors=F)																								            #
########						 source("C:/Users/CookA/Desktop/Scripts/MPA stomach data/maps of species richness distrbutions.r")                          #
########							load("C:\\Documents and Settings\\cooka\\Desktop\\Scripts\\GOMS 2009\\bathy1.RData")                                    #
########							require(PBSmapping) ;require(mgcv);require(RColorBrewer); require(reshape)                                              #
########							isob<-c(100,200)                                                                                                        #
########                            source("R:\\Science\\Population Ecology Division\\Shared\\Adam Cook\\rmaps\\convert.dd.dddd.r")                         #
########							bathy.poly<-list(NULL)                                                                                                  #
########							for(i in 1:length(isob)){                                                                                               #
########								bathy.cl<-contourLines(bathy.1,levels=isob[i])                                                                      #
########								bathy.cp<-convCP(bathy.cl)                                                                                          #
########								bathy.poly[[i]]<-bathy.cp$PolySet                                                                                   #
########								attr(bathy.poly[[i]],"projection")<-"LL"                                                                            #
########							}                                                                                                                       #
########							ilty=c(2,1)                                                                                                             #
########							atl<-read.table("R:\\Science\\Population Ecology Division\\Shared\\Adam Cook\\rmaps\\atlanticsHigh.ll", header=T)       #
########							atl.1<-thinPolys(atl,0.01)                                                                                              #
########							attr(atl.1,"projection")<-"LL"                                                                                          #
########							require(RODBC)                                                                                                          #
########							channel<-odbcDriverConnect("MSORCL32.dll")                                                                              #
########                                                                                                                                                    #
#############################################################################################################################################################

#
#data.source='all','stom','sur'
#species='finfish','inv','all','sand'
#sizes='R','S' (s is for <15 finfish ie those that are prey) April 19, 2011 10:08:36 AM 
#IQR multiplier is for the handling of outliers from the model output, it is an integer 1.5*IQR is the standard for outliers and if IQR.mult=1.5 then all points with values < or >1.5*IQR will get lumped into groups
#res is the sizes of the blocks for plotting and combining data, it is a balance between number of sets within a block and fine scale patterns, it is the number of decimal degrees and has been set at 0.27



s.rich<-function(data.source,species, effort.corrected=T,res=0.27, sizes='R',IQR.mult=1.5) {

	if(grepl('INV',toupper(species))) {s1=1700; s2=9000; spp='Invertebrate'}
	if(grepl('FI',toupper(species))) {s1=1; s2=1100; spp='Finfish'}
	if(grepl('ALL',toupper(species))) {s1=1; s2=9000; spp='Total'}
	if(grepl('SAND',toupper(species))) {s1=610; s2=610; spp='Sandlance'}
	if(grepl('SUR',toupper(data.source)) & effort.corrected==T & sizes=='R') {
			species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			  NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			  (SELECT mission          ,
			    setno                  ,
			    spec
			     FROM groundfish.gscat
			    where spec between ",s1," and ",s2," and spec not in (1091,1092,1093,1094,1095)) d 
			  WHERE i.mission=d.mission(+)
			AND i.setno      =d.setno(+)
			;",sep=""))
			
			
			species$X<-convert.dd.dddd(species$XDDMMSS)
			species$Y<-convert.dd.dddd(species$YDDMMSS)
			species<-na.omit(species)
			species$EID<-seq(1:nrow(species))
			event<-species[,c(8,9,10)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(11,12)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			species2<-unique(species[,c(1,2,3,13,14)])

			#NUMBER OF SETS IN A SQUARE -- effort correction

			pcount<-aggregate(species2$P,by=list(species2$yid),FUN=sum) #effort number of tows in each square
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]
			pdat$count<-pcount[match(pdat[,1],pcount[,1]),2]

			outs<-gam(TOT~s(count,k=length(unique(pdat$count))), data=pdat,family='poisson')
			pdat$resid<-outs$residuals
			a3<-c(median(pdat$resid)-IQR.mult*IQR(pdat$resid),median(pdat$resid)+IQR.mult*IQR(pdat$resid))
			a3[1] <- ifelse(a3[1]<min(pdat$resid), min(pdat$resid), a3[1])
			a3[2] <- ifelse(a3[2]>max(pdat$resid), max(pdat$resid), a3[2])
			pdat$resid <- ifelse(pdat$resid>=a3[2],a3[2],pdat$resid)
			pdat$resid <- ifelse(pdat$resid<=a3[1],a3[1],pdat$resid)
			brks<-seq(a3[1],a3[2],by=(a3[2]-a3[1])/10)
			lbrks<-length(brks)
			cols<-c(rev(brewer.pal(5,'Blues')),brewer.pal(6,'Reds'))
			pdat[,c(5,6)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(4,5,6)]<-c('Z','PID','SID')
			pdata<-pdat[,c(6,5,4)]
			pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)
			pdata<-makeProps(pdata,brks,"col",cols)
			
			layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			par(mar=c(8,8,2,0), cex.axis=1)
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
			addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')

			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			legend(ncol=1,x=-0.1,y=0.7,c('High','','','','','','','','','','Low'), fill=rev(cols),cex=.75,bty='n') #10
			arrows(x0=0.5,y0=0.66,x1=0.5,y1=0.50,length=0.05)
			mtext(side=3,paste(spp,'Diversity'),outer=T,line=-9)
			mtext(side=3,'Survey Data- Effort Corrected',outer=T,line=-10.5,cex=.75)
 }
#not effort corrected
if(grepl('SUR',toupper(data.source)) & effort.corrected==F & sizes=='R') {
			species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			  NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			  (SELECT mission          ,
			    setno                  ,
			    spec
			     FROM groundfish.gscat
			    where spec between ",s1," and ",s2," and  spec not in (1091,1092,1093,1094,1095)) d 
			  WHERE i.mission=d.mission(+)
			AND i.setno      =d.setno(+)
			;",sep=""))
			species$X<-convert.dd.dddd(species$XDDMMSS)
			species$Y<-convert.dd.dddd(species$YDDMMSS)
			species<-na.omit(species)
			species$EID<-seq(1:nrow(species))
			event<-species[,c(8,9,10)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(11,12)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]
			brks<-round(seq(floor(min(pdat$TOT)),ceiling(max(pdat$TOT)),by=(ceiling(max(pdat$TOT))-floor(min(pdat$TOT)))/5),0)
			lbrks<-length(brks)
			cols<-brewer.pal(6,'Reds')
			pdat[,c(3,4)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(2,3,4)]<-c('Z','PID','SID')
			pdata<-pdat[,c(4,3,2)]
			 pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			 pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)
			pdata<-makeProps(pdata,brks,"col",cols)
			layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			 par(mar=c(8,8,2,0), cex.axis=1)
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
			addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')


			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			legend(ncol=1,x=-0.1,y=0.7,brks, fill=(cols),cex=.75,bty='n',title='N Spec')
			 mtext(side=3,paste(spp,'Diversity'),outer=T,line=-9)
			  mtext(side=3,'Survey Data- no effort correction',outer=T,line=-10.5,cex=.75)
 } 
 
##############################################################################################

 #stomach data only
 
 if(grepl('STO',toupper(data.source)) & effort.corrected==T  & sizes=='R') {	 
			species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			  nvl(spec1,0) spec1,
			  nvl(sample_index,0) sample_index,
			  NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			    (SELECT distinct MISSION,SETNO,spec spec1,sample_index,SPECCD2 spec  FROM SDSTO S, PREY_SPEC_DETAILS P WHERE PREYSPECCD=SPECCD AND SPECCD2 between ",s1," and ",s2,") s 
			  where i.mission=s.mission(+) 
			  and i.setno=s.setno(+)
			;",sep=""))
			
			
			species$X<-convert.dd.dddd(species$XDDMMSS)
			species$Y<-convert.dd.dddd(species$YDDMMSS)
			species<-na.omit(species)
			species$EID<-seq(1:nrow(species))
			event<-species[,c(10,11,12)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(13,14)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			species2<-unique(species[,c(8,15,16)])   #number of fish
			species3<-unique(species[,c(1,2,15,16)]) #number of sets
			species4<-unique(species[,c(7,15,16)])   #number of species
			#NUMBER OF Stomachs IN A SQUARE
			pcount<-aggregate(species2$P,by=list(species2$yid),FUN=sum)    #number of fish   
			psets<-aggregate(species3$P,by=list(species3$yid),FUN=sum)     #number of sets   
			pspecies<-aggregate(species4$P,by=list(species4$yid),FUN=sum)  #number of species
			pcount$sets<-psets[match(pcount[,1],psets[,1]),2]
			pcount$species<-pspecies[match(pcount[,1],pspecies[,1]),2]
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]
			
			pcount<-psets
			
			# changed from number of fish sampled to number of sets in each block July 13, 2011 01:03:14 PM 
			
			pdat$count<-pcount[match(pdat[,1],pcount[,1]),2]
			
			outs<-gam(TOT~s(count), data=pdat,family='poisson')
				pdat$resid<-outs$residuals
				pcount$resid<-pdat[match(pcount[,1],pdat[,1]),4] 
				pcount$SPP<-pdat[match(pcount[,1],pdat[,1]),2] 
				#par(mfrow=c(3,3))
				names(pcount)<-c('YID','NSets','Resdiuals','NPreyspecies')
			
			#correlation matrix with rs below the diagonal and p's above
			#cor.prob <- function(X, dfr = nrow(X) - 2) {
			#	 R <- cor(X)
			#	 above <- row(R) < col(R)
			#	 r2 <- R[above]^2
			#	 Fstat <- r2 * dfr / (1 - r2)
			#	 R[above] <- 1 - pf(Fstat, 1, dfr)
			#	 R
			#}
			#cor.prob(pcount[,c(2,3,4,6)])
			 a3<-c(median(pdat$resid)-IQR.mult*IQR(pdat$resid),median(pdat$resid)+IQR.mult*IQR(pdat$resid))
			 a3[1] <- ifelse(a3[1]<min(pdat$resid), min(pdat$resid), a3[1])
			 a3[2] <- ifelse(a3[2]>max(pdat$resid), max(pdat$resid), a3[2])
			 pdat$resid <- ifelse(pdat$resid>=a3[2],a3[2],pdat$resid)
			 pdat$resid <- ifelse(pdat$resid<=a3[1],a3[1],pdat$resid)
			brks<-seq(a3[1],a3[2],by=(a3[2]-a3[1])/10)
			lbrks<-length(brks)
			cols<-c(rev(brewer.pal(5,'Blues')),brewer.pal(6,'Reds'))
			pdat[,c(5,6)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(4,5,6)]<-c('Z','PID','SID')
			pdata<-pdat[,c(6,5,4)]
			 pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			 pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)
			pdata<-makeProps(pdata,brks,"col",cols)
			
			layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			par(mar=c(8,8,2,0), cex.axis=1)
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
			addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')

			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			 mtext(side=3,paste(spp,'Diversity'),outer=T,line=-9)
			  mtext(side=3,'Stomach Data- Effort Corrected',outer=T,line=-10.5,cex=.75)
			  legend(ncol=1,x=-0.1,y=0.7,c('High','','','','','','','','','','Low'), fill=rev(cols),cex=.75,bty='n') #10
			 arrows(x0=0.5,y0=0.66,x1=0.5,y1=0.50,length=0.05)
			 
}
#########################################################################################################################################################
if(grepl('STO',toupper(data.source)) & effort.corrected==F  & sizes=='R') {	 
			species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			   NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			    (SELECT distinct MISSION,SETNO,spec spec1,sample_index,SPECCD2 spec  FROM SDSTO S, PREY_SPEC_DETAILS P WHERE PREYSPECCD=SPECCD AND SPECCD2 between ",s1," and ",s2,") s 
			  where i.mission=s.mission(+) 
			  and i.setno=s.setno(+)
			;",sep=""))
			species$X<-convert.dd.dddd(species$XDDMMSS)
			species$Y<-convert.dd.dddd(species$YDDMMSS)
			species<-na.omit(species)
			species$EID<-seq(1:nrow(species))
			event<-species[,c(8,9,10)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(11,12)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]
			brks<-round(seq(floor(min(pdat$TOT)),ceiling(max(pdat$TOT)),by=(ceiling(max(pdat$TOT))-floor(min(pdat$TOT)))/5),0)
			lbrks<-length(brks)
			cols<-brewer.pal(6,'Reds')
			pdat[,c(3,4)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(2,3,4)]<-c('Z','PID','SID')
			pdata<-pdat[,c(4,3,2)]
			 pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			 pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)

			layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			 par(mar=c(8,8,2,0), cex.axis=1)
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
			addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')

			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			legend(ncol=1,x=-0.1,y=0.7,brks, fill=(cols),cex=.75,bty='n',title='N Spec')
			 mtext(side=3,paste(spp,'Diversity'),outer=T,line=-9)
			  mtext(side=3,'Stomach Data- no effort correction',outer=T,line=-10.5,cex=.75)
}

if(grepl('ALL',toupper(data.source)) & effort.corrected==F  & sizes=='R') {
			species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			  NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			  ((SELECT mission          ,
			    setno                  ,
			    spec
			     FROM groundfish.gscat
			    where spec between ",s1," and ",s2," and spec not in (1091,1092,1093,1094,1095)) union (SELECT distinct MISSION,SETNO,SPECCD2 spec  FROM SDSTO S, PREY_SPEC_DETAILS P WHERE PREYSPECCD=SPECCD AND SPECCD2 between ",s1," and ",s2,") ) d
			  WHERE i.mission=d.mission(+)
			AND i.setno      =d.setno(+)
			;",sep=""))
			
			species$X<-convert.dd.dddd(species$XDDMMSS)
			species$Y<-convert.dd.dddd(species$YDDMMSS)
			species<-na.omit(species)
			species$EID<-seq(1:nrow(species))
			event<-species[,c(8,9,10)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(11,12)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]
			brks<-round(seq(floor(min(pdat$TOT)),ceiling(max(pdat$TOT)),by=(ceiling(max(pdat$TOT))-floor(min(pdat$TOT)))/5),0)
			lbrks<-length(brks)
			cols<-brewer.pal(6,'Reds')
			pdat[,c(3,4)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(2,3,4)]<-c('Z','PID','SID')
			pdata<-pdat[,c(4,3,2)]
			 pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			 pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)
			pdata<-makeProps(pdata,brks,"col",cols)
			layout(matrix(c(1,1,2,2),2, 2, byrow=F),widths=c(21,1.9))
			 par(mar=c(8,8,2,0), cex.axis=1)
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
			addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')

			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			legend(ncol=1,x=-0.1,y=0.7,brks, fill=(cols),cex=.75,bty='n',title='N Spec')
			 mtext(side=3,paste(spp,'Diversity'),outer=T,line=-9)
			  mtext(side=3,'Survey & Stomach Data- no effort correction',outer=T,line=-10.5,cex=.75)
 }
 
 
if(grepl('ALL',toupper(data.source)) & effort.corrected==T  & sizes=='R') {
	species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			  dep,
			  temp,
			  sal,
			  si,
			  NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep, 
			    bottom_salinity sal
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			  ((SELECT mission          ,
			    setno                  ,
			    spec, 0 si
			     FROM groundfish.gscat
			    where spec between ",s1," and ",s2," and spec not in (1091,1092,1093,1094,1095)) union (SELECT distinct MISSION,SETNO,SPECCD2 spec, sample_index si  
			    FROM SDSTO S, PREY_SPEC_DETAILS P WHERE PREYSPECCD=SPECCD AND SPECCD2 between ",s1," and ",s2,") ) d
			  WHERE i.mission=d.mission(+)
			AND i.setno      =d.setno(+)
			;",sep=""))
			
			species1a<-unique(species[,c(1,2,10)])
			species1a<-species1a[species1a[,3]>0,]
			species1b<-aggregate(species1a[,3],by=list(species1a$MISSION, species1a$SETNO), FUN=length)
			names(species1b)<-c('MISSION','SETNO','NSTOMS')
			
			species<-merge(species,species1b,by=c('MISSION','SETNO'),all=T)
			species<-species[,-10]
			species$NSTOMS[is.na(species$NSTOMS)]<-0
					species$X<-convert.dd.dddd(species$XDDMMSS)
					species$Y<-convert.dd.dddd(species$YDDMMSS)
					species<-na.omit(species)
					species$EID<-1:nrow(species)
						
			event<-species[,c(12,13,14)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(15,16)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]		
			
			species2<-unique(species[,c(1,2,11,17,18)])   #number of stomachs
			species3<-unique(species[,c(1,2,17,18)]) #number of sets
			 
			#NUMBER OF Stomachs IN A SQUARE
			pcount<-aggregate(species2$NSTOMS,by=list(species2$yid),FUN=sum)    #number of fish   
			psets<-aggregate(species3$P,by=list(species3$yid),FUN=sum)     #number of sets   
						pcount$sets<-psets[match(pcount[,1],psets[,1]),2]
			pdat[,c(3,4)]<-pcount[match(pdat[,1],pcount[,1]),c(2,3)]
			names(pdat)<-c('yid','TOT','count','sets')
			#number of species versus effort in each block
			                    
			#       July 13, 2011 01:41:25 PM  changed effort to number of sets only 
			
			
			outs<-gam(TOT~s(sets),data=pdat,family='poisson')
						
		
			pdat$resid<-outs$residuals
			 a3<-c(median(pdat$resid)-IQR.mult*IQR(pdat$resid),median(pdat$resid)+IQR.mult*IQR(pdat$resid))
			 a3[1] <- ifelse(a3[1]<min(pdat$resid), min(pdat$resid), a3[1])
			 a3[2] <- ifelse(a3[2]>max(pdat$resid), max(pdat$resid), a3[2])
			 pdat$resid <- ifelse(pdat$resid>=a3[2],a3[2],pdat$resid)
			 pdat$resid <- ifelse(pdat$resid<=a3[1],a3[1],pdat$resid)
			brks<-seq(a3[1],a3[2],by=(a3[2]-a3[1])/10)
			lbrks<-length(brks)
			cols<-c(rev(brewer.pal(5,'Blues')),brewer.pal(6,'Reds'))
			pdat[,c(6,7)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(5,6,7)]<-c('Z','PID','SID')
			pdata<-pdat[,c(7,6,5)]
			 pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			 pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)
			pdata<-makeProps(pdata,brks,"col",cols)
			 layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			 par(mar=c(8,8,2,0), cex.axis=1)
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
			addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')

			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			 mtext(side=3,paste(spp,'Diversity'),outer=T,line=-9)
			  mtext(side=3,'Combined Data- Effort Corrected',outer=T,line=-10.5,cex=.75)
			  legend(ncol=1,x=-0.1,y=0.7,c('High','','','','','','','','','','Low'), fill=rev(cols),cex=.75,bty='n') #10
			 arrows(x0=0.5,y0=0.66,x1=0.5,y1=0.50,length=0.05)
									
}

if(grepl('SUR',toupper(data.source)) & effort.corrected==T & sizes=='S') {
			species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			  NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			  (SELECT distinct mission          ,
			    setno                  ,
			    spec
			     FROM groundfish.gsdet
			    where spec between 10 and 1000
			    and flen<=15) d 
			  WHERE i.mission=d.mission(+)
			AND i.setno      =d.setno(+)
			;",sep=""))
			
			species$X<-convert.dd.dddd(species$XDDMMSS)
			species$Y<-convert.dd.dddd(species$YDDMMSS)
			species<-na.omit(species)
			species$EID<-seq(1:nrow(species))
			event<-species[,c(8,9,10)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(11,12)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			species2<-unique(species[,c(1,2,3,13,14)])
			#NUMBER OF SETS IN A SQUARE
			pcount<-aggregate(species2$P,by=list(species2$yid),FUN=sum) #effort number of tows in each square
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]
			pdat$count<-pcount[match(pdat[,1],pcount[,1]),2]

			outs<-gam(TOT~s(count,k=length(unique(pdat$count))),data=pdat,family='poisson')
			pdat$resid<-outs$residuals
			 a3<-c(median(pdat$resid)-IQR.mult*IQR(pdat$resid),median(pdat$resid)+IQR.mult*IQR(pdat$resid))
			 a3[1] <- ifelse(a3[1]<min(pdat$resid), min(pdat$resid), a3[1])
			 a3[2] <- ifelse(a3[2]>max(pdat$resid), max(pdat$resid), a3[2])
			 pdat$resid <- ifelse(pdat$resid>=a3[2],a3[2],pdat$resid)
			 pdat$resid <- ifelse(pdat$resid<=a3[1],a3[1],pdat$resid)
			brks<-seq(a3[1],a3[2],by=(a3[2]-a3[1])/10)

			lbrks<-length(brks)
			cols<-c(rev(brewer.pal(5,'Blues')),brewer.pal(6,'Reds'))
			pdat[,c(5,6)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(4,5,6)]<-c('Z','PID','SID')
			pdata<-pdat[,c(6,5,4)]
			 pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			 pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)
			pdata<-makeProps(pdata,brks,"col",cols)
			
			 layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			 par(mar=c(8,8,2,0), cex.axis=1)
			      browser()
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
			browser()
			addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			legend(ncol=1,x=-0.1,y=0.7,c('High','','','','','','','','','','Low'), fill=rev(cols),cex=.75,bty='n') #10
			arrows(x0=0.5,y0=0.66,x1=0.5,y1=0.50,length=0.05)
			mtext(side=3,paste('Small Fish Diversity'),outer=T,line=-9)
			mtext(side=3,'Survey Data- Effort Corrected',outer=T,line=-10.5,cex=.75)
			}

if(grepl('STO',toupper(data.source)) & effort.corrected==T  & sizes=='S') {	 
			species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			  nvl(spec1,0) spec1,
			  nvl(sample_index,0) sample_index,
			  NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			    (SELECT distinct MISSION,SETNO,spec spec1,sample_index,SPECCD2 spec  FROM SDSTO S, PREY_SPEC_DETAILS P WHERE PREYSPECCD=SPECCD AND SPECCD2 between 10 and 1000 and plen<15) s 
			    		  where i.mission=s.mission(+) 
			  and i.setno=s.setno(+)
			;",sep=""))
			
			
			species$X<-convert.dd.dddd(species$XDDMMSS)
			species$Y<-convert.dd.dddd(species$YDDMMSS)
			species<-na.omit(species)
			species$EID<-seq(1:nrow(species))
			event<-species[,c(10,11,12)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(13,14)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			species2<-unique(species[,c(8,15,16)])   #number of fish
			species3<-unique(species[,c(1,2,15,16)]) #number of sets
			species4<-unique(species[,c(7,15,16)])   #number of species

			#NUMBER OF Stomachs IN A SQUARE
			pcount<-aggregate(species2$P,by=list(species2$yid),FUN=sum)    #number of fish   
			psets<-aggregate(species3$P,by=list(species3$yid),FUN=sum)     #number of sets   
			pspecies<-aggregate(species4$P,by=list(species4$yid),FUN=sum)  #number of species
			pcount$sets<-psets[match(pcount[,1],psets[,1]),2]
			pcount$species<-pspecies[match(pcount[,1],pspecies[,1]),2]
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]
			
			# July 13, 2011 01:43:00 PM  changed so that effort is number of sets
			
			pcount<-psets
			pdat$count<-pcount[match(pdat[,1],pcount[,1]),2]
			#number of species versus effort in each block
			
			#REMOVING THE EFFORT EFFECT FOR STOMACHS IS BASED ON THE NUMBER OF INDIVIDUALS SAMPLES AS THE NUMBER OF INDIVIDUALS SAMPLED WILL BE A FUNCTION OF THE NUMBER OF SETS IN THE AREA AND
			#THE NUMBER OF SPECIES PRESENT, BY REGRESSING THE NUMBER OF PREY ITEMS ON THE NUMBER OF STOMACHS CAN GET AROUND THIS, SORT OF AN AREA SPECIFIC (NOT SPECIES SPECIFIC) SPECIES ACCUMULATION CURVE IDEA
			
			outs<-gam(TOT~s(count,k=length(unique(pdat$count))),data=pdat,family='poisson')
			pdat$resid<-outs$residuals
			
			 a3<-c(median(pdat$resid)-IQR.mult*IQR(pdat$resid),median(pdat$resid)+IQR.mult*IQR(pdat$resid))
			 a3[1] <- ifelse(a3[1]<min(pdat$resid), min(pdat$resid), a3[1])
			 a3[2] <- ifelse(a3[2]>max(pdat$resid), max(pdat$resid), a3[2])
			 pdat$resid <- ifelse(pdat$resid>=a3[2],a3[2],pdat$resid)
			 pdat$resid <- ifelse(pdat$resid<=a3[1],a3[1],pdat$resid)
			 pcount$resid<-pdat[match(pcount[,1],pdat[,1]),4] 
			pcount$SPP<-pdat[match(pcount[,1],pdat[,1]),2] 

			names(pcount)<-c('YID','NSets','Residuals','NPreyspecies')
			brks<-seq(a3[1],a3[2],by=(a3[2]-a3[1])/10)
			lbrks<-length(brks)
			cols<-c(rev(brewer.pal(5,'Blues')),brewer.pal(6,'Reds'))
			pdat[,c(5,6)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(4,5,6)]<-c('Z','PID','SID')
			pdata<-pdat[,c(6,5,4)]
			 pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			 pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)
			pdata<-makeProps(pdata,brks,"col",cols)

			 layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			 par(mar=c(8,8,2,0), cex.axis=1)
					plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
					addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')

			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			 mtext(side=3,paste('Small Fish Diversity'),outer=T,line=-9)
			  mtext(side=3,'Stomach Data- Effort Corrected',outer=T,line=-10.5,cex=.75)
			  legend(ncol=1,x=-0.1,y=0.7,c('High','','','','','','','','','','Low'), fill=rev(cols),cex=.75,bty='n') #10
			 arrows(x0=0.5,y0=0.66,x1=0.5,y1=0.50,length=0.05)
			 
}
#########################################################################################################################################################
if(grepl('ALL',toupper(data.source)) & effort.corrected==T  & sizes=='S') {
	species<-sqlQuery(channel,paste(" SELECT i.mission           ,
			  i.setno                   ,
			  TO_CHAR(sdate,'yyyy') YEAR,
			  strat                     ,
			  slat YDDMMSS              ,
			  slong*-1 XDDMMSS          ,
			  dep,
			  temp,
			  sal,
			  si,
			  NVL(spec,0) spec
			   FROM
			  (SELECT mission          ,
			    setno                  ,
			    sdate                  ,
			    strat                  ,
			    slat,slong,
			    bottom_temperature temp,
			    (dmin+dmax)/2 dep, 
			    bottom_salinity sal
			     FROM groundfish.gsinf
			    WHERE TO_CHAR(sdate,'mm') IN ('06','07','08') and to_char(sdate,'yyyy') in (1999,2000,2001,2002,2005,2006,2007,2008)
			  AND strat BETWEEN '440' AND '495'
			  AND type IN (1,5)
			  ) i,
			  ((SELECT mission          ,
			    setno                  ,
			    spec, 0 si
			     FROM groundfish.gsdet
			    where spec between 10 and 1000 and flen<15 ) union (SELECT distinct MISSION,SETNO,SPECCD2 spec, sample_index si  
			    FROM SDSTO S, PREY_SPEC_DETAILS P WHERE PREYSPECCD=SPECCD AND SPECCD2 between 10 and 1000) ) d
			  WHERE i.mission=d.mission(+)
			AND i.setno      =d.setno(+)
			;",sep=""))
			
			species1a<-unique(species[,c(1,2,10)])
			species1a<-species1a[species1a[,3]>0,]
			species1b<-aggregate(species1a[,3],by=list(species1a$MISSION, species1a$SETNO), FUN=length)
			names(species1b)<-c('MISSION','SETNO','NSTOMS')
			species<-merge(species,species1b,by=c('MISSION','SETNO'),all=T)
			species<-species[,-10]
			species$NSTOMS[is.na(species$NSTOMS)]<-0

					species$X<-convert.dd.dddd(species$XDDMMSS)
					species$Y<-convert.dd.dddd(species$YDDMMSS)
					species<-na.omit(species)
					species$EID<-1:nrow(species)
			event<-species[,c(12,13,14)]
			grid<-makeGrid(x=seq(-68,-55.5,res), y=seq(41.5,48.5,res), projection="LL")
			locData<-findCells(event,grid)
			species[,c(15,16)]<-locData[match(species$EID, locData$EID),c(2,3)]
			species$yid<-paste(species$PID,".",species$SID)
			species$P<-1
			pdat<-cast(species, yid~SPEC,fun.aggregate=mean,fill=0,value="P")
			pdat$TOT<-rowSums(pdat[,c(2:ncol(pdat))])
			pdat<-pdat[,c(1,ncol(pdat))]		
			species2<-unique(species[,c(1,2,11,17,18)])   #number of stomachs
			species3<-unique(species[,c(1,2,17,18)]) #number of sets

			#NUMBER OF Stomachs IN A SQUARE
			pcount<-aggregate(species2$NSTOMS,by=list(species2$yid),FUN=sum)    #number of fish   
			psets<-aggregate(species3$P,by=list(species3$yid),FUN=sum)     #number of sets   
						pcount$sets<-psets[match(pcount[,1],psets[,1]),2]
			pdat[,c(3,4)]<-pcount[match(pdat[,1],pcount[,1]),c(2,3)]
			names(pdat)<-c('yid','TOT','count','sets')

			#number of species versus effort in each block
						outs<-gam(TOT~s(sets),data=pdat,family='poisson')
			pdat$resid<-outs$residuals

			 a3<-c(median(pdat$resid)-IQR.mult*IQR(pdat$resid),median(pdat$resid)+IQR.mult*IQR(pdat$resid))
			 a3[1] <- ifelse(a3[1]<min(pdat$resid), min(pdat$resid), a3[1])
			 a3[2] <- ifelse(a3[2]>max(pdat$resid), max(pdat$resid), a3[2])
			 pdat$resid <- ifelse(pdat$resid>=a3[2],a3[2],pdat$resid)
			 pdat$resid <- ifelse(pdat$resid<=a3[1],a3[1],pdat$resid)
			brks<-seq(a3[1],a3[2],by=(a3[2]-a3[1])/10)
			lbrks<-length(brks)
			cols<-c(rev(brewer.pal(5,'Blues')),brewer.pal(6,'Reds'))
			pdat[,c(6,7)]<-as.data.frame(do.call("rbind",(strsplit(pdat$yid,"\\."))))
			names(pdat)[c(5,6,7)]<-c('Z','PID','SID')
			pdata<-pdat[,c(7,6,5)]
			 pdata[,1]<-as.numeric(pdata[,1])
			pdata[,2]<-as.numeric(pdata[,2])
			 pdata[,3]<-as.numeric(pdata[,3])
			pdata<-as.PolyData(pdata)
			pdata<-makeProps(pdata,brks,"col",cols)
			 layout(matrix(c(1,1,2,2), 2, 2, byrow=F),widths=c(21,1.9))
			 par(mar=c(8,8,2,0), cex.axis=1)
			plotMap(atl.1,col= "khaki"   , xlim=c(-68,-56.7), ylim=c(41.8,47.5))
			for(i in length(isob):1){
				addLines(bathy.poly[[i]], col="black", lty=ilty[[i]])
			}
			box()
			addPolys(grid,polyProps=pdata)
			addPolys(atl.1,col='khaki')

			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')# plot nothing
			 mtext(side=3,paste('Small Fish Diversity'),outer=T,line=-9)
			  mtext(side=3,'Combined Data- Effort Corrected',outer=T,line=-10.5,cex=.75)
			  legend(ncol=1,x=-0.1,y=0.7,c('High','','','','','','','','','','Low'), fill=rev(cols),cex=.75,bty='n') #10
			 arrows(x0=0.5,y0=0.66,x1=0.5,y1=0.50,length=0.05)
									
}
	}

	