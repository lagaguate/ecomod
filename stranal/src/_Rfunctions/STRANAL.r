Stranal<-function(year=as.numeric(format(Sys.Date(), "%Y"))-1,
                  type=c(1), 
                  species.code=11, #10-cod;11=Haddock
                  strat.list=c(440:495),
                  wingspread = 41,
                  towdist =1.75,
                  GUI=F
                  ){
#######################################################################################
#######################################################################################
#######################################################################################
###                                                                                 ###
###       Title:                STRANAL                                             ###
###                                                                                 ###
###       Author:               Mike McMahon                                        ###
###                             Mark Fowler                                         ###
###                             Adam Cook                                           ###
###                                                                                 ###
###       Creation Date:        Aug 24, 2015                                        ###
###                                                                                 ###
###       Modification Date:    Aug 31, 2015                                        ###
          stranal.ver = '2015.08.31'
###                                                                                 ###
###       Description:          Replaces the standalone STRANAL application         ###
###                             (written in APL), as well as the numbers and        ###
###                             weights analytic from the old VDC                   ###
###                                                                                 ###
###       Comments:             GUI being created 
###                             US data needs to be incorporated                    ###
#######################################################################################
#######################################################################################
#######################################################################################

#######################################################################################
###                          PACKAGES                                               ###
#######################################################################################
if(!require(RODBC)) { install.packages('RODBC',repos="http://cran.r-project.org")}
if(!require(reshape2)) { install.packages('reshape2',repos="http://cran.r-project.org")}
if(!require(data.table)) { install.packages('data.table',repos="http://cran.r-project.org")}

library(RODBC)
library(reshape2)       #reqd for melt,dcast
library(data.table)

#######################################################################################
###                          ENVIRONMENT                                            ###
#######################################################################################
options(stringsAsFactors = FALSE) 
options(local=TRUE)
options(scipen=999)  # I don't like scientific notation, this avoids it

#######################################################################################
###                          DATABASE CONNECTIONS                                   ###
#######################################################################################
channel<-odbcConnect(uid=oracle.personal.user,
                     pw=oracle.personal.password,
                     dsn=oracle.dsn,
                     case='nochange',
                     rows_at_time=1)

#######################################################################################
###                          FUNCTIONS                                             ###
#######################################################################################
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

#stolen from https://github.com/jae0/ecomod/blob/master/utility/
#            src/_Rfunctions/data.manipulation/na.zero.r
na.zero<-function(x){
  for(i in 1:length(x[1,])){
    if(length(which(is.na(x[,i])))>0){
      x[which(is.na(x[,i])),i]<-0}
  }
  return(x)
} 

#######################################################################################
###                          GUI Stuff                                              ###
#######################################################################################
STRANAL.GUI<-function(){
  #prompt user for type
  get.dfo.type<-function() {
    the.type <- sqlQuery(channel, 
                         paste("select XTYPEDESC, XTYPE 
                               from groundfish.GSXTYPE
                               ORDER BY XTYPE",sep=""))
    the.type<-paste( the.type[,1], " (", the.type[,2],")",sep="") 
    return(the.type)
  }
  #prompt user for year
  get.dfo.year<-function() {
    the.year <- sqlQuery(channel, 
                         paste("select distinct YEAR 
                               from groundfish.gsmissions
                               ORDER BY YEAR DESC",sep=""))
    the.year<-paste( the.year[,1],sep=",") 
    return(the.year)
  }
  #prompt user for species (filter selection if sexed species are required)
  get.dfo.species<-function(dat="FALSE") {
    if (dat=="TRUE"){
      dfo.species.query.tweak<-"AND LFSEXED = 'Y' "
    } else{
      dfo.species.query.tweak<-""
    }
    the.species <- sqlQuery(channel, 
                            paste("SELECT DISTINCT(SPEC), initcap(CNAME), 
                                  LGRP, LFSEXED 
                                  FROM GROUNDFISH.GSSPEC 
                                  WHERE SPEC <> 9999 "
                                  , dfo.species.query.tweak, " 
                                  ORDER BY initcap(CNAME)",sep=""))
    the.species <- paste( the.species[,2], " (", the.species[,1],")",sep="")
    return(the.species)
  }
  #prompt user for strata (filtered by selected year and type)
  get.dfo.strata<-function() {
    if (exists("year.gui")){
      dfo.strata.query.year.tweak=paste0("AND to_char(sdate,'yyyy') = ",year.gui)
    }else{
      dfo.strata.query.year.tweak=""
    }
    if (exists("type.code.gui")){
      dfo.strata.query.type.tweak=paste0("AND type IN (",type.gui,")")
    }else{
      dfo.strata.query.type.tweak=""
    }
    the.strata <- sqlQuery(channel, 
                           paste("select distinct STRAT 
                                 from groundfish.gsinf
                                 WHERE STRAT IS NOT NULL
                                 ",dfo.strata.query.year.tweak,"
                                 ",dfo.strata.query.type.tweak,"
                                 ORDER BY STRAT",sep=""))
    the.strata<-paste( the.strata[,1],sep=",") 
    return(the.strata)
  }
  
  get.dfo.area<-function() {
    if (exists("year.gui")){
      dfo.area.query.year.tweak=paste0("AND to_char(sdate,'yyyy') = ",year.gui)
    }else{
      dfo.area.query.year.tweak=""
    }
    if (exists("type.gui")){
      dfo.area.query.type.tweak=paste0("AND type IN (",type.gui,")")
    }else{
      dfo.area.query.type.tweak=""
    }
    if (exists("strata.gui")){
      dfo.area.query.strata.tweak=paste0("AND strat IN (",strata.gui,")")
    }else{
      dfo.area.query.strata.tweak=""
    }
    the.area <- sqlQuery(channel, 
                         paste("select distinct AREA
                               from groundfish.gsinf
                               WHERE 1=1
                               ",dfo.area.query.year.tweak,"
                               ",dfo.area.query.type.tweak,"
                               ",dfo.area.query.strata.tweak,"
                               ORDER BY AREA",sep=""))
    the.area<-paste( the.area[,1],sep=",") 
    return(the.area)
  }
   by.sex<-select.list(c("Unsexed Analysis","Sexed Analysis"),multiple=F, graphics=T, title='Analysis by Sex?')
  if (by.sex=="Sexed Analysis"){
    by.sex.gui="TRUE"
    sex.title="Choose a (sexed) species:"
  }else{
    by.sex.gui="FALSE"
    sex.title="Choose a species:"
  }
  
  year.gui<-select.list(paste(get.dfo.year(),sep=","),multiple=F, graphics=T, title='Choose a year:')
  gstri <- sqlQuery(channel, 
                    paste("select MISSION, VESEL, CRUNO, SEASON
                               from groundfish.gsmissions
                          WHERE YEAR = ",year.gui," AND
                          SEASON IN ('SUMMER','SPRING','FALL','4VWCOD','GEORGES')", sep=""))
  
  type.gui<-select.list(get.dfo.type(), multiple=T, graphics=T, title='Choose trawl type:')
  #preselect=c(1), 
  type.gui<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', type.gui) )
  
  species.gui<-select.list(get.dfo.species(by.sex.gui),multiple=F, graphics=T, title=sex.title)
  species.code.gui<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', species.gui)) 
  species.name.gui<-gsub("\\s?\\(.*?\\)", "", species.gui)
  
  strata.gui<-select.list(paste(get.dfo.strata(),sep=","),multiple=T, graphics=T, title=paste0("Choose the strata (",year.gui," only):"))
  #preselect=c(440:495), 
  strata.gui<-paste(unlist(gsub("(.*)","'\\1'",strata.gui)),sep="",collapse=",")  
  area.gui<-select.list(paste(get.dfo.area(),sep=","), preselect=paste(get.dfo.area(),sep=","),multiple=T, graphics=T, title=paste0("Choose the area(s):"))
  area.gui<-paste(unlist(gsub("(.*)","'\\1'",area.gui)),sep="",collapse=",") 
  
  wingspread = as.numeric(select.list(c("41","34"),preselect=c("41"),multiple=F, graphics=T, title='wingspread (ft)'))
  towdist =1.75 #no way to change this via GUI yet
  GUI.results<-list(wingspread, towdist, by.sex.gui, year.gui,species.code.gui, species.name.gui, type.gui,strata.gui,gstri, area.gui)
  return(GUI.results)
}
#######################################################################################
#######################################################################################

if (GUI==T){
  stranal.mode<-"GUI"
  GUI.results<-STRANAL.GUI()
  wingspread<-GUI.results[[1]]
  towdist<-GUI.results[[2]]
  by.sex<-GUI.results[[3]]
  year<-GUI.results[[4]]
  species.code<-GUI.results[[5]]
  species.name<-GUI.results[[6]]
  these.type<-GUI.results[[7]]
  these.strat<-GUI.results[[8]]
  gstri<-GUI.results[[9]]
  these.areas<-GUI.results[[10]]
  these.missions<-paste(unlist(gsub("(.*)","'\\1'",gstri$MISSION)),sep="",collapse=",")
}else{

  stranal.mode<-"command-line"
  gstri<-sqlQuery(channel, paste("select * from groundfish.gsmissions 
                                 WHERE SEASON IN ('SUMMER','SPRING','FALL','4VWCOD','GEORGES')
                                 AND YEAR = ",year, sep=""))
  #convert some information into a format that can be sent to SQL "IN" statments
  these.type  <- paste(unlist(gsub("(.*)","'\\1'",type)),sep="",collapse=",")
  these.strat  <- paste(unlist(gsub("(.*)","'\\1'",strat.list)),sep="",collapse=",")  
  these.missions<-paste(unlist(gsub("(.*)","'\\1'",gstri$MISSION)),sep="",collapse=",")
  these.areas<-"not implemented"
  species.name <- sqlQuery(channel, 
                          paste("SELECT initcap(CNAME)
                                  FROM GROUNDFISH.GSSPEC 
                                  WHERE SPEC =",species.code ,sep=""))

}
print(paste0("running Stranal version ",stranal.ver))
input_parameters<-list("Stranal version"=paste(stranal.ver, "(",stranal.mode,")",sep=""),
                       
                       "Analysis Date"=Sys.time(),
                       "Experiment Type"=these.type, 
                       "Strata"=these.strat, 
                       "Missions"=these.missions,
                       "Year"=year,
                       "Species"=paste0(species.code, if (exists("species.name")){paste0(" (",species.name,")")}),
                       "Wingspread"=wingspread, 
                       "Distance"=towdist,
                       "Data Source"="DFO",
                       "ALK Modifications"="No",
                       "Area"="None")

#######################################################################################
###                          DATABASE EXTRACTIONS                                   ###
###  Do them initially so that they don't need to be run each time                  ###
#######################################################################################
#limit extraction by the users' selections
raw_gscat_query <- 
  paste("select mission,setno,size_class,totwgt,sampwgt,totno,calwt
          from 
          groundfish.gscat 
          where 
          spec=",species.code,"
          and mission IN (",these.missions,")", sep="")
raw_gscat<-sqlQuery(channel,raw_gscat_query)
#added mission, strat and type filters
raw_gsinf_query<-
  paste("select i.mission, i.setno,sdate,time,strat,area,slat,slong,dmin,dmax,depth,dur,dist
          from 
          groundfish.gsinf i 
          where 
          i.MISSION IN (",these.missions,")
          AND strat IN (",these.strat,")
          AND type IN (",type,")", sep="")
raw_gsinf<-sqlQuery(channel,raw_gsinf_query)
#Provide data to build age:length keys and age composition. 
#converted to ANSI join
raw_gsdet_query<-
  paste("select mission,setno,size_class,fsex,age,fwt,
          decode(1,1,flen,2,.5+2*floor(flen/2),3,1+3*floor(flen/3),flen) flen,
          lgrp binwidth,lfsexed bysex, clen
          from 
          groundfish.gsdet g 
            LEFT OUTER JOIN groundfish.gsspec s ON (s.spec=g.spec)
          WHERE MISSION IN (",these.missions,")
          AND G.SPEC=",species.code,"
        ", sep="")
raw_gsdet<-sqlQuery(channel,raw_gsdet_query)
#calculate strat areas into tunits (access to GROUNDFISH.GS_STRATUM reqd?)
#US nautical mile is 6080.2ft
tunits_query<-
  paste("SELECT strat, area SQNM, nvl(area,0)/(",towdist,"*(",wingspread,"/6080.2)) tunits 
          FROM 
          GROUNDFISH.GSSTRATUM 
          WHERE 
          strat IN (",these.strat,")", sep="")
tunits<-sqlQuery(channel,tunits_query)
#tunits$WTS = tunits$TUNITS/sum(tunits$TUNITS)
    #hydrographic data at trawl depth - Not required for STRANAL
    #ctd made from joining dataframe gstri to extraction from gshyd
    #added mission filter to reduce extraction size
    gshyd_source_query<-
      paste("select mission, setno, sdepth, gear, temp, sal, bid
                  from 
                  groundfish.gshyd
                  where 
                  gear in (1,2) 
                  and mission IN (",these.missions,") 
                  and bid='B' 
                  and temp is not null",sep="")
    gshyd_source<-sqlQuery(channel,gshyd_source_query)
    pre<-merge(gshyd_source,gstri, by='MISSION', all.x=F)
    bot<-pre[which(pre$GEAR==1),]
    ctd<-pre[which(pre$GEAR==2),]
    
    bottom<-rbind(ctd, bot)
    bottom<-bottom[!duplicated(bottom[c("MISSION","SETNO")]),]


#done with our database connection - close it
odbcClose(channel)
#######################################################################################
###                          STRATA AREA1                                           ###
#######################################################################################
strata_area<-tunits[order(tunits$STRAT),c("STRAT","TUNITS","SQNM")]
strata_tunits<-tunits[order(tunits$STRAT),c("STRAT","TUNITS")]

#######################################################################################
###                          NUMBERS AND WEIGHTS                                    ###
###    WEIGHT:
###        WEIGHT BY SET
###        WEIGHT MEAN
###        WEIGHT TOTAL
###        WEIGHT TOTAL STANDARD ERROR
###        BIOMASS
###        BIOMASS STANDARD ERROR
###
###    NUMBERS:
###        NUMBERS BY SET
###        NUMBERS MEAN
###        NUMBERS TOTAL
###        NUMBERS TOTAL STANDARD ERROR
###        ABUNDANCE
###        ABUNDANCE STANDARD ERROR
#######################################################################################
nw_by_set<-merge(raw_gsinf, raw_gscat, all.x=T)
nw_by_set<-merge(nw_by_set,gstri, all.x=T)
nw_by_set[which(is.na(nw_by_set$TOTWGT)),c('SAMPWGT','TOTWGT','TOTNO','CALWT')] <- 0
nw_by_set$SIZE_CLASS[which(is.na(nw_by_set$SIZE_CLASS))] <- 1
nw_by_set$RAW_TOTWGT <-nw_by_set$TOTWGT
nw_by_set$TOTWGT <- (nw_by_set$TOTWGT*towdist)/nw_by_set$DIST
nw_by_set$RAW_TOTNO <-nw_by_set$TOTNO 
nw_by_set$TOTNO <- (nw_by_set$TOTNO*towdist)/nw_by_set$DIST
nw_by_set<-nw_by_set[order(nw_by_set$STRAT,nw_by_set$SETNO),]

set_info<-nw_by_set[order(nw_by_set$STRAT,nw_by_set$SETNO),c("MISSION","SEASON","STRAT","SETNO","SDATE","AREA","SLAT","SLONG","DMIN","DMAX","DEPTH","DUR","DIST" )]
nw_by_set_final<-nw_by_set[order(nw_by_set$STRAT,nw_by_set$SETNO),c("STRAT","MISSION","SETNO","TOTNO","TOTWGT")]

nw_by_set2<-nw_by_set[order(nw_by_set$STRAT,nw_by_set$SETNO),c("STRAT","SLAT","SLONG","AREA","SETNO","TOTWGT","TOTNO")]

nw_by_set2<-merge(nw_by_set2,strata_tunits)

nw_by_set2$BIOMASS<-nw_by_set2$TOTWGT*nw_by_set2$TUNITS
nw_by_set2$ABUND<-nw_by_set2$TOTNO*nw_by_set2$TUNITS

nw_by_strata.cnt<-aggregate(list(COUNT=nw_by_set2$STRAT), by=list(STRAT=nw_by_set2$STRAT), FUN=length)
nw_by_strata.sum<-aggregate(list(TOT_WGT=nw_by_set2$TOTWGT,TOT_NO=nw_by_set2$TOTNO), by=list(STRAT=nw_by_set2$STRAT), FUN=sum)
nw_by_strata.mean<-aggregate(list(MEAN_WGT=nw_by_set2$TOTWGT,MEAN_NO=nw_by_set2$TOTNO,BIOMASS=nw_by_set2$BIOMASS,ABUND=nw_by_set2$ABUND), by=list(STRAT=nw_by_set2$STRAT), FUN=mean)
nw_by_strata.sterr<-aggregate(list(ST_ERR_WGT=nw_by_set2$TOTWGT,ST_ERR_NO=nw_by_set2$TOTNO,ST_ERR_BIOMASS=nw_by_set2$BIOMASS,ST_ERR_ABUND=nw_by_set2$ABUND), by=list(STRAT=nw_by_set2$STRAT), FUN=st.err)

nw<-merge(nw_by_strata.cnt,nw_by_strata.sum,by="STRAT")
nw<-merge(nw, nw_by_strata.mean,by="STRAT")
nw<-merge(nw, nw_by_strata.sterr,by="STRAT")

numbers<-nw[,c("STRAT","COUNT","TOT_NO","MEAN_NO","ABUND","ST_ERR_NO","ST_ERR_ABUND")]
weights<-nw[,c("STRAT","COUNT","TOT_WGT","MEAN_WGT","BIOMASS","ST_ERR_WGT","ST_ERR_BIOMASS")]

#######################################################################################
###                          STRATA AREAS                                           ###
###    Strat
###    Tunits
###    SQNM
###    AreaProp
###    AreaPropStErr
###    AreaTot
###    AreaTotStErr
#######################################################################################
catchsets<-nw_by_set2
catchsetskeeps <- c("STRAT","TOTWGT","TOTNO")
catchsets<-catchsets[catchsetskeeps]
catchsets$somecatch[catchsets$TOTWGT!=0 | catchsets$TOTNO!=0]<-1
catchsets$somecatch[is.na(catchsets$somecatch)]<-0
catchsets<-merge(strata_area,catchsets,by="STRAT",all.y=T)  #dropping strata with no catches
catchsets<-merge(catchsets,nw_by_strata.cnt,by="STRAT",all.x=T)
catchsets$Area<-catchsets$SQNM*catchsets$somecatch

catchsets.AreaProp<-aggregate(list(AreaProp=catchsets$somecatch), by=list(STRAT=catchsets$STRAT), FUN=mean)
catchsets.AreaPropStErr<-aggregate(list(AreaPropStErr=catchsets$somecatch), by=list(STRAT=catchsets$STRAT), FUN=st.err)
catchsets.AreaTot<-aggregate(list(AreaTot=catchsets$Area), by=list(STRAT=catchsets$STRAT), FUN=mean)
catchsets.AreaTotStErr<-aggregate(list(AreaTotStErr=catchsets$Area), by=list(STRAT=catchsets$STRAT), FUN=st.err)
strata.areas<-merge(strata_area,catchsets.AreaProp,by="STRAT")
strata.areas<-merge(strata.areas,catchsets.AreaPropStErr,by="STRAT")
strata.areas<-merge(strata.areas,catchsets.AreaTot,by="STRAT")
strata.areas<-merge(strata.areas,catchsets.AreaTotStErr,by="STRAT")
#######################################################################################
###                          SET UP AGELEN                                          ###
#######################################################################################
#merge on particular fields by=c("MISSION", "SETNO","SIZE_CLASS")
agelen<-merge(nw_by_set, raw_gsdet, by=c("MISSION", "SETNO","SIZE_CLASS"), all.x=T)
agelen<-merge(agelen,tunits, all.x=T)
agelen$FLEN<-floor(agelen$FLEN/agelen$BINWIDTH)*agelen$BINWIDTH
agelen$CAGE<-NA

#if sampwgt is 0 or NA and totwgt is not null or 0 then replace sample weigt with total weight 
#need to see if this is consistent
iu = which(agelen$SAMPWGT ==0)
iw = which(is.na(agelen$SAMPWGT))
ii = which(agelen$TOTWGT>0)

iiu = intersect(ii,iu)
if(length(iiu)>0) {
  agelen$SAMPWGT[iiu] = agelen$TOTWGT[iiu]
}

iiw = intersect(ii,iw)
if(length(iiw)>0) {
  agelen$SAMPWGT[iiw] = agelen$TOTWGT[iiw]
}

ie = which(agelen$SAMPWGT >0)
io = which(!is.na(agelen$SAMPWGT))
iy = which(agelen$SAMPWGT==0)
  #if sampwgt > 0, use it to calculate cage
  st1<- union(ie,io)
  if (length(st1) >0){
    agelen$CAGE[st1]<-agelen$RAW_TOTWGT[st1]/agelen$SAMPWGT[st1]*(towdist/agelen$DIST[st1])*agelen$CLEN[st1]
  }
  #if sampwgt ==0, cage ==0
  st0 = intersect(io,iy)
  if (length(st0) >0){
    agelen$CAGE[st0]<-0
  }
  #if sampwgt is na/null, cage is na/null
  stNA<-which(is.na(agelen$SAMPWGT))
  if (length(stNA)>0){
    agelen$CAGE[stNA]<-NA
  }
#defaults
agelen<-merge(agelen,bottom, all.x=T)  
agelen$DEPTH[is.na(agelen$DEPTH)]<--99
agelen$DMIN[is.na(agelen$DMIN)]<--99
agelen$DMAX[is.na(agelen$DMAX)]<--99
agelen$SDEPTH[is.na(agelen$SDEPTH)]<--99.9  
agelen$TEMP[is.na(agelen$TEMP)]<--99.99     
agelen$SAL[is.na(agelen$SAL)]<--99.999  
agelen$FLEN<-agelen$FLEN+(agelen$BINWIDTH*.5)-.5
agelen$LOCTIME<-agelen$TIME
agelen$TIME<-NULL

allFlen<-sort(unique(agelen$FLEN[!is.na(agelen$FLEN)]))
#######################################################################################
###                          LENGTH ANALYTICS                                       ###
###    LENGTH BY SET
###    LENGTH MEAN
###    LENGTH MEAN STANDARD ERROR 
###    LENGTH TOTAL
###    LENGTH TOTAL STANDARD ERROR
#######################################################################################
#length by set sheet (not formatted for output)
lset<-agelen[,c("STRAT","SLAT","SLONG","AREA","MISSION","SETNO","FLEN","CAGE")]
li = which(lset$CAGE==0)
lset$FLEN[li] = unique(lset$FLEN)[1]
##length by set
lset <- aggregate(CAGE~STRAT+SLAT+SLONG+AREA+MISSION+SETNO+FLEN,data=lset,FUN=sum)
#lset[lset$STRAT==440,]
lset<-lset[with(lset,order(lset$STRAT,lset$MISSION,lset$SETNO,lset$FLEN,lset$SLAT,lset$SLONG,lset$AREA)),]
lset<-melt(lset, id.vars = c("STRAT","SLAT","SLONG","AREA","MISSION","SETNO","FLEN"))


length_by_set <- na.zero(dcast(lset, STRAT + MISSION + SETNO  ~ FLEN))
length_by_set<-length_by_set[order(length_by_set$STRAT,length_by_set$SETNO),]
length_total<-merge(strata_tunits,length_by_set)
#add row_tots to length_by_set
length_by_set$TOTAL<-rowSums(length_by_set[,4:length(length_by_set)])

#separate the length and non-length-related data for the sets of the dataframe
length_total_pre  <-length_total[,c("STRAT","TUNITS")]
length_total_strat_data       <-length_total[,c(1,5:ncol(length_total))]
#capture all of the column names of the data
colNames<-names(length_total_strat_data)
#count sets/strata
length_total_strat_data.cnt<-aggregate(list(COUNT=length_total_strat_data$STRAT), by=list(STRAT=length_total_strat_data$STRAT), FUN=length)

#add an additional rowname for the column totals 
length_rownames<-c(length_total_strat_data.cnt[,1],'Total')
length_rownames_noTotal<-c(length_total_strat_data.cnt[,1])
#mean length
length_mean<-setNames(aggregate(list(length_total_strat_data[,2:ncol(length_total_strat_data)]), by=list(STRAT=length_total_strat_data$STRAT), FUN=mean), colNames)
  length_mean<-length_mean[,-1] #drop strata column
  length_mean<-cbind(length_mean,RowTotals=rowSums(length_mean)) #add rowsums
  length_mean<-cbind(STRAT=length_rownames_noTotal,length_mean)        #add rowname

#mean length std error 
length_mean_se<-setNames(aggregate(list(length_total_strat_data[,c(1,2:ncol(length_total_strat_data))]), by=list(STRAT=length_total_strat_data$STRAT), FUN=st.err), colNames)
  length_mean_se<-length_mean_se[,-1] #drop strata column
  length_mean_se<-cbind(STRAT=length_rownames_noTotal,length_mean_se)            #add rowname

#multiply all length data by tunits for "total length"
length_total_strat_data_tunits<-cbind(STRAT=length_total_pre$STRAT,length_total[,c(5:ncol(length_total))]*length_total_pre$TUNITS)

#total length
length_total<-setNames(aggregate(list(length_total_strat_data_tunits[,2:ncol(length_total_strat_data_tunits)]), by=list(STRAT=length_total_strat_data_tunits$STRAT), FUN=mean), colNames)
  length_total<-length_total[,-1] #drop strata column
  length_total<-cbind(length_total,RowTotals=rowSums(length_total)) #add rowsums
  ColTotalsLength=colSums(length_total) #separated this because I use it in age calculations
  length_total<-rbind(length_total,ColTotals=ColTotalsLength) #add colsums
  length_total<-cbind(STRAT=length_rownames,length_total)          #add rowname

#total length std error 
length_total_se<-setNames(aggregate(list(length_total_strat_data_tunits[,2:ncol(length_total_strat_data_tunits)]), by=list(STRAT=length_total_strat_data_tunits$STRAT), FUN=st.err), colNames)
  length_total_se<-length_total_se[,-1] #drop strata column
  length_total_se<-cbind(STRAT=length_rownames_noTotal,length_total_se)          #add rowname


#######################################################################################
###                          AGE LENGTH KEY                                         ###
#######################################################################################

alk<-agelen[,c("AGE","FLEN","CAGE","SETNO")]
alk<-alk[!is.na(alk$AGE), ]
if (nrow(alk)<1){
  age_by_set<-"can't do age_by_set - no ages in data"
  ages<-"can't do ages - no ages in data"
  age_length_key_totals<-"can't do age_length_key_totals - no ages in data"
  age_table<-"can't do age_table - no ages in data"
  age_length_weight<-"can't do age_length_weight - no ages in data"
}else{
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
age_length_key_naked<-na.zero(t(tapply(alk$SETNO, list(alk$AGE, alk$FLEN), function(x) {sum(x/x)})))
#age_length_key<-rbind(age_length_key,Age_Totals)  #these results seem fine
allFlenDF<-data.frame(allFlen) #all possiblem lengths
rownames(allFlenDF)<-allFlenDF$allFlen
age_length_key<-merge(allFlenDF,age_length_key_naked,by="row.names", all=TRUE)
age_length_key<-na.zero(age_length_key[order(age_length_key$allFlen),])
age_length_key$Row.names<-NULL
row.names(age_length_key)<-age_length_key[,1]
age_length_key$allFlen<-NULL
Length_Totals<-rowSums(age_length_key, dims = 1)
age_length_key_totals<-cbind(age_length_key,Length_Totals)
Age_Totals<-colSums(age_length_key_totals, dims = 1)
age_length_key_totals<-rbind(age_length_key_totals,Age_Totals)
#age length weight sheet
alw<-aggregate(FWT~AGE+FLEN,data=agelen,FUN=mean)
#necess for columns in asc order of age
alw<-alw[order(alw$AGE,alw$FLEN),]
alw$FWT = alw$FWT / 1000
age_length_weight = na.zero(reshape(alw,idvar='FLEN',timevar='AGE',direction='wide'))
#if want to add all ages, should do it here!
rownames(age_length_weight)<-age_length_weight[,1]
age_length_weight<-merge(allFlenDF,age_length_weight,by="row.names", all=TRUE)
age_length_weight<-na.zero(age_length_weight[order(age_length_weight$allFlen),])
age_length_weight$FLEN<-NULL
age_length_weight$Row.names<-NULL
#######################################################################################
###                          AGE CALCULATIONS                                       ###
#######################################################################################
lengths<-ColTotalsLength[1:length(ColTotalsLength)-1] #total lengths
ages_prop<-prop.table(as.matrix(age_length_key),1) #proportions of the different sizes of each age
ages_prop<-ifelse(is.nan(ages_prop),0,ages_prop)
ages_prop<-as.data.frame(ages_prop)
theseages<-c(names(ages_prop))
age_table<-ages_prop*lengths
ages_prop$FLEN<-as.numeric(rownames(ages_prop))
ageset<-lset[,c("STRAT","MISSION","SETNO","FLEN","value")]
colnames(ageset)[which(names(ageset) == "value")] <- "CAGE"
ageset<-ageset[order(ageset$STRAT,ageset$MISSION, ageset$SETNO),]

ages_pre<-merge(ageset,ages_prop, by="FLEN")
#ages_pre$MISSION<-NULL
ages_pre<-as.data.table(ages_pre)

ages_pre[, (theseages) := lapply(.SD, function(x) x * ages_pre[['CAGE']] ), .SDcols = theseages]

age_by_set<-aggregate(.~STRAT + MISSION + SETNO, data=ages_pre, sum)
age_by_set<-age_by_set[order(age_by_set$STRAT,age_by_set$SETNO),]
#mission was removed to allow aggregation, don't need these, but keeping 
#them in case I need them to add the mission back
age_by_set$FLEN<-NULL 
age_by_set$CAGE<-NULL

age_mean<-age_by_set
age_mean$SETNO<-NULL
age_mean$MISSION<-NULL
age_mean<-aggregate(.~STRAT, data=age_mean, mean)
setnames(age_mean,old=names(age_mean[,2:ncol(age_mean)]), new=c(paste0("age_",theseages,"_mean")))

age_mean_se<-age_by_set
age_mean_se$SETNO<-NULL
age_mean_se$MISSION<-NULL
age_mean_se<-aggregate(.~STRAT, data=age_mean_se, st.err)
setnames(age_mean_se,old=names(age_mean_se[,2:ncol(age_mean_se)]), new=c(paste0("age_",theseages,"_se")))

age_pretotal<-as.data.table(merge(strata_area,age_by_set, by="STRAT"))
age_pretotal$SQNM<-NULL
age_pretotal[, (theseages) := lapply(.SD, function(x) x * age_pretotal[['TUNITS']] ), .SDcols = theseages]
age_pretotal$TUNITS<-NULL

age_total<-age_pretotal
age_total$MISSION<-NULL
age_total$SETNO<-NULL
age_total<-aggregate(.~STRAT, data=age_total, mean)
setnames(age_total,old=names(age_total[,2:ncol(age_total)]), new=c(paste0("age_",theseages,"_tot")))

age_total_se<-age_pretotal
age_total_se$MISSION<-NULL
age_total_se$SETNO<-NULL
age_total_se<-aggregate(.~STRAT, data=age_total_se, st.err)
setnames(age_total_se,old=names(age_total_se[,2:ncol(age_total_se)]), new=c(paste0("age_",theseages,"_tot_se")))

age_by_set.cnt<-aggregate(list(COUNT=age_by_set$STRAT), by=list(STRAT=age_by_set$STRAT), FUN=length)
ages<-merge(age_by_set.cnt,age_mean)
ages<-merge(ages,age_mean_se)
ages<-merge(ages,age_total)
ages<-na.zero(merge(ages,age_total_se))
}

#######################################################################################
###                          RESULTS                                                ###
#######################################################################################

results<-list(input_parameters,
              strata.areas, 
              set_info,
              length_by_set, 
              length_mean, 
              length_mean_se, 
              length_total, 
              length_total_se,
              nw_by_set_final,   
              weights,
              numbers, 
              age_table, 
              age_length_key_totals,
              age_length_weight,
              age_by_set,  
              ages)
#r_results              == STRANAL results sheet
#---------------------------------------
#input_parameters         == QUERY
#strata.areas             == Strata Area                    (good - TUNITS; SQNM)
#                         == Prop Area                      (good - AreaProp)
#                         == Prop Area Std Err              (good - AreaPropStErr)
#                         == Total Area                     (good - AreaTot)
#                         == Total Area Std Area            (good - AreaTotStErr)
#set_info(*)              == set specific information       (*)
#age_length_key_totals    == Age Length Key                 (good)
#age_table                == Age Table                      (good - avg lengths not included)             
#age_length_weight        == Age Length Weight              (good - avg weights not included)
#length_by_set            == Length by Set                  (good)
#length_mean              == Length Mean                    (good - column totals not included)
#length_mean_se           == Length Mean Standard Error     (good - row and column totals not included)
#length_total             == Length Total                   (good)
#length_total_se          == Length Total Standard Error    (good - row and column totals not included)
#age_by_set               == Age By Set                     (good)
#ages                     == Age Mean                       (good - column=age_x_mean)
#                         == Age Mean Std Error             (good - column=age_x_se)
#                         == Age Total                      (good - column=age_x_tot)
#                         == Age Total Standard Error       (good - column=age_x_tot_se)
#nw_by_set                == Weight By Set                  (good - column=TOTWGT)
#                         == Numbers By Set (*)             (*)
#weights                  == Weight Mean                    (good - column=MEAN_WGT)
#                         == Weight Mean Std Err            (good - column=ST_ERR_WGT)
#                         == Weight (BIOMASS) Total         (good - column=BIOMASS)
#                         == Weight (BIOMASS) Total Std Err (good - column=ST_ERR_BIOMASS)
#numbers(*)               == Numbers (ABUNDANCE) Total      (*)
#                         == Numbers  (ABUNDANCE) Total Std Err (*)
#                         == Numbers Mean                   (*)
#                         == Numbers Mean Std Err           (*)

# * not in original STRANAL
return(results)
}

# #example function call
# results<-Stranal(year=2010, species.code=12, strat.list=c(440:495))
# resultsGUI<-Stranal(GUI=T)
# results <-Stranal(GUI=F)