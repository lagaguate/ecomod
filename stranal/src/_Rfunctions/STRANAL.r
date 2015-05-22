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
###       Creation Date:        Nov 5, 2014                                         ###
###                                                                                 ###
###       Modification Date:    May 1, 2015                                         ###
###                                                                                 ###
###       Description:          Replaces the standalone STRANAL application         ###
###                             (written in APL), as well as the numbers and        ###
###                             weights analytic from the old VDC                   ###
###                                                                                 ###
###       Comments:             US data needs to be incorporated                    ###
#######################################################################################
#######################################################################################
#######################################################################################

#######################################################################################
###                          CONSTANTS/USER PARAMETERS                              ###
#######################################################################################
year          =2010
type          =c(1)
species.code  =10
strat.list    =c(440:495)
wingspread    = 41          #e.g. Western IIa=41;  Yankee=34
towdist       =1.75         #abundant evidence suggests that this varies by set

#######################################################################################
###                          PACKAGES                                               ###
#######################################################################################
library(RODBC)
library(plyr)
library(reshape2)

#######################################################################################
###                          ENVIRONMENT                                            ###
#######################################################################################
options(stringsAsFactors = FALSE) 
loadfunctions("utility")
options(scipen=999)

#######################################################################################
###                          DIRECTORY PATHS                                        ###
#######################################################################################
wd <- file.path(project.datadirectory('stranal'),'data')

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

#######################################################################################
###                          READ IN DATA                                           ###
#######################################################################################
#can we surveys from the db? Maintaining this table is not a good approach 
survey_list<-read.csv(file=file.path(wd,"survey_list.csv"), head=T, sep=",")
names(survey_list) = toupper(names(survey_list))
survey_list$MISSION<-paste(survey_list$VESEL,survey_list$YEAR,sprintf("%03d",survey_list$CRUNO),sep="")

#capture relevant missions for use in later filters
gstri <- subset(survey_list , (survey_list$SERIES %in% c('SUMMER','SPRING','FALL','4VWCOD','GEORGES')))
gstri<-gstri[which(gstri$YEAR==year),]

#convert some information into a format that can be sent to SQL "IN" statments
these.type  <- paste(unlist(gsub("(.*)","'\\1'",type)),sep="",collapse=",")
these.strat  <- paste(unlist(gsub("(.*)","'\\1'",strat.list)),sep="",collapse=",")  
these.missions<-paste(unlist(gsub("(.*)","'\\1'",gstri$MISSION)),sep="",collapse=",")

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

#######################################################################################
###                          STRATA AREA                                            ###
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
nw_by_set<-merge(nw_by_set,survey_list, all.x=T)
nw_by_set[which(is.na(nw_by_set$TOTWGT)),c('SAMPWGT','TOTWGT','TOTNO','CALWT')] <- 0
nw_by_set$SIZE_CLASS[which(is.na(nw_by_set$SIZE_CLASS))] <- 1
nw_by_set$RAW_TOTWGT <-nw_by_set$TOTWGT
nw_by_set$TOTWGT <- (nw_by_set$TOTWGT*towdist)/nw_by_set$DIST
nw_by_set$RAW_TOTNO <-nw_by_set$TOTNO 
nw_by_set$TOTNO <- (nw_by_set$TOTNO*towdist)/nw_by_set$DIST
nw_by_set<-nw_by_set[order(nw_by_set$STRAT,nw_by_set$SETNO),c("STRAT","SLAT","SLONG","AREA","SETNO","TOTWGT","TOTNO")]

nw_by_set<-merge(nw_by_set,strata_tunits)
nw_by_set$BIOMASS<-nw_by_set$TOTWGT*nw_by_set$TUNITS
nw_by_set$ABUND<-nw_by_set$TOTNO*nw_by_set$TUNITS

nw_by_strata.cnt<-aggregate(list(COUNT=nw_by_set$STRAT), by=list(STRAT=nw_by_set$STRAT), FUN=length)
nw_by_strata.sum<-aggregate(list(TOT_WGT=nw_by_set$TOTWGT,TOT_NO=nw_by_set$TOTNO), by=list(STRAT=nw_by_set$STRAT), FUN=sum)
nw_by_strata.mean<-aggregate(list(MEAN_WGT=nw_by_set$TOTWGT,MEAN_NO=nw_by_set$TOTNO,BIOMASS=nw_by_set$BIOMASS,ABUND=nw_by_set$ABUND), by=list(STRAT=nw_by_set$STRAT), FUN=mean)
nw_by_strata.sterr<-aggregate(list(ST_ERR_WGT=nw_by_set$TOTWGT,ST_ERR_NO=nw_by_set$TOTNO,ST_ERR_BIOMASS=nw_by_set$BIOMASS,ST_ERR_ABUND=nw_by_set$ABUND), by=list(STRAT=nw_by_set$STRAT), FUN=st.err)

nw<-merge(nw_by_strata.cnt,nw_by_strata.sum,by="STRAT")
nw<-merge(nw, nw_by_strata.mean,by="STRAT")
nw<-merge(nw, nw_by_strata.sterr,by="STRAT")

numbers<-nw[,c("STRAT","COUNT","TOT_NO","MEAN_NO","ABUND","ST_ERR_NO","ST_ERR_ABUND")]
weights<-nw[,c("STRAT","COUNT","TOT_WGT","MEAN_WGT","BIOMASS","ST_ERR_WGT","ST_ERR_BIOMASS")]

#######################################################################################
###                          SET UP AGELEN                                          ###
#######################################################################################
#merge on particular fields by=c("MISSION", "SETNO","SIZE_CLASS")
agelen<-merge(raw_gscat_gsinf, raw_gsdet, by=c("MISSION", "SETNO","SIZE_CLASS"), all.x=T)
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
lset <- aggregate(CAGE~STRAT+SLAT+SLONG+AREA+MISSION+SETNO+FLEN,data=lset,FUN=sum)
lset<-lset[with(lset,order(lset$STRAT,lset$MISSION,lset$SETNO,lset$FLEN,lset$SLAT,lset$SLONG,lset$AREA)),]
lset<-melt(lset, id.vars = c("STRAT","SLAT","SLONG","AREA","MISSION","SETNO","FLEN"))

length_by_set <- na.zero(dcast(lset, STRAT + SLAT + SLONG + AREA + MISSION + SETNO  ~ FLEN))
remove<-c("STRAT","SLAT","SLONG","AREA","MISSION","SETNO")
length_by_set<-length_by_set[order(length_by_set$STRAT,length_by_set$SETNO),]
length_total<-merge(strata_tunits,length_by_set)

#separate the length and non-length-related data for the sets of the dataframe
length_total_pre  <-length_total[,c(1:7)]
length_total_strat_data       <-length_total[,c(1,8:ncol(length_total))]
#capture all of the column names of the data
colNames<-names(length_total_strat_data)

#count sets/strata
length_total_strat_data.cnt<-aggregate(list(COUNT=length_total_strat_data$STRAT), by=list(STRAT=length_total_strat_data$STRAT), FUN=length)

#add an additional rowname for the column totals 
length_rownames<-c(length_total_strat_data.cnt[,1],'Total')

#mean length
length_mean<-setNames(aggregate(list(length_total_strat_data[,2:ncol(length_total_strat_data)]), by=list(STRAT=length_total_strat_data$STRAT), FUN=mean), colNames)
  length_mean<-length_mean[,-1] #drop strata column
  length_mean<-cbind(length_mean,RowTotals=rowSums(length_mean)) #add rowsums
  length_mean<-rbind(length_mean,ColTotals=colSums(length_mean)) #add colsums
  length_mean<-cbind(STRAT=length_rownames,length_mean)          #add rowname

#mean length std error 
length_mean_se<-setNames(aggregate(list(length_total_strat_data[,2:ncol(length_total_strat_data)]), by=list(STRAT=length_total_strat_data$STRAT), FUN=st.err), colNames)
  length_mean_se<-length_mean_se[,-1] #drop strata column
  length_mean_se<-cbind(length_mean_se,RowTotals=rowSums(length_mean_se)) #add rowsums
  length_mean_se<-rbind(length_mean_se,ColTotals=colSums(length_mean_se)) #add colsums
  length_mean_se<-cbind(STRAT=length_rownames,length_mean_se)            #add rowname

#multiply all length data by tunits for "total length"
length_total_strat_data_tunits<-cbind(STRAT=length_total_pre[,1],length_total[,c(8:ncol(length_total))]*length_total_pre$TUNITS)

#total length
length_total<-setNames(aggregate(list(length_total_strat_data_tunits[,2:ncol(length_total_strat_data_tunits)]), by=list(STRAT=length_total_strat_data_tunits$STRAT), FUN=mean), colNames)
  length_total<-length_total[,-1] #drop strata column
  length_total<-cbind(length_total,RowTotals=rowSums(length_total)) #add rowsums
  length_total<-rbind(length_total,ColTotals=colSums(length_total)) #add colsums
  length_total<-cbind(STRAT=length_rownames,length_total)          #add rowname

#total length std error 
length_total_se<-setNames(aggregate(list(length_total_strat_data_tunits[,2:ncol(length_total_strat_data_tunits)]), by=list(STRAT=length_total_strat_data_tunits$STRAT), FUN=st.err), colNames)
  length_total_se<-length_total_se[,-1] #drop strata column
  length_total_se<-cbind(length_total_se,RowTotals=rowSums(length_total_se)) #add rowsums
  length_total_se<-rbind(length_total_se,ColTotals=colSums(length_total_se)) #add colsums
  length_total_se<-cbind(STRAT=length_rownames,length_total_se)          #add rowname

#######################################################################################
###                          AGE LENGTH KEY                                         ###
#######################################################################################

alk<-agelen[,c("AGE","FLEN","CAGE","SETNO")]
alk<-alk[!is.na(alk$AGE), ]
age_length_key<-na.zero(t(tapply(alk$SETNO, list(alk$AGE, alk$FLEN), function(x) {sum(x/x)})))

Length_Totals<-rowSums(age_length_key, dims = 1)
age_length_key<-cbind(age_length_key,Length_Totals)
Age_Totals<-colSums(age_length_key, dims = 1)

age_length_key<-rbind(age_length_key,Age_Totals)  #these results seem fine

#age length weight sheet
alw<-aggregate(FWT~AGE+FLEN,data=agelen,FUN=mean)
#necess for columns in asc order of age
alw<-alw[order(alw$AGE,alw$FLEN),]
alw$FWT = alw$FWT / 1000
age_length_weight = na.zero(reshape(alw,idvar='FLEN',timevar='AGE',direction='wide'))
#necess for rows in asc order of flen
age_length_weight<-age_length_weight[order(age_length_weight$FLEN),]
#does not show the avg wgts like stranal, but stranal seems to have incorrect totals

results<-list(strata_area, 
              age_length_key, 
              length_by_set, 
              length_mean, 
              length_mean_se, 
              length_total, 
              length_total_se, 
              age_length_weight, 
              nw_by_set,
              weights,
              numbers)
#r_results              == STRANAL results sheet
#---------------------------------------
#strata_area              == Strata Area      (OK)
#age_length_key           == Age Length Key   (OK)
#length_by_set            == Length by Set    (OK)
#length_mean              == Length Mean      (OK)
#length_mean_se           == Length Mean Standard Error (OK)
#length_total             == Length Total     (discrepencies > 75)
#length_total_se          == Length Total Standard Error (OK)
#age_length_weight        == Age Length Weight  (OK)
#nw_by_set                == Weight By Set (OK)
#                         == Numbers By Set (*)
#weights                  == Weight (BIOMASS) Total
#                         == Weight (BIOMASS) Total Std Err
#                         == Weight Mean
#                         == Weight Mean Std Err

#numbers(*)               == Numbers (ABUNDANCE) Total (*)
#                         == Numbers  (ABUNDANCE) Total Std Err (*)
#                         == Numbers Mean (*)
#                         == Numbers Mean Std Err (*)

# * not in original STRANAL
##########################################BEWARE!#########################################
#################################BEYOND HERE BE DRAGONS!##################################
##########################################BEWARE!#########################################


test<-agelen[,c("AGE","TOTWGT","TOTNO","FLEN")]
test$MEANWGT<-test$TOTWGT/test$TOTNO

test.mean<-aggregate(list(MEAN_WGT=test$TOTWGT), by=list(FLEN=test$FLEN), FUN=mean)
test.no<-aggregate(list(NO_WGT=test$TOTNO), by=list(FLEN=test$FLEN), FUN=length)
test.all<-merge(test.no,test.mean)
test.all$MMM<-test.all$MEAN_WGT/test.all$NO_WGT

agelen[agelen$STRAT==440,]
# 
# #lookup table for numbers of sets and area in nm per strata
# xxx1_query<-
#   paste("select strat,setno,sum(setno/setno) recs
#     from agelen
#     where year=",year," and to_number(strat)>=",strat.min," and to_number(strat)<=",strat.max,"
#     group by strat,setno
#     order by strat,setno",sep="")
# xxx1<-sqlQuery(channel,xxx1_query)
# 
# xxx2_query<-
#   "select x.strat,sum(x.recs/x.recs) sets, g.area areanm
#     from xxx1 x, groundfish.gsstratum g
#     where x.strat=g.strat
#     group by x.strat,g.area
#     order by x.strat,g.area"
# xxx2<-sqlQuery(channel,xxx2_query)
# 
# #stratified LF table
# #cage is the count at length adjusted for real tow distance and sample weight bumped to total weight
# #sage is stratified numbers at length
# #summing over sex and size_class (sex would be an option in the application)
# #adding number of sets per strata to each record for convenient math
# 
# ####
# ####bad join!
# ####
# LF_query<-
#   paste("select vessel,mission,sa.strat,tunits,setno,flen, sum(cage) cage, sum(nvl(cage,0)*tunits) sage, sets
#     from agelen sa, xxx2 x
#     where year=",year," and to_number(sa.strat)>=",strat.min," and to_number(sa.strat)<=",strat.max,"
#     and x.strat=sa.strat(+)
#     group by sa.strat,tunits,vessel,mission,setno,flen,sets
#     order by sa.strat,tunits,vessel,mission,setno,flen,sets",sep="")
# LF<-sqlQuery(channel,LF_query)
# 
# #weight by set sheet (not formatted for output)
# 
# ####
# ####bad join!
# ####
# setwgt_query<-
#   paste("select sa.strat,sa.slat,sa.slong,sa.mission,setno,sets,tunits,x.areanm,avg(raw_totwgt*1.75/dist) setwt
#  from agelen sa, xxx2 x
# where year=",year," and to_number(sa.strat)>=",strat.min," and to_number(sa.strat)<=",strat.max,"
# and x.strat=sa.strat(+)
# group by sa.strat,sa.mission,setno,sa.slat,sa.slong,sa.name,sets,tunits,x.areanm
# order by sa.strat,sa.mission,setno,sa.slat,sa.slong,sa.name,sets,tunits,x.areanm",sep="")
# setwgt<-sqlQuery(channel,setwgt_query)
