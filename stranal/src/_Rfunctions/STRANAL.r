############################
#User PARAMETERS
#
year=1974
type=c(1)
species.code=11
strat.list<-c(470:495)
############################

library(RODBC)
library(plyr)
library(reshape2)

channel<-odbcConnect(uid=oracle.personal.user,pw=oracle.personal.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
wd <- file.path(project.directory('stranal'),'data')
options(stringsAsFactors = FALSE)

#FOWLER 2014 
#original surveyscope table
#It must be updated for new surveys when loaded.
#At present we do not resolve NMFS surveys. 
#The APL version does or did (Y/N?).  (MMM - YES, it did)

#MMM Oct 2014
#Rather than a temporary oracle table, Ive put the data into a 
#stand-alone csv file.  This is still not ideal as it requires csv to 
#be updated - can we get this from the db? Note also that the original 
#values like 'SUMMER ', 'FALL ', etc included trailing spaces.  I 
#removed them, as they can only increase the suffering
#"MISSION" created from other fields for later convenience

survey_list<-read.csv(file=file.path(wd,"survey_list.csv"), head=T, sep=",")
survey_list$MISSION<-paste(survey_list$VESEL,survey_list$YEAR,sprintf("%03d",survey_list$CRUNO),sep="")

#FOWLER 2014 
#original surveyscope tables
#These provide hydrographic data at trawl depth associated with 
#surveys. It all gets wrapped up in the 'bottom' table, which is then 
#linked to the sampling data. Not required for STRANAL, but can serve 
#as valid set filters so included.
#MMM Oct 2014
#capture relevant missions for use in later filters

gstri <- subset(survey_list , (survey_list$SERIES %in% c('SUMMER','SPRING','FALL','4VWCOD','GEORGES')))
gstri<-gstri[which(gstri$YEAR==year),]


#MMM 2014 
#convert some information into a format that can be sent to SQL "IN" statments
these.type  <- paste(unlist(gsub("(.*)","'\\1'",type)),sep="",collapse=",")
these.strat  <- paste(unlist(gsub("(.*)","'\\1'",strat.list)),sep="",collapse=",")  #"'470','471','472','473'
these.missions<-paste(unlist(gsub("(.*)","'\\1'",gstri$MISSION)),sep="",collapse=",")

#MMM Oct 08, 2014 - MMM
#used calculation to convert strat areas into tunits, saves referencing external, static data
#may need to grant users access to GROUNDFISH.GS_STRATUM
#renamed "name" to "unitarea" to match Mark's app
tunits_query<-
  paste("SELECT strat, NAME UNITAREA, ROUND(nvl(area,0)/(1.75*(41/6080.2))) tunits 
          FROM 
          GROUNDFISH.GSSTRATUM 
          WHERE 
          strat IN (",these.strat,")", sep="")
tunits<-sqlQuery(channel,tunits_query)

#MMM Oct 2014
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

#MMM Oct 2014
#ctd and bot determination previously done with SQL joins
pre<-merge(gshyd_source,gstri, by='MISSION', all.x=F)
bot<-pre[which(pre$GEAR==1),]
ctd<-pre[which(pre$GEAR==2),]

bottom<-rbind(ctd, bot)
bottom<-bottom[!duplicated(bottom[c("MISSION","SETNO")]),]

#FOWLER 2014
#original surveyscope table 
#For SurveyScope this is part of a batch job in which custom tables 
#are created for every species and/or stock. The species is a 
#substitution variable in the batch job (spec=&&1), hard-coded here

#MMM Oct 2014
#not creating tables for every species/stock anymore
#extraction is limited by the users' selection of year and species at top
stock_all_raw_cat_query <- 
  paste("select mission,setno,size_class,totwgt,sampwgt,totno,calwt
          from 
          groundfish.gscat 
          where 
          spec=",species.code,"
          and mission IN (",these.missions,")", sep="")
stock_all_raw_cat<-sqlQuery(channel,stock_all_raw_cat_query)

#MMM Oct 2014 
#added mission and strat filters, used merge rather than SQL
#added a type limiter
gsinf_source_query<-
  paste("select i.mission, i.setno,sdate,time,strat,area,slat,slong,dmin,dmax,depth,dur,dist
          from 
          groundfish.gsinf i 
          where 
          i.MISSION IN (",these.missions,")
          AND strat IN (",these.strat,")
          AND type IN (",type,")", sep="")
gsinf_source<-sqlQuery(channel,gsinf_source_query)

stock_all_adj_cat<-merge(gsinf_source, stock_all_raw_cat, all.x=T)
stock_all_adj_cat<-merge(stock_all_adj_cat,survey_list, all.x=T)
stock_all_adj_cat$TOTWGT[is.null(stock_all_adj_cat$TOTWGT)] <- 0
stock_all_adj_cat$RAW_TOTWGT <-stock_all_adj_cat$TOTWGT
stock_all_adj_cat$TOTWGT <- (stock_all_adj_cat$TOTWGT*1.75)/stock_all_adj_cat$DIST
stock_all_adj_cat$RAW_TOTNO <-stock_all_adj_cat$TOTNO 
stock_all_adj_cat$TOTNO <- (stock_all_adj_cat$TOTNO*1.75)/stock_all_adj_cat$DIST

#FOWLER 2014
#STRANAL-specific tables from here on
#Provide data to build age:length keys and age composition. 
#Grab default bin width and whether age composition should be sexed or not from gsspec.
#If you want non-default stipulations for bins or sexing, replace the variable values below, e.g. lgrp 1,lfsexed 'Y'
#The bin width and sex stipulations would be options in an app (default or custom).
# drop table stock_all_raw_age;

#MMM Oct 2014
#converted to ANSI join
stock_all_raw_age_query<-
  paste("select mission,setno,size_class,fsex,age,fwt,
          decode(1,1,flen,2,.5+2*floor(flen/2),3,1+3*floor(flen/3),flen) flen,
          lgrp binwidth,lfsexed bysex, clen
          from 
          groundfish.gsdet g 
            LEFT OUTER JOIN groundfish.gsspec s ON (s.spec=g.spec)
          WHERE MISSION IN (",these.missions,")
          AND G.SPEC=",species.code,"
        AND age IS NOT NULL", sep="")
stock_all_raw_age<-sqlQuery(channel,stock_all_raw_age_query)


stock_all_adj_age<-merge(stock_all_adj_cat, stock_all_raw_age, by=c("MISSION", "SETNO","SIZE_CLASS"), all.x=T) #merge on particular fields by=c("MISSION", "SETNO","SIZE_CLASS"), 

stock_all_adj_age$FLEN<-floor(stock_all_adj_age$FLEN/stock_all_adj_age$BINWIDTH)*stock_all_adj_age$BINWIDTH
#instantiate cage
stock_all_adj_age$CAGE<-NA

  #if sampwgt > 0, use it to calculate cage
  stock_all_adj_age_1<-stock_all_adj_age[!is.na(stock_all_adj_age$SAMPWGT) & stock_all_adj_age$SAMPWGT>0,]
  if (nrow(stock_all_adj_age_1) >0){
  stock_all_adj_age_1$CAGE<-(stock_all_adj_age_1$RAW_TOTWGT/stock_all_adj_age_1$RAW_TOTWGT)*(1.75/stock_all_adj_age_1$DIST)*stock_all_adj_age_1$CLEN
  }
  #if sampwgt ==0, cage ==0
  stock_all_adj_age_0<-stock_all_adj_age[!is.na(stock_all_adj_age$SAMPWGT) & stock_all_adj_age$SAMPWGT==0,]
  if (nrow(stock_all_adj_age_0) >0){
  stock_all_adj_age_0$CAGE<-0
  }
  #if sampwgt is na/null, cage is na/null
  stock_all_adj_age_NA<-stock_all_adj_age[is.na(stock_all_adj_age$SAMPWGT),]
  if (nrow(stock_all_adj_age_NA)>0){
    stock_all_adj_age_NA$CAGE<-NA
  }
stock_all_adj_age<-rbind(stock_all_adj_age_1,stock_all_adj_age_0,stock_all_adj_age_NA)

#FOWLER 2014
#Adjust raw counts for subsampling and actual tow distance (relative to standard 1.75 nm tow).
#Aggregate counts at length according to the bin width.
#Only addresses standard Maritimes and Gulf region survey strata, not NMFS or Industry surveys.

stock_agelen<-merge(stock_all_adj_age,tunits, all.x=T)
#MMM 2014 
#now get strata results from the data frame
tunits_results<-unique(stock_agelen[,c("STRAT","TUNITS")])
tunits_results<-tunits_results[order(tunits_results$STRAT),]  #these results seem fine

#FOWLER 2014
  #The primary table to accomplish STRANAL.
  #Includes some spurious variables that some might want to consider as 
  #explanatory or filter variates (e.g. don't include sets below some
  # temperature). Making flen reflect the centre of its bin.

#MMM 2014
#Added some default values to replace NA values (this was done in thh SQL previously)
stock_agelen$DEPTH[is.na(stock_agelen$DEPTH)]<--99
stock_agelen$DMIN[is.na(stock_agelen$DMIN)]<--99
stock_agelen$DMAX[is.na(stock_agelen$DMAX)]<--99
stock_agelen$FLEN<-stock_agelen$FLEN+(stock_agelen$BINWIDTH*.5)-.5
stock_agelen$LOCTIME<-stock_agelen$TIME
stock_agelen$TIME<-NULL

stock_agelen<-merge(stock_agelen,bottom, all.x=T)

stock_agelen$SDEPTH[is.na(stock_agelen$SDEPTH)]<--99.9  
stock_agelen$TEMP[is.na(stock_agelen$TEMP)]<--99.99     
stock_agelen$SAL[is.na(stock_agelen$SAL)]<--99.999      

#FOWLER 2014
#From here on replicating the APL STRANAL Excel sheet outputs in the 
#same order as they appear in the Excel file. Skipped the first 
#(QUERY) sheet and only do the second (Strata Area) sheet in SQL 
#(without summing for totals). All the rest conform to the Excel file 
#that I was given for truthing, inclusive of formatting, making it 
#easy to compare with historical results.

alk<-stock_agelen[,c("AGE","FLEN","CAGE","SETNO")]
alk<-alk[!is.na(alk$AGE), ]
alk_results<-t(tapply(alk$SETNO  , list(alk$AGE, alk$FLEN), function(x) {sum(x/x)}))
  Length_Totals<-rowSums(alk_results, na.rm = T, dims = 1)
  alk_results<-cbind(alk_results,Length_Totals)
  Age_Totals<-colSums(alk_results, na.rm = T, dims = 1)

  propage<-alk_results/Age_Totals  #unverified


  alk_results<-rbind(alk_results,Age_Totals)  #these results seem fine

#length by set sheet (not formatted for output)
lset<-stock_agelen[,c("STRAT","SLAT","SLONG","UNITAREA","MISSION","SETNO","FLEN","CAGE")]
#need plyr for here
groupColumns = c("STRAT","SLAT","SLONG","UNITAREA","MISSION","SETNO","FLEN")
dataColumns = c("CAGE")
lset <- ddply(lset, groupColumns, function(x) colSums(x[dataColumns]))

lset<-lset[with(lset,order(lset$STRAT,lset$MISSION,lset$SETNO,lset$FLEN,lset$SLAT,lset$SLONG,lset$UNITAREA)),]
lset<-melt(lset, id.vars = c("STRAT","SLAT","SLONG","UNITAREA","MISSION","SETNO","FLEN"))
lset_results <- dcast(lset, STRAT + SLAT + SLONG + UNITAREA + MISSION + SETNO  ~ FLEN)
  remove<-c("STRAT","SLAT","SLONG","UNITAREA","MISSION","SETNO")
  Set_Totals<-rowSums(lset_results[-match(remove, names(lset_results))], na.rm = T)
  lset_results<-cbind(lset_results,Set_Totals)  
#length by set values do not match STRANAL - these reults are MISSING data for some sets


results<-list(tunits_results, alk_results,lset_results)
##########################################BEWARE!#########################################
#################################BEYOND HERE BE DRAGONS!##################################
##########################################BEWARE!#########################################

#age length weight sheet (not formatted for output)
alw_query<-
  paste("select age,flen,avg(fwt)/1000 meanwt from stock_agelen
    where year=",year," and age is not null
    and to_number(strat)>=",strat.min," and to_number(strat)<=",strat.max,"
    group by age,flen
    order by age,flen",sep="")
alw<-sqlQuery(channel,alw_query)

#lookup table for numbers of sets and area in nm per strata
xxx1_query<-
  paste("select strat,setno,sum(setno/setno) recs
    from stock_agelen
    where year=",year," and to_number(strat)>=",strat.min," and to_number(strat)<=",strat.max,"
    group by strat,setno
    order by strat,setno",sep="")
xxx1<-sqlQuery(channel,xxx1_query)

xxx2_query<-
  "select x.strat,sum(x.recs/x.recs) sets, g.area areanm
    from xxx1 x, groundfish.gsstratum g
    where x.strat=g.strat
    group by x.strat,g.area
    order by x.strat,g.area"
xxx2<-sqlQuery(channel,xxx2_query)

#stratified LF table
#cage is the count at length adjusted for real tow distance and sample weight bumped to total weight
#sage is stratified numbers at length
#summing over sex and size_class (sex would be an option in the application)
#adding number of sets per strata to each record for convenient math

####
####bad join!
####
LF_query<-
  paste("select vessel,mission,sa.strat,tunits,setno,flen, sum(cage) cage, sum(nvl(cage,0)*tunits) sage, sets
    from stock_agelen sa, xxx2 x
    where year=",year," and to_number(sa.strat)>=",strat.min," and to_number(sa.strat)<=",strat.max,"
    and x.strat=sa.strat(+)
    group by sa.strat,tunits,vessel,mission,setno,flen,sets
    order by sa.strat,tunits,vessel,mission,setno,flen,sets",sep="")
LF<-sqlQuery(channel,LF_query)

#weight by set sheet (not formatted for output)

####
####bad join!
####
setwgt_query<-
  paste("select sa.strat,sa.slat,sa.slong,sa.name unitarea,sa.mission,setno,sets,tunits,x.areanm,avg(raw_totwgt*1.75/dist) setwt
 from stock_agelen sa, xxx2 x
where year=",year," and to_number(sa.strat)>=",strat.min," and to_number(sa.strat)<=",strat.max,"
and x.strat=sa.strat(+)
group by sa.strat,sa.mission,setno,sa.slat,sa.slong,sa.name,sets,tunits,x.areanm
order by sa.strat,sa.mission,setno,sa.slat,sa.slong,sa.name,sets,tunits,x.areanm",sep="")
setwgt<-sqlQuery(channel,setwgt_query)
