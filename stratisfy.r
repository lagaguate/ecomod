stratisfy<-function(user=-1,
                  password=-1,
                  year=as.numeric(format(Sys.Date(), "%Y"))-1,
                  type=c(1), 
                  species.code=220,
                  strat.list=c(440:495),
                  wingspread = 41,
                  towdist =1.75,
                  by.sex=FALSE,
                  agency="DFO"
                  #, GUI=T
                  ){
  user=oracle.personal.user
  password=oracle.personal.password
################################################################################
###                                                                             
###       Title:                Stratisfy                                         
###                                                                             
###       Author:               Mike McMahon                                    
###                             Mark Fowler                                     
###                             Adam Cook                                       
###                                                                             
###       Creation Date:        Sept 9, 2015                                    
###       Modification Date:    Sept 9, 2015                                    
          stratisfy.ver = '2015.09.25'
###                                                                             
###       Description:          Replaces the standalone STRANAL application     
###                             (written in APL), as well as the numbers and    
###                             weights analytic from the old VDC               
###                                                                             
###       TODO:  US data needs to be incorporated                
###              sexed data needs to be incorporated             
################################################################################
writeLines("########\nrunning Stratisfy version ", stratisfy.ver,"\n########")
################################################################################
###                          PACKAGES                                           
if(!require(RODBC)) { 
    install.packages('RODBC',repos="http://cran.r-project.org")
  }
if(!require(reshape2)) { 
    install.packages('reshape2',repos="http://cran.r-project.org")
  }
if(!require(data.table)) { 
    install.packages('data.table',repos="http://cran.r-project.org")
  }

library(RODBC)
library(reshape2)       #reqd for melt,dcast
library(data.table)

################################################################################
###                          ENVIRONMENT                                        
options(stringsAsFactors = FALSE) 
# MMM - necessary?
options(local=TRUE)  
# MMM - I don't like scientific notation, this avoids it
options(scipen=999)  

################################################################################
###                          FUNCTIONS (general)                                         

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

#Format data for a SQL "IN ()" statement
SQL.in <- function(x) {
  paste(unlist(gsub("(.*)","'\\1'",x)),sep="",collapse=",")
}

#unless username and password set in profile, prompt user for 
#credentials several times
connect <- function(
                    user=ifelse(exists("oracle.personal.user"),
                                oracle.personal.user,-1),
                    password=ifelse(exists("oracle.personal.password"),
                                    oracle.personal.password,-1),
                    try=0
                    ) 
                    {   
                    if (user==-1 | password==-1){
                      user <- readline(prompt="Enter Username: ")
                      password <- readline(prompt="Enter Password: ")
                    }
                    channel<-suppressWarnings(
                            odbcConnect(uid=user,
                                         pw=password,
                                         dsn='PTRAN',
                                         case='nochange',
                                         rows_at_time=1))
                    if (channel==-1){
                      if (try >4){return(-999)}
                      writeLines(
"Bad username/password, or db not available.  Please try again."
                      )
                      user=-1
                      password=-1
                      try=try+1
                      connect(user, password, try)
                      }else{
                        return(channel)
                      }
                    }

###                         CONNECT TO DB    

channel<<-connect(user, password)
if (channel==-999|channel==-1){
  writeLines(
"Unable to connect to Oracle.
This may be due to an invalid username/password combination, or
the database may not currently be available.")
  return("Unable to connect to Oracle")
}else{
  writeLines(paste0(
"Successfully connected to Oracle."
  ))
}


################################################################################
#New improved, NMFS-friendly pick lists
gui<-function(){
#list available stratum area tables
get.stratum.tables<-function(agency.gui){
  if (agency.gui=="NMFS"){
    stratum.tables<-c("USNEFSC.DFO5ZJM", 
                      "USNEFSC.DFO5ZGHNO", 
                      "USNEFSC.NMFS5ZJM", 
                      "USNEFSC.NMFS5ZGHNO", 
                      "USNEFSC.NMFS5ZJMC", 
                      "USNEFSC.NMFS5ZJMU", 
                      "USNEFSC.NMFS5ZU")
  }
return(stratum.tables)
}


#list available survey types
get.type<-function(agency.gui) {
  if (agency.gui=="NMFS"){
   the.type="111" 
  }else{
  the.type <- sqlQuery(channel, 
                       paste("select XTYPEDESC, XTYPE 
                             from groundfish.GSXTYPE
                             ORDER BY XTYPE",sep=""))
  the.type<-paste( the.type[,1], " (", the.type[,2],")",sep="") 
  }
  return(the.type)
}
#list available years
#NMFS -- 
# --10=  NMFS NEFSC BOTTOM TRAWL SURVEY
# --11 = '36 YANKEE TRAWL'
# --41 = Modified 41 Yankee Trawl (Accepted Code)
# --45 = Modified 41 Yankee Trawl (Erroneously Coded on Several Cruises)
get.year<-function(agency.gui) {
  if (agency.gui=="NMFS"){
    year.query = "SELECT DISTINCT Year
                    FROM USNEFSC.USS_MSTR_CRUISE
                    WHERE PURPOSE_CODE='10' AND 
                    SVGEAR In ('11','41','45')
                    ORDER BY Year DESC"
  }else{
    year.query = "select distinct YEAR 
                             from groundfish.gsmissions
                             ORDER BY YEAR DESC"    
  }
  the.year = sqlQuery(channel, year.query)
  the.year<-paste( the.year[,1],sep=",") 
  return(the.year)
}
#list available missions
#not sure if I want to offer filter by season (would need additional dialog box)
# removed gear restriction from NMFS
#--AND SVGEAR In ('11','41','45')
get.missions<-function(agency.gui,year.gui){
  if (agency.gui=="NMFS"){
    mission.query=paste("SELECT CRUISE6 AS MISSION, SVVESSEL AS VESEL, 
                          CRUISE AS CRUNO, YEAR, SEASON
                          FROM USNEFSC.USS_MSTR_CRUISE
                          WHERE PURPOSE_CODE='10' 
                          AND SEASON IN ('SPRING','SUMMER','FALL','WINTER')
                          AND YEAR = ",year.gui, sep="")
  }else{
    mission.query=paste("select MISSION, VESEL, CRUNO, YEAR, SEASON
                        from groundfish.gsmissions 
                         WHERE SEASON IN 
                         ('SUMMER','SPRING','FALL','4VWCOD','GEORGES')
                         AND YEAR = ",year.gui, sep="") 
  }
  return(sqlQuery(channel, mission.query))
}

#list available strata (filtered by selected year and type)
get.strata<-function(agency.gui, year.gui, missions.gui, stratum.table.gui,type.gui) {
 if (agency.gui =="NMFS"){
  strata.query=paste0("SELECT DISTINCT STRAT 
                        FROM ",stratum.table.gui," 
                        ORDER BY STRAT")
  the.strata <- sqlQuery(channel,strata.query)
  #have to do nmfs strat differently, since they may need leading zeros
  the.strata<-sprintf("%05d", the.strata$STRAT)
  the.strata<-paste(the.strata,sep=",") 
  }else{
    dfo.strata.query.year.tweak=paste0("AND to_char(sdate,'yyyy') = ",year.gui)
    dfo.strata.query.type.tweak=paste0("AND type IN (",type.gui,")")
    dfo.strata.query.mission.tweak=paste0("AND MISSION IN (",missions.gui,")")
    strata.query<-paste0("select DISTINCT STRAT 
                                  from groundfish.gsinf
                                  WHERE STRAT IS NOT NULL
                                  ",dfo.strata.query.year.tweak,"
                                  ",dfo.strata.query.type.tweak,"
                                  ",dfo.strata.query.mission.tweak,"
                                  ORDER BY STRAT")
    the.strata <- sqlQuery(channel,strata.query)
    the.strata<-paste(the.strata[,1],sep=",") 
  }
  return(the.strata)
}   

#list available areas (given selected year, type and strata)
get.area<-function(agency.gui,year.gui,type.gui,strata.gui ) {
  if (agency.gui =="NMFS"){
    nmfs.area.query.year.tweak=paste0("AND GMT_YEAR = ", year.gui)
    nmfs.area.query.type.tweak=paste0("AND SHG <=",type.gui)
    #stratum needs have leading zero removed
    nmfs.area.query.strata.tweak=paste0("AND ltrim(STRATUM, '0') IN (", strata.gui,")")
    
   area.query=paste0("SELECT distinct AREA 
                from USS_STATION
                WHERE 1=1  
                ",nmfs.area.query.year.tweak,"
                ",nmfs.area.query.type.tweak,"
                ",nmfs.area.query.strata.tweak)
  }else{
    dfo.area.query.year.tweak=paste0("AND to_char(sdate,'yyyy') = ",year.gui)
    dfo.area.query.type.tweak=paste0("AND type IN (",type.gui,")")
    dfo.area.query.strata.tweak=paste0("AND strat IN (",strata.gui,")")
    area.query <- paste0("select distinct AREA
                                from groundfish.gsinf
                                WHERE 1=1
                                ",dfo.area.query.year.tweak,"
                                ",dfo.area.query.type.tweak,"
                                ",dfo.area.query.strata.tweak,"
                                ORDER BY AREA")
  }
  the.area <- sqlQuery(channel, area.query)
  the.area<-paste( the.area[,1],sep=",") 
  return(the.area)
}



################################################################################
#prompt user for agency
agency.gui<-select.list(c("DFO","NMFS"), 
                        multiple=F, graphics=T, 
                        title='Please choose an agency:')

by.sex.gui<-select.list(c("Unsexed Analysis","Sexed Analysis"),
                        multiple=F, graphics=T, 
                        title='Analysis by Sex?')
if (by.sex.gui=="Sexed Analysis") by.sex.gui=T else by.sex.gui=F

#This section holds agency-specific questions/inputs
if (agency.gui=='DFO'){
     stratum.table.gui=""
    type.gui<-select.list(get.type(agency.gui), 
                              multiple=F, graphics=T, 
                              title='Please select the type of trawl:')
    #grab the (numeric) type from the selection
    type.gui<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', type.gui) )
    wingspread.gui = as.numeric(select.list(c("41","34"),
                                        preselect=c("41"),
                                        multiple=F, graphics=T, 
                                        title='wingspread (ft)'))
}else{
  stratum.table.gui<-select.list(get.stratum.tables(agency.gui), 
                             multiple=F, graphics=T, 
                             title='Please choose a stratum table:')
  type.gui=136
  wingspread.gui = as.numeric(select.list(c("34","36","41"),
                                      preselect=c("34"),
                                      multiple=F, graphics=T, 
                                      title='wingspread (ft)'))
}
#get df of species info, including LGRP
#list available species
the.species<-function(agency.gui, by.sex.gui) {
  #MMM - this is not ideal - I investigated the uss_catch table to find which tables
  #had species with more than 1 catchsex, and manually paste them into
  if (agency.gui=="NMFS"){
    if (isTRUE(by.sex.gui)){
      species.query.tweak<-"AND SPEC IN ('015','022','026','075','108')"
    } else{
      species.query.tweak<-""
    }
    
    species.query=paste("SELECT US.SPEC, INITCAP(US.CNAME) CNAME, US.LGRP, US.LFSEXED 
                        FROM USNEFSC.USSPEC US
                        WHERE  
                        US.SPEC <> 9999 ",species.query.tweak,"
                        ORDER BY initcap(US.CNAME)",sep="")
  }else{
    if (isTRUE(by.sex.gui)){
      species.query.tweak<-"AND LFSEXED = 'Y' "
    } else{
      species.query.tweak<-""
    }
    species.query=paste("SELECT DISTINCT(SPEC), initcap(CNAME) CNAME, 
                        LGRP, LFSEXED 
                        FROM GROUNDFISH.GSSPEC 
                        WHERE SPEC <> 9999 "
                        , species.query.tweak, " 
                        ORDER BY initcap(CNAME)",sep="")
  }
  the.species = sqlQuery(channel, species.query)
  #the.species = paste( the.species[,2], " (", the.species[,1],")",sep="")
  return(the.species)
  }

the.species<-the.species(agency.gui, by.sex.gui)
#prompt user for species (filter selection if sexed species are required) 
species.gui<-select.list(paste( the.species$CNAME, " (", the.species$SPEC,")",sep=""),
                         multiple=F, graphics=T, 
                         title=ifelse(isTRUE(by.sex.gui),
                                        "Choose a (sexed) species:",
                                        "Choose a species"))
#got species selection - extract code and name
species.code.gui<-as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', species.gui)) 
species.name.gui<-gsub("\\s?\\(.*?\\)", "", species.gui)
#use the species code to grab lgrp and lfsexed
species.lgrp.gui<-the.species[which(the.species$SPEC == species.code.gui), ][3]
species.lfsexed.gui<-the.species[which(the.species$SPEC == species.code.gui), ][4]

#prompt user for year
year.gui<-select.list(paste(get.year(agency.gui),sep=","),
                      multiple=F, graphics=T, 
                      title='Choose a year:')


mission.df<-get.missions(agency.gui,year.gui)
missions.gui<-SQL.in(unique(mission.df$MISSION))

strata.gui<-select.list(paste(get.strata(agency.gui,year.gui,missions.gui,stratum.table.gui,type.gui),sep=","),
                    multiple=T, graphics=T, 
                    title='Choose the strata:')
##if being sent to NMFS, stratum needs leading zer
strata.gui = SQL.in(strata.gui)

#prompt user for area (filtered by those matching existing selections 
#for year, type and strata) 
area.gui<-select.list(paste(get.area(agency.gui,year.gui,type.gui,strata.gui),sep=","), 
                      multiple=T, graphics=T, 
                      preselect=c(get.area(agency.gui,year.gui,type.gui,strata.gui)),
                      title=paste0("Choose the area(s):"))
area.gui<-SQL.in(area.gui)
#prompt user for tow distance (default and only current option is 1.75)
#would be nice to allow user selection consistent with other pop-ups (not scan)  
towdist.gui = as.numeric(select.list(c("1.75"),
                                 preselect=c("1.75"),
                                 multiple=F, graphics=T, 
                                 title='Tow Distance (NM)'))
GUI.results<-list(wingspread.gui, 
                  towdist.gui, 
                  by.sex.gui, 
                  year.gui,
                  species.code.gui, 
                  species.name.gui, 
                  type.gui,
                  strata.gui, 
                  area.gui,
                  missions.gui,
                  mission.df,
                  agency.gui,
                  species.lgrp.gui,
                  species.lfsexed.gui)
return(GUI.results)
}
  GUI.results<-gui()
  wingspread = GUI.results[[1]]
  towdist = GUI.results[[2]]
  by.sex = GUI.results[[3]]
  year = GUI.results[[4]]
  species.code = GUI.results[[5]]
  species.name = GUI.results[[6]]
  these.type = GUI.results[[7]]
  these.strat = GUI.results[[8]]
  these.areas = GUI.results[[9]] 
  these.missions = GUI.results[[10]]
  mission.df = GUI.results[[11]] 
  agency.gui = GUI.results[[12]] 
  species.lgrp.gui = GUI.results[[13]] 
  species.lfsexed.gui = GUI.results[[14]] 


################################################################################
###                          DATABASE EXTRACTIONS                               

#limit extraction by the users' selections
if (agency.gui=='DFO'){
raw_gscat_query <- 
  paste("select mission,setno,size_class,totwgt,sampwgt,totno,calwt
          from 
          groundfish.gscat 
          where 
          spec=",species.code,"
          and mission IN (",these.missions,")", sep="")

}else{
  #nmfs does not have a size class  

}
raw_gscat<-sqlQuery(channel,raw_gscat_query)


# catchsex is inconsistent in old STRANAL - testing using "unsexed" found that 
# sometimes catchsex 1 OR 2 was chosen - not a combination of the 3 (0, 1 and 2)
# problem was found in cruise6=199502, spp=15
#AND catchsex in ('0','1','2')
raw.gscat.query <- 
  paste("select cruise6 mission,to_number(station) setno, sum(expcatchwt) totwgt,sum(expcatchnum) totno
          from 
          usnefsc.uss_catch 
          WHERE 
          to_number(svspp)=",species.code,"
          and cruise6 IN (",these.missions,")
          AND STRATUM   IN (",these.strat,")
          group by 
          cruise6, to_number(station)", sep="")
raw.gscat.data<-sqlQuery(channel,raw.gscat.query )

#distance was assumed to be 1.75, but appears to be dopdistb
raw.gsinf.query<-
  paste("SELECT CRUISE6 mission,to_number(station) setno, begin_est_towdate sdate, est_time time, STRATUM strat, 
    area,  BEGLAT slat, BEGLON slon, mindepth dmin, maxdepth dmax, avgdepth depth, towdur dur, dopdistb dist 
        FROM USNEFSC.USS_STATION 
        WHERE CRUISE6 in (",these.missions,")
        AND STRATUM in (",these.strat,")"
        , sep="")
raw.gsinf.data<-sqlQuery(channel,raw.gsinf.query )
cat.inf<-merge(raw.gscat.data,raw.gsinf.data, all.y=T)
#need to determine how to limit missions similar to STRANAL
#in the meantime, for testing, using this...
cat.inf<-cat.inf[cat.inf$MISSION=='199502',]



raw.gsdet.query<-
  paste("
select cruise6 mission, to_number(station) setno, age, length, avg(indwt) fwt,
decode(",species.lgrp.gui,",1,length,2,.5+2*floor(length/2),3,1+3*floor(length/3),length) flen,
",species.lgrp.gui," binwidth,'N' bysex
from usnefsc.uss_detail
where to_number(svspp)=",species.code,"
AND CRUISE6 in (",these.missions,")
AND STRATUM in (",these.strat,")
group by cruise6,station,age,length", sep="")
raw.gsdet.data<-sqlQuery(channel,raw.gsdet.query )
#need to determine how to limit missions similar to STRANAL
#in the meantime, for testing, using this...
raw.gsdet.data<-raw.gsdet.data[raw.gsdet.data$MISSION=='199502',]



raw.gsdetLF.query<-
  paste("
select cruise6 mission, to_number(station) setno,length, sum(expnumlen) clen
from usnefsc.uss_lengths
where to_number(svspp)=",species.code,"
AND CRUISE6 in (",these.missions,")
AND STRATUM in (",these.strat,")
group by cruise6,station,length", sep="")
raw.gsdetLF.data<-sqlQuery(channel,raw.gsdetLF.query )
#need to determine how to limit missions similar to STRANAL
#in the meantime, for testing, using this...
raw.gsdetLF.data<-raw.gsdetLF.data[raw.gsdetLF.data$MISSION=='199502',]


stock_all_raw_age<-merge(raw.gsdetLF.data,raw.gsdet.data, all = TRUE)












#added mission, strat and type filters
if (agency.gui=='DFO'){
raw_gsinf_query<-
  #no area filter??
  paste("select i.mission, i.setno,sdate,time,strat,
         area,slat,slong,dmin,dmax,depth,dur,dist
          from 
          groundfish.gsinf i 
          where 
          i.MISSION IN (",these.missions,")
          AND strat IN (",these.strat,")
          AND type IN (",these.type,")", sep="")
}else{
}
raw_gsinf<-sqlQuery(channel,raw_gsinf_query)

if (agency.gui=='DFO'){
raw_gsdet_query<-
  paste("select mission,setno,size_class,fsex,age,fwt,
          decode(",species.lgrp.gui,",1,flen,2,.5+2*floor(flen/2),3,1+3*floor(flen/3),flen) flen,
          lgrp binwidth,lfsexed bysex, clen
          from 
          groundfish.gsdet g 
            LEFT OUTER JOIN groundfish.gsspec s ON (s.spec=g.spec)
          WHERE MISSION IN (",these.missions,")
          AND G.SPEC=",species.code,"
        ", sep="")
  raw_gsdet<-sqlQuery(channel,raw_gsdet_query)
}else{
  nmfs_detail_query<-paste("select cruise6 mission, station setno,age,
                        length, avg(indwt) fwt
                        from usnefsc.uss_detail
                        where to_number(svspp)=",species.code,"
                        and sex in ('0','1','2')
                        group by cruise6,station,age,length", sep="")
  nmfs_detail<-sqlQuery(channel,nmfs_detail_query)
  nmfs_lf_query<-paste("select cruise6 mission, station setno,length, 
                      sum(expnumlen) clen
                      from usnefsc.uss_lengths
                      where to_number(svspp)=",species.code,"
                      and catchsex in ('0','1','2')
                      group by cruise6,station,length",sep="")
  nmfs_lf<-sqlQuery(channel,nmfs_lf_query)

  stock_all_raw_age<-merge(nmfs_detail,nmfs_lf)
#   stock_all_raw_age$FLEN<-1,
#   if(x==1){
#     FLEN<-l.length
#   }else if(x==2){
#     FLEN<-.5+2*floor(l.length/2)
#   }else if(x==3){
#     FLEN<-1+3*floor(l.length/3)
#   }else{
#     FLEN<-l.length
#   }

  
}

#calculate strat areas into tunits (access to GROUNDFISH.GS_STRATUM reqd?)
#US nautical mile is 6080.2ft
strata_area_query<-
  paste("SELECT strat, area SQNM, 
         nvl(area,0)/(",towdist,"*(",wingspread,"/6080.2)) tunits 
          FROM 
          GROUNDFISH.GSSTRATUM 
          WHERE 
          strat IN (",these.strat,")", sep="")
strata_area<-sqlQuery(channel,strata_area_query)
strata_area<-strata_area[order(strata_area$STRAT),c("STRAT","TUNITS","SQNM")]

#strata_tunits<-tunits[order(tunits$STRAT),c("STRAT","TUNITS")]
################################################################################
###                          STRATA AREA1                                       
################################################################################
    #hydrographic data at trawl depth - Not required for STRANAL
    #ctd made from joining dataframe mission.df to extraction from gshyd
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
    pre<-merge(gshyd_source,mission.df, by='MISSION', all.x=F)
    bot<-pre[which(pre$GEAR==1),]
    ctd<-pre[which(pre$GEAR==2),]
    
    bottom<-rbind(ctd, bot)
    bottom<-bottom[!duplicated(bottom[c("MISSION","SETNO")]),]


#done with our database connection - close it
odbcClose(channel)


################################################################################
###                          NUMBERS AND WEIGHTS                                
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
################################################################################
nw_by_set_pre<-merge(raw_gsinf, raw_gscat, all.x=T)
nw_by_set_pre<-merge(nw_by_set_pre,mission.df, all.x=T)
nw_by_set_pre[which(is.na(nw_by_set_pre$TOTWGT)),
              c('SAMPWGT','TOTWGT','TOTNO','CALWT')] <- 0
nw_by_set_pre$SIZE_CLASS[which(is.na(nw_by_set_pre$SIZE_CLASS))] <- 1
nw_by_set_pre$RAW_TOTWGT <-nw_by_set_pre$TOTWGT
nw_by_set_pre$TOTWGT <- (nw_by_set_pre$TOTWGT*towdist)/nw_by_set_pre$DIST
nw_by_set_pre$RAW_TOTNO <-nw_by_set_pre$TOTNO 
nw_by_set_pre$TOTNO <- (nw_by_set_pre$TOTNO*towdist)/nw_by_set_pre$DIST
nw_by_set_pre<-nw_by_set_pre[order(nw_by_set_pre$STRAT,nw_by_set_pre$SETNO),]

set_info<-nw_by_set_pre[order(nw_by_set_pre$STRAT,nw_by_set_pre$SETNO),
                        c("MISSION","SEASON","STRAT","SETNO","SDATE","AREA",
                          "SLAT","SLONG","DMIN","DMAX","DEPTH","DUR","DIST" )]
nw_by_set<-nw_by_set_pre[order(nw_by_set_pre$STRAT,nw_by_set_pre$SETNO),
            c("STRAT","MISSION","SETNO","TOTNO","TOTWGT")]

nw_by_set_pre2<-nw_by_set_pre[order(nw_by_set_pre$STRAT,nw_by_set_pre$SETNO),
                c("STRAT","SLAT","SLONG","AREA","SETNO","TOTWGT","TOTNO")]

nw_by_set_pre2<-merge(nw_by_set_pre2,subset(strata_area,select=-c(SQNM)))
#unexpected sqnm

nw_by_set_pre2$BIOMASS<-nw_by_set_pre2$TOTWGT*nw_by_set_pre2$TUNITS
nw_by_set_pre2$ABUND<-nw_by_set_pre2$TOTNO*nw_by_set_pre2$TUNITS

nw_by_strata.cnt<-aggregate(list(COUNT=nw_by_set_pre2$STRAT), 
                            by=list(STRAT=nw_by_set_pre2$STRAT), 
                            FUN=length)
nw_by_strata.sum<-aggregate(list(TOT_WGT=nw_by_set_pre2$TOTWGT,
                                 TOT_NO=nw_by_set_pre2$TOTNO), 
                            by=list(STRAT=nw_by_set_pre2$STRAT), 
                            FUN=sum)
nw_by_strata.mean<-aggregate(list(MEAN_WGT=nw_by_set_pre2$TOTWGT,
                                  MEAN_NO=nw_by_set_pre2$TOTNO,
                                  BIOMASS=nw_by_set_pre2$BIOMASS,
                                  ABUND=nw_by_set_pre2$ABUND), 
                            by=list(STRAT=nw_by_set_pre2$STRAT), 
                            FUN=mean)
nw_by_strata.sterr<-aggregate(list(ST_ERR_WGT=nw_by_set_pre2$TOTWGT,
                                   ST_ERR_NO=nw_by_set_pre2$TOTNO,
                                   ST_ERR_BIOMASS=nw_by_set_pre2$BIOMASS,
                                   ST_ERR_ABUND=nw_by_set_pre2$ABUND), 
                                by=list(STRAT=nw_by_set_pre2$STRAT), 
                                FUN=st.err)

nw<-merge(nw_by_strata.cnt,nw_by_strata.sum,by="STRAT")
nw<-merge(nw, nw_by_strata.mean,by="STRAT")
nw<-merge(nw, nw_by_strata.sterr,by="STRAT")

numbers<-na.zero(nw[,c("STRAT","COUNT","TOT_NO","MEAN_NO","ABUND",
                       "ST_ERR_NO","ST_ERR_ABUND")])
weights<-na.zero(nw[,c("STRAT","COUNT","TOT_WGT","MEAN_WGT","BIOMASS",
                       "ST_ERR_WGT","ST_ERR_BIOMASS")])

catchsets<-nw_by_set_pre2
catchsetskeeps <- c("STRAT","TOTWGT","TOTNO")
catchsets<-catchsets[catchsetskeeps]
catchsets$somecatch[catchsets$TOTWGT!=0 | catchsets$TOTNO!=0]<-1
catchsets$somecatch[is.na(catchsets$somecatch)]<-0
catchsets<-merge(strata_area,catchsets,by="STRAT",all.y=T)  
catchsets<-merge(catchsets,nw_by_strata.cnt,by="STRAT",all.x=T)
catchsets$Area<-catchsets$SQNM*catchsets$somecatch
catchsets.AreaProp<-aggregate(list(AreaProp=catchsets$somecatch), 
                              by=list(STRAT=catchsets$STRAT), 
                              FUN=mean)
catchsets.AreaPropStErr<-aggregate(list(AreaPropStErr=catchsets$somecatch), 
                                    by=list(STRAT=catchsets$STRAT), 
                                    FUN=st.err)
catchsets.AreaTot<-aggregate(list(AreaTot=catchsets$Area), 
                                  by=list(STRAT=catchsets$STRAT), 
                                  FUN=mean)
catchsets.AreaTotStErr<-aggregate(list(AreaTotStErr=catchsets$Area), 
                                  by=list(STRAT=catchsets$STRAT), 
                                  FUN=st.err)


################################################################################
###                          STRATA AREAS                                       
###    Strat
###    Tunits
###    SQNM
###    AreaProp
###    AreaPropStErr
###    AreaTot
###    AreaTotStErr

strata.areas<-merge(strata_area,catchsets.AreaProp,by="STRAT")
strata.areas<-merge(strata.areas,catchsets.AreaPropStErr,by="STRAT")
strata.areas<-merge(strata.areas,catchsets.AreaTot,by="STRAT")
strata.areas<-na.zero(merge(strata.areas,catchsets.AreaTotStErr,by="STRAT"))
################################################################################
###                          SET UP AGELEN                                      
agelen<-merge(nw_by_set_pre, raw_gsdet, by=c("MISSION", "SETNO","SIZE_CLASS"), all.x=T)
agelen<-merge(agelen,strata_area, all.x=T)
agelen$FLEN<-floor(agelen$FLEN/agelen$BINWIDTH)*agelen$BINWIDTH
agelen$CAGE<-NA

#if sampwgt is 0 or NA and totwgt is not null or 0 
#then replace sample weigt with total weight 
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
    agelen$CAGE[st1]<-agelen$RAW_TOTWGT[st1]/agelen$SAMPWGT[st1]*
                      (towdist/agelen$DIST[st1])*agelen$CLEN[st1]
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
################################################################################
###                          LENGTH ANALYTICS                                   
###    LENGTH BY SET
###    LENGTH MEAN
###    LENGTH MEAN STANDARD ERROR 
###    LENGTH TOTAL
###    LENGTH TOTAL STANDARD ERROR
all.c<-c("STRAT", "MISSION", "SETNO")
if (isTRUE(by.sex)){
    order.c<-c("STRAT","MISSION","SETNO","FSEX")
    sex.c<-c("STRAT","SLAT","SLONG","AREA", "MISSION","SETNO","FLEN","CAGE","FSEX") 
    fields<-c("STRAT","SLAT","SLONG","AREA", "MISSION","SETNO","FLEN","FSEX")
  }else{
    order.c<-c("STRAT","MISSION","SETNO")
    sex.c<-c("STRAT","SLAT","SLONG","AREA", "MISSION","SETNO","FLEN","CAGE")
    fields<-c("STRAT","SLAT","SLONG","AREA", "MISSION","SETNO","FLEN")
}
lset<-na.zero(agelen[,sex.c])
li = which(lset$CAGE==0)
lset$FLEN[li] = unique(lset$FLEN)[1]
lset<-aggregate(lset$CAGE,
                lset[fields],
                FUN=sum)
lset<-lset[with(lset,order(get(order.c))),]
lset<-melt(lset,id.vars=fields)

#not very slick - would like to be able to dynamically send columns to dcast
if (isTRUE(by.sex)){  
  lset$FSEX[lset$FSEX==0]<-'UNK'
  lset$FSEX[lset$FSEX==1]<-'MALE'
  lset$FSEX[lset$FSEX==2]<-'FEMALE'
  length_by_set <- na.zero(dcast(lset, STRAT + MISSION + SETNO ~ FSEX +FLEN  ))
}else{
  #following gives - Aggregation function missing: defaulting to length
  length_by_set <- na.zero(dcast(lset, STRAT + MISSION + SETNO ~ FLEN ))
}
length_by_set<-length_by_set[order(length_by_set$STRAT,length_by_set$SETNO),]
length_total<-merge(subset(strata_area,select=-c(SQNM)),length_by_set)
#unexpected sqnm
#add row_tots to length_by_set
length_by_set$TOTAL<-rowSums(length_by_set[,4:length(length_by_set)])

#separate the length and non-length-related data for the sets of the dataframe
length_total_pre  <-length_total[,c("STRAT","TUNITS")]
length_total_strat_data       <-length_total[,c(1,5:ncol(length_total))]
#capture all of the column names of the data
colNames<-names(length_total_strat_data)
#count sets/strata
length_total_strat_data.cnt<-aggregate(list(
                                  COUNT=length_total_strat_data$STRAT), 
                                  by=list(STRAT=length_total_strat_data$STRAT), 
                                  FUN=length)

#add an additional rowname for the column totals 
length_rownames<-c(length_total_strat_data.cnt[,1],'Total')
length_rownames_noTotal<-c(length_total_strat_data.cnt[,1])
#mean length
length_mean<-setNames(aggregate(
                list(length_total_strat_data[,2:ncol(length_total_strat_data)]), 
                by=list(STRAT=length_total_strat_data$STRAT), 
                FUN=mean), colNames)
 #drop strata column
  length_mean<-length_mean[,-1]
  #add rowsums
  length_mean<-cbind(length_mean,RowTotals=rowSums(length_mean)) 
  #add rowname
  length_mean<-cbind(STRAT=length_rownames_noTotal,length_mean)        

#mean length std error 
length_mean_se<-setNames(aggregate(
            list(length_total_strat_data[,c(2:ncol(length_total_strat_data))]), 
            by=list(STRAT=length_total_strat_data$STRAT), 
            FUN=st.err), colNames)
  #drop strata column
  length_mean_se<-length_mean_se[,-1] 
  #add rowname
  length_mean_se<-na.zero(cbind(STRAT=length_rownames_noTotal,length_mean_se))            

#multiply all length data by tunits for "total length"
length_total_strat_data_tunits<-cbind(STRAT=length_total_pre$STRAT,
                                    length_total[,c(5:ncol(length_total))]*
                                    length_total_pre$TUNITS)

#total length
length_total<-setNames(aggregate(list(
      length_total_strat_data_tunits[,2:ncol(length_total_strat_data_tunits)]), 
      by=list(STRAT=length_total_strat_data_tunits$STRAT), 
      FUN=mean), colNames)
  #drop strata column
  length_total<-length_total[,-1] 
  #add rowsums
  length_total<-cbind(length_total,RowTotals=rowSums(length_total)) 
  #separated this because I use it in age calculations
  ColTotalsLength=colSums(length_total) 
  #add colsums
  length_total<-rbind(length_total,ColTotals=ColTotalsLength) 
  #add rowname
  length_total<-na.zero(cbind(STRAT=length_rownames,length_total))          

#total length std error 
length_total_se<-setNames(aggregate(list(
        length_total_strat_data_tunits[,2:ncol(length_total_strat_data_tunits)]), 
        by=list(STRAT=length_total_strat_data_tunits$STRAT), 
        FUN=st.err), colNames)
  #drop strata column
  length_total_se<-length_total_se[,-1] 
  #add rowname
  length_total_se<-na.zero(cbind(STRAT=length_rownames_noTotal,length_total_se))          


################################################################################
###                          AGE LENGTH KEY                                     

alk<-agelen[,c("AGE","FLEN","CAGE","SETNO")]
alk<-alk[!is.na(alk$AGE), ]
if (nrow(alk)<1){     #only try age calculations if we have ages
  age_by_set<-"can't do age_by_set - no ages in data"
  ages<-"can't do ages - no ages in data"
  age_length_key_totals<-"can't do age_length_key_totals - no ages in data"
  age_table<-"can't do age_table - no ages in data"
  age_length_weight<-"can't do age_length_weight - no ages in data"
}else{
age_length_key_naked<-na.zero(t(
                      tapply(alk$SETNO, 
                        list(alk$AGE, alk$FLEN), 
                        function(x) {sum(x/x)})))
#all possiblem lengths
allFlenDF<-data.frame(allFlen) 
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
age_length_weight = na.zero(reshape(
                            alw,idvar='FLEN',timevar='AGE',direction='wide'))
#if want to add all ages, should do it here!
rownames(age_length_weight)<-age_length_weight[,1]
age_length_weight<-merge(allFlenDF,age_length_weight,by="row.names", all=TRUE)
age_length_weight<-na.zero(age_length_weight[order(age_length_weight$allFlen),])
age_length_weight$FLEN<-NULL
age_length_weight$Row.names<-NULL
################################################################################
###                          AGE CALCULATIONS                                   
lengths<-ColTotalsLength[1:length(ColTotalsLength)-1] 
ages_prop<-prop.table(as.matrix(age_length_key),1) 
ages_prop<-ifelse(is.nan(ages_prop),0,ages_prop)
ages_prop<-as.data.frame(ages_prop)
theseages<-c(names(ages_prop))
age_table<-ages_prop*lengths
ages_prop$FLEN<-as.numeric(rownames(ages_prop))
ageset<-lset[,c("STRAT","MISSION","SETNO","FLEN","value")]
colnames(ageset)[which(names(ageset) == "value")] <- "CAGE"
ageset<-ageset[order(ageset$STRAT,ageset$MISSION, ageset$SETNO),]

ages_pre<-merge(ageset,ages_prop, by="FLEN")
ages_pre<-as.data.table(ages_pre)
ages_pre[, (theseages) := lapply(.SD, 
                          function(x) x * ages_pre[['CAGE']] ), 
                          .SDcols = theseages]
age_by_set<-aggregate(.~STRAT + MISSION + SETNO, data=ages_pre, sum)
age_by_set<-age_by_set[order(age_by_set$STRAT,age_by_set$SETNO),]
age_by_set$FLEN<-NULL 
age_by_set$CAGE<-NULL

age_mean<-age_by_set
age_mean$SETNO<-NULL
age_mean$MISSION<-NULL
age_mean<-aggregate(.~STRAT, data=age_mean, mean)
setnames(age_mean,
        old=names(age_mean[,2:ncol(age_mean)]), 
        new=c(paste0("age_",theseages,"_mean")))

age_mean_se<-age_by_set
age_mean_se$SETNO<-NULL
age_mean_se$MISSION<-NULL
age_mean_se<-aggregate(.~STRAT, data=age_mean_se, st.err)
setnames(age_mean_se,
        old=names(age_mean_se[,2:ncol(age_mean_se)]), 
        new=c(paste0("age_",theseages,"_se")))

age_pretotal<-as.data.table(merge(strata_area,age_by_set, by="STRAT"))
age_pretotal$SQNM<-NULL
age_pretotal[, (theseages) := lapply(.SD, 
                              function(x) x * age_pretotal[['TUNITS']] ), 
                              .SDcols = theseages]
age_pretotal$TUNITS<-NULL
age_total<-age_pretotal
age_total$MISSION<-NULL
age_total$SETNO<-NULL
age_total<-aggregate(.~STRAT, data=age_total, mean)
setnames(age_total,
          old=names(age_total[,2:ncol(age_total)]), 
          new=c(paste0("age_",theseages,"_tot")))
age_total_se<-age_pretotal
age_total_se$MISSION<-NULL
age_total_se$SETNO<-NULL
age_total_se<-aggregate(.~STRAT, data=age_total_se, st.err)
setnames(age_total_se,
          old=names(age_total_se[,2:ncol(age_total_se)]), 
          new=c(paste0("age_",theseages,"_tot_se")))
age_by_set.cnt<-aggregate(list(
                          COUNT=age_by_set$STRAT), 
                          by=list(STRAT=age_by_set$STRAT), 
                          FUN=length)
ages<-merge(age_by_set.cnt,age_mean)
ages<-merge(ages,age_mean_se)
ages<-merge(ages,age_total)
ages<-na.zero(merge(ages,age_total_se))
} #end of age calculations

input_parameters<-list("Stratisfy version"=paste0( stratisfy.ver ),
  "Analysis Date"=Sys.time(),
  "Experiment Type"=these.type, 
  "Strata"=these.strat, 
  "Missions"=these.missions,
  "Year"=year,
  "By.Sex"=by.sex,
  "Species"=paste0(species.code, " (",species.name,")"),
  "Wingspread"=wingspread, 
  "Distance"=towdist,
  "Data Source"="DFO",
  "ALK Modifications"="No",
  "Area"="None")
################################################################################
###                          RESULTS                                            
results<-list(
  input_parameters=input_parameters,
  strata.areas=strata.areas, 
  set_info=set_info,
  length_by_set=length_by_set, 
  length_mean=length_mean, 
  length_mean_se=length_mean_se, 
  length_total=length_total, 
  length_total_se=length_total_se,
  nw_by_set=nw_by_set,   
  weights=weights,
  numbers=numbers, 
  age_table=age_table, 
  age_length_key_totals=age_length_key_totals,
  age_length_weight=age_length_weight,
  age_by_set=age_by_set,  
  ages=ages)
# for more information about the results, please see 
#http://gitlab.ssc.etg.gc.ca/mcmahon/PED_Analytics/blob/
#c844101a518ba68a4f0914460adcb4e0178536bb/stranal/README.md#Results
return(results)
}
# example function calls can be found at 
#http://gitlab.ssc.etg.gc.ca/mcmahon/PED_Analytics/blob/
#c844101a518ba68a4f0914460adcb4e0178536bb/stranal/README.md#Running