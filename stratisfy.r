stratisfy<-function(user=-1, password=-1 #these are your Oracle credentials
                    ){
################################################################################
###                                                                             
###       Title:                stratisfy.r                                         
###                                                                             
###       Author:               Mike McMahon                                    
###                             Mark Fowler                                     
###                             Adam Cook                                       
###                                                                             
###       Modification Date:    Nov 9, 2015                                    
          stratisfy.ver = '2015.11.09'
###                                                                             
###       Description:          Replaces the standalone STRANAL application     
###                             (written in APL), as well as the numbers and    
###                             weights analytic from the old VDC               
###                                                                             
###       TODO:  US data needs to be QC'd                
###              Net conversion needs to be implemented
###              ALK Modifications need to be implemented
###              Custom Age\Length Tables to be implemented
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
options(stringsAsFactors = FALSE) #necessary?
options(scipen=999)  # this avoids scientific notation
################################################################################
###                          FUNCTIONS (general)            

st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

#stolen from https://github.com/jae0/ecomod/ -- "na.zero.r"
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

#prompt user for credentials several times
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
            
            get.stratum.tables<-function(agency.gui){
              if (agency.gui=="DFO"){
                stratum.tables<-c("GROUNDFISH.GSSTRATUM")
              }else{
                stratum.tables<-c("GROUNDFISH.GSSTRATUM",
                                  "USNEFSC.DFO5ZJM", 
                                  "USNEFSC.DFO5ZGHNO", 
                                  "USNEFSC.NMFS5ZJM", 
                                  "USNEFSC.NMFS5ZGHNO", 
                                  "USNEFSC.NMFS5ZJMC", 
                                  "USNEFSC.NMFS5ZJMU", 
                                  "USNEFSC.NMFS5ZU")
              }
            return(stratum.tables)
            }
            
            get.type<-function(agency.gui) {
              if (agency.gui=="DFO"){
                the.type <- sqlQuery(channel, 
                                     paste("select XTYPEDESC, XTYPE 
                                         from groundfish.GSXTYPE
                                         ORDER BY XTYPE",sep=""))
                the.type<-paste( the.type[,1], " (", the.type[,2],")",sep="") 
              }else{
                the.type="111" 
              }
              return(the.type)
            }
            
            get.year<-function(agency.gui) {
              if (agency.gui=="DFO"){
                year.query = "select distinct YEAR 
                                         from groundfish.gsmissions
                                         ORDER BY YEAR DESC"  
              }else{  
                #10=  NMFS NEFSC BOTTOM TRAWL SURVEY
                #11 = '36 YANKEE TRAWL'
                #41 = Mod. 41 Yankee Trawl (Accepted Code)
                #45 = Mod. 41 Yankee Trawl (Erroneously Coded on Several Cruises)
                year.query = "SELECT DISTINCT Year
                                FROM USNEFSC.USS_MSTR_CRUISE
                                WHERE PURPOSE_CODE='10' AND 
                                SVGEAR In ('11','41','45')
                                ORDER BY Year DESC"
              }
              the.year = sqlQuery(channel, year.query)
              US_Conn<-grepl("TNS:Connect timeout occurred", as.character(the.year[1]))
              if (US_Conn){
                print("ERROR -- Can't connect to US database")
                return()
              }
              the.year<-paste( the.year[,1],sep=",") 
              return(the.year)
            }
            
            get.missions<-function(agency.gui,year.gui){
              if (agency.gui=="DFO"){
                mission.query=paste("select MISSION, VESEL, CRUNO, YEAR
                                    from groundfish.gsmissions 
                                     WHERE YEAR = ",year.gui,"
                                    ORDER BY MISSION", sep="") 
              }else{
                # removed gear restriction from NMFS
                #--AND SVGEAR In ('11','41','45')
                mission.query=paste("SELECT CRUISE6 AS MISSION, SVVESSEL AS VESEL, 
                                      CRUISE AS CRUNO, YEAR
                                      FROM USNEFSC.USS_MSTR_CRUISE
                                      WHERE PURPOSE_CODE='10' 
                                      AND SEASON IN ('SPRING','SUMMER','FALL','WINTER')
                                      AND YEAR = ",year.gui,"
                                      ORDER BY CRUISE6", sep="")
              }
              missions.data<-sqlQuery(channel, mission.query)
              missions<-select.list(missions.data$MISSION, 
                          multiple=F, graphics=T, 
                          title='Please choose a mission:') 
             
              if (agency.gui=="DFO"){
                mission.id<-gsub("\\s?\\(.*?\\)", "", missions)
              }else{
                mission.id<-as.numeric(gsub("\\s?\\(.*?\\)", "", missions))
              }
              missions.data<-missions.data[missions.data$MISSION==mission.id,]
              mission.info<-list(mission.id,missions.data)
              return(mission.info)
            }
            
            get.species<-function(agency.gui, by.sex.gui) {
              if (agency.gui=="DFO"){
                if (isTRUE(by.sex.gui)){
                  species.query.tweak<-"AND LFSEXED = 'Y' "
                } else{
                  species.query.tweak<-""
                }
                species.query=paste("SELECT DISTINCT(SPEC), initcap(CNAME) CNAME, 
                                    LGRP, LFSEXED 
                                    FROM GROUNDFISH.GSSPEC 
                                    WHERE SPEC <> 9999 
                                    ", species.query.tweak, " 
                                    ORDER BY initcap(CNAME)",sep="")
              }else{ 
                #not ideal - hard coded spp from uss_catch table with more than 1 catchsex
                if (isTRUE(by.sex.gui)){
                  species.query.tweak<-"AND SPEC IN ('015','022','026','075','108')"
                } else{
                  species.query.tweak<-""
                }
                species.query=paste("SELECT US.SPEC, INITCAP(US.CNAME) CNAME, 
                                    US.LGRP, US.LFSEXED 
                                    FROM USNEFSC.USSPEC US
                                    WHERE  
                                    US.SPEC <> 9999
                                    ", species.query.tweak,"
                                    ORDER BY initcap(US.CNAME)",sep="")
              }
              the.species = sqlQuery(channel, species.query)
              return(the.species)
              }
            
            get.strata<-function(agency.gui, year.gui, missions.gui, stratum.table.gui,type.gui) {
             if (agency.gui =="DFO"){
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
              }else{
              strata.query=paste0("SELECT STRAT 
                                    FROM ",stratum.table.gui,"
                                    WHERE LENGTH(STRAT)=5
                                    ORDER BY STRAT")
              the.strata <- sqlQuery(channel,strata.query)
              #have to do nmfs strat differently, since they leading zeros
              the.strata<-sprintf("%05d", the.strata$STRAT)
              the.strata<-paste(the.strata,sep=",") 
              }
              return(the.strata)
            }   
            
            agency.gui<-select.list(c("DFO","NMFS"), 
                                    multiple=F, graphics=T, 
                                    title='Please choose an agency:')
            
            by.sex.gui<-select.list(c("Unsexed Analysis","Sexed Analysis"),
                                    multiple=F, graphics=T, 
                                    title='Analysis by Sex?')
            
            if (by.sex.gui=="Sexed Analysis") by.sex.gui=T else by.sex.gui=F
            
            if (agency.gui=='DFO'){
                stratum.table.gui="GROUNDFISH.GSSTRATUM"
                type.gui<-select.list(get.type(agency.gui), 
                                          multiple=F, graphics=T, 
                                          title='Please select the type of trawl:')
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
            
            the.species<-get.species(agency.gui, by.sex.gui)
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
            
            year.gui<-select.list(paste(get.year(agency.gui),sep=","),
                                  multiple=F, graphics=T, 
                                  title='Choose a year:')
            
            mission.info<-get.missions(agency.gui,year.gui)
            missions.gui<-SQL.in(mission.info[[1]])
            mission.df<-mission.info[[2]]
            if (agency.gui=="DFO"){
              #thought it would be handy to default to the summer survey strata
              pre<-as.character(c(intersect(get.strata(agency.gui,year.gui,missions.gui,stratum.table.gui,type.gui),c(440:495))))
            }else{
              pre<-""
            }
            strata.gui<-select.list(paste(get.strata(agency.gui,year.gui,missions.gui,stratum.table.gui,type.gui),sep=","),
                                multiple=T, graphics=T, 
                                preselect=pre,
                                title='Choose the strata:')
            strata.gui = SQL.in(strata.gui)
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
                              missions.gui,
                              mission.df,
                              agency.gui,
                              species.lgrp.gui,
                              species.lfsexed.gui,
                              stratum.table.gui)
            return(GUI.results)
}
#end of GUI
################################################################################
  GUI.results<-gui()
  wingspread = GUI.results[[1]]
  towdist = GUI.results[[2]]
  by.sex = GUI.results[[3]]
  year = GUI.results[[4]]
  species.code = GUI.results[[5]]
  species.name = GUI.results[[6]]
  these.type = GUI.results[[7]]
  these.strat = GUI.results[[8]]
  these.missions = GUI.results[[9]]
  mission.df = GUI.results[[10]] 
  agency.gui = GUI.results[[11]] 
  species.lgrp.gui = GUI.results[[12]] 
  species.lfsexed.gui = GUI.results[[13]] 
  stratum.table.gui = GUI.results[[14]] 

################################################################################
###                          DATABASE EXTRACTIONS                               

#limit extraction by the users' selections
if (agency.gui=='DFO'){
raw.gscat.query <- 
  paste("select mission,setno,size_class,totwgt,sampwgt,totno,calwt
          from 
          groundfish.gscat 
          where 
          spec=",species.code,"
          and mission IN (",these.missions,")", sep="")

}else{
  #adding fake values for calwt(0), size_class(1), and sampwgt(0) so data format matches CDN data
  raw.gscat.query <- 
    paste("select cruise6 mission,to_number(station) setno, 1 size_class, sum(expcatchwt) totwgt, 0 sampwgt, sum(expcatchnum) totno, 0 calwt
          from 
          usnefsc.uss_catch 
          WHERE 
          to_number(svspp)=",species.code,"
          and cruise6 IN (",these.missions,")
          AND STRATUM   IN (",these.strat,")
          group by 
          cruise6, to_number(station)", sep="")
}
raw.gscat<-sqlQuery(channel,raw.gscat.query)
if (nrow(raw.gscat)<1)
  stop("Error: No catch data can be found for your selection")
#added mission, strat and type filters
if (agency.gui=='DFO'){
  raw.gsinf.query<-
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
  #distance was assumed to be 1.75, but appears to be dopdistb
  raw.gsinf.query<-
    paste("SELECT CRUISE6 mission,to_number(station) setno, begin_est_towdate sdate, est_time time, STRATUM strat, 
    area,  BEGLAT slat, BEGLON slong, mindepth dmin, maxdepth dmax, avgdepth depth, towdur dur, dopdistb dist 
        FROM USNEFSC.USS_STATION 
        WHERE CRUISE6 in (",these.missions,")
        AND STRATUM in (",these.strat,")
        AND to_number(SHG) <= ",these.type, sep="")
  
}
raw.gsinf<-sqlQuery(channel,raw.gsinf.query )
if (agency.gui=="NMFS") raw.gsinf$STRAT<-sprintf("%05d", raw.gsinf$STRAT)

if (agency.gui=='DFO'){
  raw.gsdet.query<-
  paste("select mission,setno,size_class,fsex,age,fwt,
          decode(",species.lgrp.gui,",1,flen,2,.5+2*floor(flen/2),3,1+3*floor(flen/3),flen) flen,
          lgrp binwidth,lfsexed bysex, clen
          from 
          groundfish.gsdet g 
            LEFT OUTER JOIN groundfish.gsspec s ON (s.spec=g.spec)
          WHERE MISSION IN (",these.missions,")
          AND G.SPEC=",species.code,"
        ", sep="")
  raw.gsdet<-sqlQuery(channel,raw.gsdet.query)
}else{
  #missing fsex, sizeclass,clen
  raw.gsdet.query<-
        paste("
        select cruise6 mission, to_number(station) setno, age, length, avg(indwt) fwt,
        decode(",species.lgrp.gui,",1,length,2,.5+2*floor(length/2),3,1+3*floor(length/3),length) flen
        from usnefsc.uss_detail
        where to_number(svspp)=",species.code,"
        AND CRUISE6 in (",these.missions,")
        AND STRATUM in (",these.strat,")
        group by cruise6,station,age,length", sep="")
      raw.gsdet<-sqlQuery(channel,raw.gsdet.query )
  
  raw.lf.query<-
      paste("select cruise6 mission, catchsex fsex, station setno,length, 
      sum(expnumlen) clen, 1 size_class
      from usnefsc.uss_lengths
      where to_number(svspp)=",species.code,"
      and cruise6 in (",these.missions,")
      and catchsex in ('0','1','2')
      group by cruise6,station,length, catchsex",sep="")
  raw.lf<-sqlQuery(channel,raw.lf.query)

  raw.gsdet<-merge(raw.gsdet,raw.lf, all.x=T) 
  raw.gsdet$FLEN[is.na(raw.gsdet$FLEN)] <- raw.gsdet$LENGTH[is.na(raw.gsdet$FLEN)]

}

#calculate strat areas into tunits
#US nautical mile is 6080.2ft
strata.area.query<-
  paste("SELECT strat, area SQNM, 
         nvl(area,0)/(",towdist,"*(",wingspread,"/6080.2)) tunits 
          FROM 
          ",stratum.table.gui,"
          WHERE 
          strat IN (",these.strat,")", sep="")

strata.area<-sqlQuery(channel,strata.area.query)
if (agency.gui=="NMFS") strata.area$STRAT<-sprintf("%05d", strata.area$STRAT)
strata.area<-strata.area[order(strata.area$STRAT),c("STRAT","TUNITS","SQNM")]

#done with our database connection - close it
#odbcClose(channel)


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
nw_by_set_pre<-merge(raw.gsinf, raw.gscat, all.x=T)
nw_by_set_pre<-merge(nw_by_set_pre, mission.df, all.x=T)
nw_by_set_pre[which(is.na(nw_by_set_pre$TOTWGT)),
              c('SAMPWGT','TOTWGT','TOTNO','CALWT')] <- 0
nw_by_set_pre$SIZE_CLASS[which(is.na(nw_by_set_pre$SIZE_CLASS))] <- 1

nw_by_set_pre$DIST[which(is.na(nw_by_set_pre$DIST)|(nw_by_set_pre$DIST==0))] <-towdist

nw_by_set_pre$RAW_TOTWGT <-nw_by_set_pre$TOTWGT
nw_by_set_pre$TOTWGT <- (nw_by_set_pre$TOTWGT*towdist)/nw_by_set_pre$DIST
nw_by_set_pre$TOTWGT[which(!is.finite(nw_by_set_pre$TOTWGT))] <-1
nw_by_set_pre$RAW_TOTNO <-nw_by_set_pre$TOTNO 
nw_by_set_pre$TOTNO <- (nw_by_set_pre$TOTNO*towdist)/nw_by_set_pre$DIST
nw_by_set_pre$TOTNO[which(!is.finite(nw_by_set_pre$TOTNO))] <- 1
nw_by_set_pre<-nw_by_set_pre[order(nw_by_set_pre$STRAT,nw_by_set_pre$SETNO),]
#removed season from below
set_info<-nw_by_set_pre[order(nw_by_set_pre$STRAT,nw_by_set_pre$SETNO),
                        c("MISSION","STRAT","SETNO","SDATE","AREA",
                          "SLAT","SLONG","DMIN","DMAX","DEPTH","DUR","DIST" )]
nw_by_set<-nw_by_set_pre[order(nw_by_set_pre$STRAT,nw_by_set_pre$SETNO),
            c("STRAT","MISSION","SETNO","TOTNO","TOTWGT")]

nw_by_set_pre2<-nw_by_set_pre[order(nw_by_set_pre$STRAT,nw_by_set_pre$SETNO),
                c("STRAT","SLAT","SLONG","AREA","SETNO","TOTWGT","TOTNO")]

nw_by_set_pre2<-merge(nw_by_set_pre2,subset(strata.area,select=-c(SQNM)))
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
catchsets<-merge(strata.area,catchsets,by="STRAT",all.y=T)  
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

strata.areas<-merge(strata.area,catchsets.AreaProp,by="STRAT",all.x=T)
strata.areas<-merge(strata.areas,catchsets.AreaPropStErr,by="STRAT",all.x=T)
strata.areas<-merge(strata.areas,catchsets.AreaTot,by="STRAT",all.x=T)
strata.areas<-na.zero(merge(strata.areas,catchsets.AreaTotStErr,by="STRAT",all.x=T))
################################################################################
###                          SET UP AGELEN  
#remove records without weight or totalno
# nw_by_set_pre<-nw_by_set_pre[nw_by_set_pre$TOTNO>0|nw_by_set_pre$TOTWGT>0,]
agelen<-merge(nw_by_set_pre, raw.gsdet, by=c("MISSION", "SETNO","SIZE_CLASS"), all.x=T)
agelen<-merge(agelen,strata.area, all.x=T)
if (by.sex==F){
agelen$BYSEX<-'N'
}else{
agelen$BYSEX<-'Y'
}
agelen[which(is.na(agelen$BINWIDTH)), c('BINWIDTH')]<-as.numeric(species.lgrp.gui)
agelen[which(is.na(agelen$FLEN)), c('FLEN')] <- 0
agelen$FLEN<-floor(agelen$FLEN/agelen$BINWIDTH)*agelen$BINWIDTH

agelen$CAGE<-NA
if (agency.gui=="DFO"){
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
}else{
  agelen$CAGE<-agelen$CLEN
}
#defaults
agelen$DEPTH[is.na(agelen$DEPTH)]<--99
agelen$DMIN[is.na(agelen$DMIN)]<--99
agelen$DMAX[is.na(agelen$DMAX)]<--99
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
#SHOULD THIS USE CLEN, NOT CAGE?
li = which(lset$CAGE==0) 
lset$FLEN[li] = unique(lset$FLEN)[1]
 lset<-aggregate(lset$CAGE,
                 lset[fields],
                 FUN=sum)

lset<-lset[with(lset,order(get(order.c))),]
lset<-melt(lset,id.vars=fields)
#not very slick - would like to be able to dynamically send columns to dcast
if (isTRUE(by.sex)){  
  
  lset$FSEX[is.na(lset$FSEX)]<-'UNK' #MMM Oct 28, 2015
  lset$FSEX[lset$FSEX==0]<-'UNK'
  lset$FSEX[lset$FSEX==1]<-'MALE'
  lset$FSEX[lset$FSEX==2]<-'FEMALE'
  length_by_set <- na.zero(dcast(lset, STRAT + MISSION + SETNO ~ FSEX +FLEN  ))
}else{
  #following gives - Aggregation function missing: defaulting to length
  length_by_set <- na.zero(dcast(lset, STRAT + MISSION + SETNO ~ FLEN ))
}
length_by_set<-length_by_set[order(length_by_set$STRAT,length_by_set$SETNO),]
length_total<-merge(subset(strata.area,select=-c(SQNM)),length_by_set)
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
  print("age calculations unavailable - no ages in data")
  age_by_set<-"can't do age_by_set - no ages in data"
  ages<-"can't do ages - no ages in data"
  age.length.key.totals<-"can't do age.length.key.totals - no ages in data"
  age_table<-"can't do age_table - no ages in data"
  age_length_weight<-"can't do age_length_weight - no ages in data"
}else{
all.ages = seq(min(alk$AGE),max(alk$AGE)) 
all.lengths = seq(min(alk$FLEN),max(alk$FLEN),by=as.numeric(species.lgrp.gui )) 
al = expand.grid(all.ages,all.lengths)
names(al) = c('AGE','FLEN')
al$SETNO = al$CAGE = 0
alk = rbind(alk,al)
alk$SETNO = ifelse(alk$SETNO>0,1,NA)

age.length.key<-na.zero(
  t(
    tapply(
      alk$SETNO,
      list(alk$AGE, alk$FLEN),
      function(x) length(x[!is.na(x)])
    )
  )
)
Length_Totals<-rowSums(age.length.key, dims = 1)

age.length.key.totals<-cbind(age.length.key,Length_Totals) 

Age_Totals<-colSums(age.length.key.totals, dims = 1)
age.length.key.totals<-rbind(age.length.key.totals,Age_Totals)

alw<-aggregate(FWT~AGE+FLEN,data=agelen,FUN=mean)
alw<-alw[order(alw$AGE,alw$FLEN),]
alw$FWT = alw$FWT / 1000
age_length_weight = na.zero(reshape(
                            alw,idvar='FLEN',timevar='AGE',direction='wide'))
age_length_weight<-age_length_weight[order(age_length_weight$FLEN),]
rownames(age_length_weight)<-age_length_weight[,1]
age_length_weight$FLEN<-NULL
################################################################################
###                          AGE CALCULATIONS                                   

lengths<-ColTotalsLength[1:length(ColTotalsLength)-1] 
lengths1<-as.data.frame(lengths)
lengths1$FLEN<-names(lengths)
lengths1<-merge(ages_prop,lengths1,all.x=T)


ages_prop<-prop.table(as.matrix(age.length.key),1) 
ages_prop<-ifelse(is.nan(ages_prop),0,ages_prop)
ages_prop<-as.data.frame(ages_prop)
theseages<-c(names(ages_prop))

age_table<-na.zero(ages_prop*lengths1$lengths)

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

age_pretotal<-as.data.table(merge(strata.area,age_by_set, by="STRAT"))
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
  "Data Source"=agency.gui,
  "ALK Modifications"="No")
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
  nw=nw,   
  weights=weights,
  numbers=numbers, 
  age.length.key.totals=age.length.key.totals,
  age_table=age_table, 
  age_length_weight=age_length_weight,
  age_by_set=age_by_set,  
  ages=ages)
return(results)
}
# for more information about the results, please see 
#http://gitlab.ssc.etg.gc.ca/mcmahon/PED_Analytics/tree/master/stratisfy/README.md#Results
# here's an example of how to run this
#test<-stratisfy(user=oracle.personal.username,password=oracle.personal.password)
