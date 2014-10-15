#R version of STRANAL
############################
#User PARAMETERS
#
year=1974
species.code=11
strat.min=470
strat.max=495
#
############################
#MMM 2014
  #Extensively modified from Mark's submitted code - orginally this was 
  #a combination of SQL and R snippets in a single text file - it 
  #required a schema with write access to Oracle to generate a bunch of 
  #temporary tables.  Because of the combined languages,and could not be 
  #run within a single program.  I have attempted to put it all in R - 
  #replacing temporary tables with RODBC queries loading data.frames, 
  #and if that is not immediately obvious or possible, I have saved the 
  #data as csv files in the data folder, and then load that data into 
  #data.frames.

#Mark Fowler Aug 2014
  #Interactive scripting, jumping around between SQL and R 
  #Current example is 4X Haddock in 1974. This was truthed against the 
  #original APL version of STRANAL by Stratis Gavaris.
  #Intention is to add STRANAL to SurveyScope as a web application. This
  #scripting file serves to document the methods, and may suffice if the 
  #APL version is lost to us before incorporation into SurveyScope. 
  #Apparently IT will not maintain the original.
  #Arithmetic precision differs between APL and R. For example we see 
  #differences in stratified means beginning about the 3rd decimal
  # place. After all the math and rounding and bumping by trawlable 
  #units, we see annual totals of 89563863 from The APL STRANAL and
  #89563906 from the R STRANAL, a difference of 0.0000005%.
  #Using Oracle SQL*Plus (from the DFO SMS) to create custom database 
  #objects. Not the only way to do this. Requires an account with access 
  #to GROUNDFISH on BANK. 
  
  #Stock identification (species, strata) is hard-coded so must be 
  #edited as required for several of the SQL steps.

  #Sex and bin width stipulations are the 'official' defaults pulled 
  #from groundfish.gsspec. If you want to change these go to the SQL 
  #step that creates stock_all_raw_age and follow instructions in 
  #comments there. Encountered a conceivable error in the APL STRANAL, 
  #but could also be a known and disregarded issue. This is discussed in 
  #comments associated with replicating the Length Weight sheet.

  #Some scripting is redundant with SurveyScope, but included here so 
  #STRANAL can be achieved as a stand-alone job.
library(RODBC)
library(sqldf)

wd <- file.path(project.directory('stranalR'),'data')
channel<-odbcConnect(uid=oracle.personal.user,pw=oracle.personal.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
options(stringsAsFactors = FALSE)

#FOWLER 2014 
  #original surveyscope table
  #This is a lookup table for valid surveys by Maritimes and Gulf 
  #regions. It must be updated for new surveys when loaded.
  #At present we do not resolve NMFS surveys. The APL version does or 
  #did (Y/N?). 
#MMM Oct 2014
  #Mark generated a temporary oracle table using many "insert into" 
  # statements.  I took the data out of that format and placed it into a 
  #stand-alone csv file.  This is still not ideal as it requires csv to 
  #be updated - can we get this from the db? Note also that the original 
  #values like 'SUMMER ', 'FALL ', etc included trailing spaces.  I 
  #removed them, as they can only increase the suffering
survey_list<-read.csv(file=file.path(wd,"survey_list.csv"), head=T, sep=",")

#FOWLER 2014 
  #original surveyscope tables
  #These provide hydrographic data at trawl depth associated with 
  #surveys. It all gets wrapped up in the 'bottom' table, which is then 
  #linked to the sampling data. Not required for STRANAL, but can serve 
  #as valid set filters so included.
#MMM Oct 2014
  #gstri generated using r instead of SQL
gstri <- subset(survey_list , (survey_list$year>=1970 & survey_list$series %in% c('SUMMER','SPRING','FALL','4VWCOD','GEORGES')))

#MMM Oct 2014
  #ctd made from joining dataframe gstri to extraction from gshyd
gshyd_source_query<-
  paste("select mission, setno, sdepth, gear, temp, sal, bid
          from 
          groundfish.gshyd
          where 
          substr(mission,4,4) = ",year," 
          and gear in (1,2) 
          and bid='B' 
          and temp is not null",sep="")
          #MMM added year limiter here to reduce extraction size
gshyd_source<-sqlQuery(channel,gshyd_source_query)

#MMM Oct 2014
#ctd and bot now share a common extraction of gshyd added year limiter 
#to reduce extraction size
ctd<-sqldf(
  paste("select h.mission, h.setno, h.sdepth, h.gear, h.temp, h.sal, h.bid 
          from gshyd_source h, gstri t
          where 
          substr(h.mission,1,3) = t.vesel
          and substr(h.mission,8,3) = t.cruno
          and substr(h.mission,4,4) = t.year
          and gear=2",sep="")
         )

bot<-sqldf(
  paste("select h.mission, h.setno, h.sdepth, h.gear, h.temp, h.sal, h.bid 
          from gshyd_source h, gstri t
          where 
          substr(h.mission,1,3) = t.vesel
          and substr(h.mission,8,3) = t.cruno
          and substr(h.mission,4,4) = t.year
          and gear=1",sep="")
          )

#MMM Oct 2014
  #the sql proved to complex for me and/or sqldf - come back to it
bottom <- sqldf("select * from ctd 
union
select * from bot 
where (mission, setno) in
(select mission, setno from bot except
 select mission, setno from ctd")

#FOWLER 2014
  #original surveyscope table 
  #For SurveyScope this is part of a batch job in which custom tables 
  #are created for every species and/or stock. The species is a 
  #substitution variable in the batch job (spec=&&1), hard-coded here

stock_all_raw_cat_query <- 
  paste("select mission,setno,size_class,totwgt,sampwgt,totno,calwt
  from 
  groundfish.gscat 
  where spec=",species.code,"
  and substr(mission,4,4) = ",year, sep="")
  #MMM added year limiter here to reduce extraction size
stock_all_raw_cat<-sqlQuery(channel,stock_all_raw_cat_query)

#FOWLER 2014
  #original surveyscope table 
#MMM Oct 2014 
  #following needs to join to a dataframe
gsinf_source_query<-
paste("select to_number(substr(i.mission,4,4))year,
  i.mission,i.setno,sdate,time,strat,area,slat,slong,dmin,dmax,depth,dur,dist
  from groundfish.gsinf i
where substr(mission,4,4) = ",year, sep="")
#MMM added year limiter here to reduce extraction size
gsinf_source<-sqlQuery(channel,gsinf_source_query)

stock_all_adj_cat <- 
  sqldf("select series, substr(i.mission,4,4) year,
  i.mission,i.setno,w.size_class,sdate,time,strat,area,slat,slong,dmin,dmax,depth,dur,dist,
  totwgt raw_totwgt,
  IFNULL(totwgt*1.75/dist,0) totwgt,
  totno raw_totno,
  IFNULL(totno*1.75/dist,0) totno,  
  sampwgt,calwt
  from gsinf_source i, stock_all_raw_cat w, survey_list s
  where i.mission = w.mission
  and i.setno = w.setno
  and substr(i.mission,1,3) = s.vesel
  and substr(i.mission,8,3) = s.cruno
  and substr(i.mission,4,4) = s.year")
#MMM Oct 2014
  #changed nvl to ifnull
  #join is not correct for the following - had to remove (+)
    #where i.mission = w.mission(+)
    #and i.setno = w.setno(+)





stock_all_adj_cat_query <- 
"select series, to_number(substr(i.mission,4,4))year,
  i.mission,i.setno,w.size_class,sdate,time,strat,area,slat,slong,dmin,dmax,depth,dur,dist,
  totwgt raw_totwgt,
  NVL(totwgt*1.75/dist,0) totwgt,
  totno raw_totno,
  NVL(totno*1.75/dist,0) totno,
  sampwgt,calwt
  from groundfish.gsinf i, stock_all_raw_cat w, survey_list s
  where i.mission = w.mission(+)
  and i.setno = w.setno(+)
  and substr(i.mission,1,3) = s.vesel
  and to_number(substr(i.mission,8,3)) = s.cruno
  and to_number(substr(i.mission,4,4)) = s.year"
stock_all_adj_cat<-sqlQuery(channel,stock_all_adj_cat_query)

#FOWLER 2014
  #STRANAL-specific tables from here on
  #Provide data to build age:length keys and age composition. Species is hard-coded.
  #Grab default bin width and whether age composition should be sexed or not from gsspec.
  #If you want non-default stipulations for bins or sexing, replace the variable values below, e.g. lgrp 1,lfsexed 'Y'
  #The bin width and sex stipulations would be options in an app (default or custom).
  # drop table stock_all_raw_age;
stock_all_raw_age_query<-
  paste("select mission,setno,size_class,fsex,age,fwt,
    decode(1,1,flen,2,.5+2*floor(flen/2),3,1+3*floor(flen/3),flen) flen,
    lgrp binwidth,lfsexed bysex, clen
    from groundfish.gsdet g, groundfish.gsspec s
    where g.spec=",species.code,"
    and s.spec=g.spec(+)",sep="")
stock_all_raw_age<-sqlQuery(channel,stock_all_raw_age_query)

#FOWLER 2014
  #Adjust raw counts for subsampling and actual tow distance (relative to standard 1.75 nm tow).
  #Aggregate counts at length according to the bin width.
  #Provide trawlable units for strata. Seems the survey database should include a table with these.
  #Only addresses standard Maritimes and Gulf region survey strata, not NMFS or Industry surveys.
#MMM Oct 08, 2014 - MMM
  #Mark forced the addition of the tunits value for each strata as a long decode statement within the SQL
  #Similar to the survey_list data, I extracted the values into an external csv file that is then read in
  #Maybe this exists as a table in the groundfish schema we can query directly? (instead of maintaining a csv?)
  #Also, requires extra joining (tunits to stock_all_adj_age), since Mark added this data on the fly to the 
  #appropriate records
tunits<-read.csv(file=file.path(wd,"tunits.csv"), head=T, sep=",")
stock_all_adj_age_query<-
  "select series,year,strat,
    c.mission,c.setno,dist,sampwgt,raw_totwgt,c.size_class,fsex,bysex,binwidth,floor(flen/binwidth)*binwidth flen,age,fwt,
    decode(sampwgt,0,0,NULL,NULL,(raw_totwgt/sampwgt)*1.75/dist*clen) cage
    from stock_all_adj_cat c, stock_all_raw_age l
    where c.mission=l.mission(+)
    and c.setno=l.setno(+)
    and c.size_class=l.size_class(+)"
stock_all_adj_age<-sqlQuery(channel,stock_all_adj_age_query)
#!!MMM cbind the tunits data to the stock_all_adj_age data

#FOWLER 2014
  #The primary table to accomplish STRANAL.
  #Includes some spurious variables that some might want to consider as 
  #explanatory or filter variates (e.g. don't include sets below some
  # temperature). Making flen reflect the centre of its bin.

stock_agelen_query<-
  "select a.series,a.mission,substr(i.mission,1,3) vessel,
    to_number(substr(i.mission,4,4)) year,
    to_number(substr(i.mission,8,3)) cruno,a.setno,
    i.strat,i.slat,i.slong, ga.name,nvl(i.depth,-99) depth, nvl(i.dmin,-99) dmin,
    nvl(i.dmax,-99) dmax, nvl(i.time,-999) loctime,
    nvl(b.SDEPTH,-99.9) sdepth, nvl(b.TEMP,-99.99) temp, nvl(b.SAL,-99.999) sal,
    a.dist, a.tunits,a.sampwgt,a.raw_totwgt,a.size_class,a.fsex,bysex,binwidth,a.flen+(binwidth*.5)-.5 flen,a.age,a.fwt,a.cage
    from groundfish.gsinf i, stock_all_adj_age a, bottom b, groundfish.gsstratum ga
    where i.mission=a.mission(+)
    and i.setno=a.setno(+)
    and i.mission=b.mission(+)
    and i.setno=b.setno(+)
    and i.strat=ga.strat(+)
    and type='1'"
stock_agelen<-sqlQuery(channel,stock_agelen_query)

#FOWLER 2014
  #From here on replicating the APL STRANAL Excel sheet outputs in the 
  #same order as they appear in the Excel file. Skipped the first 
  #(QUERY) sheet and only do the second (Strata Area) sheet in SQL 
  #(without summing for totals). All the rest conform to the Excel file 
  #that I was given for truthing, inclusive of formatting, making it 
  #easy to compare with historical results.

  #Strata Area sheet
  #Hard-coded selection of strata 470-495 (4X Haddock stock definition)
tunits_results<-tunits[tunits$strat>=strat.min & tunits$strat<=strat.max,]


#age length key (not formatted for output)
alk_query<-
  paste("select age,flen,sum(setno/setno) cage from stock_agelen
    where year=",year," and age is not null
    and to_number(strat)>=",strat.min," and to_number(strat)<=",strat.max," 
    group by age,flen
    order by age,flen",sep="")
alk<-sqlQuery(channel,alk_query)

#length by set sheet (not formatted for output)
lset_query<-
  paste("select strat,slat,slong,name unitarea,mission,setno,flen,sum(cage) cage from stock_agelen
    where year=",year," and to_number(strat)>=",strat.min," and to_number(strat)<=",strat.max,"
    group by strat,slat,slong,name,mission,setno,flen
    order by strat,slat,slong,name,mission,setno,flen",sep="")
lset<-sqlQuery(channel,lset_query)

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
LF_query<-
  paste("select vessel,mission,sa.strat,tunits,setno,flen, sum(cage) cage, sum(nvl(cage,0)*tunits) sage, sets
    from stock_agelen sa, xxx2 x
    where year=",year," and to_number(sa.strat)>=",strat.min," and to_number(sa.strat)<=",strat.max,"
    and x.strat=sa.strat(+)
    group by sa.strat,tunits,vessel,mission,setno,flen,sets
    order by sa.strat,tunits,vessel,mission,setno,flen,sets",sep="")
LF<-sqlQuery(channel,LF_query)

#weight by set sheet (not formatted for output)
setwgt_query<-
  paste("select sa.strat,sa.slat,sa.slong,sa.name unitarea,sa.mission,setno,sets,tunits,x.areanm,avg(raw_totwgt*1.75/dist) setwt
 from stock_agelen sa, xxx2 x
where year=",year," and to_number(sa.strat)>=",strat.min," and to_number(sa.strat)<=",strat.max,"
and x.strat=sa.strat(+)
group by sa.strat,sa.mission,setno,sa.slat,sa.slong,sa.name,sets,tunits,x.areanm
order by sa.strat,sa.mission,setno,sa.slat,sa.slong,sa.name,sets,tunits,x.areanm",sep="")
setwgt<-sqlQuery(channel,setwgt_query)
