gsnumbersandweights<-function(){
  #DFO/RV Survey Numbers and Weights
  #ported to ecomod Feb 20, 2015 (Mike McMahon)
  
  library(RODBC)
  chan<-odbcConnect(uid=oracle.vdc.user,pw=oracle.vdc.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
  
  
  selections<-populateselections()
  
  paramlist<-list()
  paramlist[[":bind__Species"]]<-select.list(as.character(selections$the.species[[2]]),title='Choose a species:',multiple=F,graphics=T,preselect="White Hake")
  paramlist[[":bind__Series"]]<-select.list(selections$the.series,title='Choose a series:',multiple=T,graphics=T, preselect=c('SUMMER','SUMMER_TELEOST'))
  paramlist[[":bind__Strata_forcequote"]]<-select.list(selections$the.strata,title='Choose strata:',multiple=T,graphics=T, preselect=as.character(c(440:495)))
  paramlist[[":bind__Years"]]<-select.list(selections$the.years,title='Choose time span:',multiple=T,graphics=T, preselect=as.character(c(2004:2008)))
  
  sselect<-"
  SELECT 
  i.year, SUM(i.tunits) tunits, SUM(ntows) sets,
  ROUND(SUM(i.tunits*NVL(c.avgstdno,0))/SUM(i.tunits),2) avg_number,
  ROUND(SUM(i.tunits*NVL(c.avgstdwgt,0))/SUM(i.tunits),2) avg_weight,
  ROUND(SUM(i.tunits*NVL(c.avgstdno,0))/1000000,2) abundance,
  ROUND(SUM(i.tunits*NVL(c.avgstdwgt,0))/1000,2) biomass,
  ROUND(SUM(i.temp_area*i.avgbtemp)/SUM(i.temp_area),1) bottom_temperature,
  ROUND(SUM(i.sal_area*i.avgbsal)/SUM(i.sal_area),1) bottom_salinity
  FROM (SELECT year, strat, avgstdwgt, avgstdno
        FROM nwags.gsscat_mv
        WHERE upper(series) in (:bind__Series)
        AND spec in (SELECT research FROM species_codes WHERE upper(common)=upper(:bind__Species))
        AND strat in (SELECT DISTINCT strat FROM mflib.gsmgt WHERE unit in(:bind__Strata_forcequote))
        AND year IN (:bind__Years)
  ) c,
  (SELECT year, i.strat, ROUND(area/(1.75*(41/6080.2))) tunits, ntows,
   DECODE(NVL(avgbtemp, -99),-99,NULL,area) temp_area, avgbtemp,
   DECODE(NVL(avgbsal, -99),-99,NULL,area) sal_area, avgbsal
   FROM nwags.gssinf_mv i,
   nwags.gsstratum s
   WHERE upper(series) in (:bind__Series)
   AND i.strat in (SELECT DISTINCT strat FROM mflib.gsmgt WHERE unit in(:bind__Strata_forcequote))
   AND s.strat=i.strat
   AND year IN (:bind__Years)
  ) i
  WHERE c.strat(+)=i.strat
  AND c.year(+)=i.year
  GROUP BY i.year
  ORDER BY 1"
  data<-sqlQuery(chan,binder(sselect, paramlist))
  close(chan)
  return(data)
}

gsnumbersandweights()
