numbersandweights.gs<-function(showchemistry=FALSE){
  #DFO/RV Survey Numbers and Weights
  #ported to ecomod Feb 20, 2015 (Mike McMahon)
  
  # TO  DO ----------------------------------------------------------
  #Add following functionality already present on the VDC:
    #1 selecting a year shows all of the strata with all of the same data limited to that year alone
    #2 when looking at single year's worth of data, selecting a stratum shows all of the sets from that stratum
  
    #3 selecting the hyperlinked column "numbers" shows length frequencies for each year  
      #selecting ain individual hyperlinked stratified mean number for a number would plot the length frequencies for that year
    
    #4 selecting a hyperlinked stratified mean weight would map the weights for that year 
      #selecting an individual hyperlinked column "weight" shows maps of weights for each year
  
    #5 the mission name is shown, as well as the date range for that mission.
  
    #6 the standard error is shown

  # CONCERNS ----------------------------------------------------------
  # This relies on some derived tables that were created for the VDC, rather than calculating
  # the information from the original data (eg nwags.gsscat_mv, nwags.gssinf_mv, mflib.gsmgt).  
  # This has the potential to be ugly as additional grants will be necessary so tha users can 
  # access everythingthat will be needed.
  
  library(RODBC)
  chan<-odbcConnect(uid=oracle.vdc.user,pw=oracle.vdc.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
  
  selections<-populateselections()
  
  paramlist<-NULL
  paramlist<-list()
  paramlist[[":bind__Species"]]<-select.list(as.character(selections$the.species[[2]]),title='Choose a species:',multiple=F,graphics=T,preselect="Silver Hake")
  paramlist[[":bind__Series"]]<-select.list(selections$the.series,title='Choose a series:',multiple=T,graphics=T, preselect=c('SUMMER'))
  paramlist[[":bind__Strata_forcequote"]]<-select.list(selections$the.strata,title='Choose strata:',multiple=T,graphics=T, preselect=as.character(c(440:495)))
  paramlist[[":bind__Years"]]<-select.list(selections$the.years,title='Choose time span:',multiple=T,graphics=T, preselect=as.character(c(1970:2014)))
  
  sselect<-"
  SELECT 
  i.year, SUM(i.tunits) tunits, SUM(ntows) sets,
  ROUND(SUM(i.tunits*NVL(c.avgstdno,0))/1000000,2) strat_tot_num,
  ROUND(SUM(i.tunits*NVL(c.avgstdno,0))/SUM(i.tunits),2) strat_mean_num,
  ROUND(SUM(i.tunits*NVL(c.varstdno,0))/SUM(i.tunits),2) strat_var_num,
  ROUND(SUM(i.tunits*NVL(c.avgstdwgt,0))/1000,2) strat_tot_wt,
  ROUND(SUM(i.tunits*NVL(c.avgstdwgt,0))/SUM(i.tunits),2) strat_mean_wt,
  ROUND(SUM(i.tunits*NVL(c.varstdwgt,0))/SUM(i.tunits),2) strat_var_wt,
  ROUND(SUM(i.temp_area*i.avgbtemp)/SUM(i.temp_area),1) bottom_temperature,
  ROUND(SUM(i.sal_area*i.avgbsal)/SUM(i.sal_area),1) bottom_salinity
  FROM (SELECT year, strat, avgstdwgt, avgstdno,varstdwgt,varstdno
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
  sql<-binder(sselect, paramlist)
  df<-sqlQuery(chan,sql)
  close(chan)
  output<-list()
  output[["paramlist"]]<-paramlist
  output[["sql"]]<-sql
  output[["df"]]<-df[order(-df$YEAR),]
  numbersandweights.plots(df, showchemistry=showchemistry)
  return(output)
}



#loadfunctions("vdc.analytic.numbersandweights")


#data<-test[[3]]
