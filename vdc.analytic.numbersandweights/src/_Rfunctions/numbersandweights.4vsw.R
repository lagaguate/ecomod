numbersandweights.4vsw<-function(){
  #DFO/4VSW Sentinel Survey Numbers and Weights
  #ported to ecomod Feb 21, 2015 (Mike McMahon)
  
   # CONCERNS ----------------------------------------------------------
  # This relies on some derived tables that were created for the VDC, rather than calculating
  # the information from the original data (eg isinf_4vsw_commercial, iscat_4vsw_commercial).  
  # This has the potential to be ugly as additional grants will be necessary so tha users can 
  # access everythingthat will be needed.

  #may need to tweak input parameters to narrow down selections available to user
  library(RODBC)
  chan<-odbcConnect(uid=oracle.vdc.user,pw=oracle.vdc.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
  
  selections<-populateselections()
  
  paramlist<-list()
  #settype needs to be a human readable (6=fishers choice; 10=commercial index)
  paramlist[[":bind__SetType"]]<-select.list(as.character(c(6,10)),title='Choose a set type:',multiple=F,graphics=T, preselect=as.character(c(6)))
  paramlist[[":bind__Area"]]<-select.list(as.character(c("4VB","4VS","4WD","4WE","4WF","4WG","4WH","4WJ","4WK","4WL","4WU")),title='Choose an area:',multiple=T,graphics=T)
  paramlist[[":bind__Species"]]<-select.list(as.character(selections$the.species[[2]]),title='Choose a species:',multiple=F,graphics=T,preselect="White Hake")
  paramlist[[":bind__Strata_forcequote"]]<-select.list(selections$the.strata,title='Choose strata:',multiple=T,graphics=T, preselect=as.character(c(440:495)))
  
  sselect<-"
  SELECT i.year, areas, sets, 
 NVL(total_num,0)/sets average_number, NVL(total_wgt,0)/sets average_weight
 FROM 
 (SELECT year, COUNT(DISTINCT nafarea_id) areas, COUNT(*) sets
 FROM isinf_4vsw_commercial 
 WHERE nafarea_id in (:bind__Area) 
 AND haulccd_id IN (1,2,3) 
 AND setcd_id IN (:bind__SetType)
 GROUP BY year) i, 
 (SELECT year, SUM(est_combined_wt*(1500/num_hook_haul)) total_wgt, 
 SUM(totno*(1500/num_hook_haul)) total_num 
 FROM iscat_4vsw_commercial
 WHERE num_hook_haul > 0 
 AND nafarea_id in (:bind__Area) 
 AND haulccd_id IN (1,2,3) 
 AND setcd_id IN (:bind__SetType)
 AND speccd_id IN (SELECT DISTINCT research FROM species_codes WHERE common=UPPER(:bind__Species)) 
 GROUP BY year) c 
 WHERE i.year=c.year(+)
 ORDER by 1
"
  sql<-binder(sselect, paramlist)
  df<-sqlQuery(chan,sql)
  close(chan)
  output<-list()
  output[["paramlist"]]<-paramlist
  output[["sql"]]<-sql
  output[["df"]]<-df
  return(output)
}

#loadfunctions("vdc.analytic.numbersandweights")
#4vsw<-numbersandweights.4vsw()
