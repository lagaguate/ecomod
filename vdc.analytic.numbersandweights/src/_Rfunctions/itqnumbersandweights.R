itqnumbersandweights<-function(){
  #DFO/ITQ Survey Numbers and Weights
  #ported to ecomod Feb 20, 2015 (Mike McMahon)
  #may need to tweak input paramters to narrow down selections available to user
  library(RODBC)
  chan<-odbcConnect(uid=oracle.vdc.user,pw=oracle.vdc.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
  selections<-populateselections()
  
  paramlist<-list()
  paramlist[[":bind__Species"]]<-select.list(as.character(selections$the.species[[2]]),title='Choose a species:',multiple=F,graphics=T,preselect="White Hake")
  paramlist[[":bind__Series"]]<-select.list(selections$the.series,title='Choose a series:',multiple=T,graphics=T, preselect=c('SUMMER','SUMMER_TELEOST'))
  paramlist[[":bind__Strata_forcequote"]]<-select.list(selections$the.strata,title='Choose strata:',multiple=T,graphics=T, preselect=as.character(c(440:495)))
  standardize<-select.list(c("Yes","No"),title='Standardize by Tow Dist?:',multiple=F,graphics=T,preselect="Yes")
  lenfreqonly<-select.list(c("Yes","No"),title='Only use catches with length frequency samples?',multiple=F,graphics=T,preselect="No")
  if (standardize=="Yes"){
    paramlist[[":bind__DistTowed_noquote"]]="*(1.0/d.distance)"
  }else{
    paramlist[[":bind__DistTowed_noquote"]]=" "
  }
  if(lenfreqonly=="Yes"){
  paramlist[[":bind__LenFreq_noquote"]]="catch_id IN (SELECT DISTINCT catch_id FROM islf_7051) AND "
}else{
  paramlist[[":bind__LenFreq_noquote"]]=" "
}
  sselect<-"
  SELECT i.year, 0 dummy, SUM(i.sets) sets, 
 ROUND(SUM(NVL(c.total_num,0))/SUM(i.sets),1) average_number, 
 ROUND(SUM(NVL(c.total_wgt,0))/SUM(i.sets),1) average_weight
 FROM 
 (SELECT year, COUNT(1) sets
 FROM mflib.isinf_7051 i
 WHERE haulccd_id IN (1,2,3)
 AND stratum_id IN (SELECT DISTINCT strat FROM groundfish.gsmgt WHERE unit IN (:bind__Strata_forcequote))
 GROUP BY year) i, 
 (SELECT c.year, 
   SUM(c.est_combined_wt :bind__DistTowed_noquote) total_wgt, 
 SUM(c.totno :bind__DistTowed_noquote) total_num 
 FROM mflib.iscat_7051 c, mflib.isinf_7051_calc d
 WHERE :bind__LenFreq_noquote 
 c.fishset_id=d.fishset_id AND c.haulccd_id IN (1,2,3) 
 AND c.speccd_id IN (SELECT DISTINCT research FROM species_codes WHERE common=:bind__Species)
 AND c.stratum_id IN (SELECT DISTINCT strat FROM groundfish.gsmgt WHERE unit IN (:bind__Strata_forcequote))
 GROUP BY c.year) c
 WHERE i.year=c.year(+)
 AND i.year >= 1996
 GROUP BY i.year	
 ORDER by 1 "
  data<-sqlQuery(chan,binder(sselect, paramlist))
  close(chan)
  return(data)
}

#itqnumbersandweights()
