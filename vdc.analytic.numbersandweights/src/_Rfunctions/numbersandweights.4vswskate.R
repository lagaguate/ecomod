numbersandweights.4vswskate<-function(){
  #DFO/4VSW Skate Survey Numbers and Weights
  #ported to ecomod Feb 21, 2015 (Mike McMahon)
  
  #may need to tweak input parameters to narrow down selections available to user
  library(RODBC)
  chan<-odbcConnect(uid=oracle.vdc.user,pw=oracle.vdc.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
  
  selections<-populateselections()
  
  paramlist<-list()
  #settype needs to be a human readable (1=commercial;6=fishers choice;9=experiental)
  paramlist[[":bind__SetType"]]<-select.list(as.character(c(1,6,9)),title='Choose a set type:',multiple=F,graphics=T, preselect=as.character(c(1)))
  paramlist[[":bind__Area"]]<-select.list(as.character(c("4VC","4VS","4WD","4WE","4WF","4WG","4WH","4WJ")),title='Choose an area:',multiple=T,graphics=T)
  paramlist[[":bind__Species"]]<-select.list(as.character(selections$the.species[[2]]),title='Choose a species:',multiple=F,graphics=T,preselect="White Hake")
  standardize<-select.list(c("Yes","No"),title='Standardize by Tow Dist?:',multiple=F,graphics=T,preselect="Yes")
  if (standardize=="Yes"){
    paramlist[[":bind__DistTowed_noquote"]]="*(1.0/d.distance)"
  }else{
    paramlist[[":bind__DistTowed_noquote"]]=" "
  }
  
  sselect<-"
SELECT  i.year, 1 areas, sets,  
        NVL(total_num,0)/sets average_number, NVL(total_wgt,0)/sets average_weight
   FROM 
        (SELECT year, COUNT(*) sets
           FROM isinf_4vn_sentinel 
               WHERE haulccd_id IN (1,2,3) 
                 AND setcd_id IN (:bind__SetType)
          GROUP BY year) i, 
        (SELECT year, SUM(est_combined_wt*(1500/num_hook_haul)) total_wgt, 
                SUM(est_num_caught*(1500/num_hook_haul)) total_num 
           FROM iscat_4vn_sentinel
               WHERE num_hook_haul > 0 
                 AND haulccd_id IN (1,2,3) 
                 AND setcd_id IN (:bind__SetType)
                 AND speccd_id IN 
                   (SELECT DISTINCT research 
                      FROM species_codes 
                     WHERE common=UPPER(:bind__Species)) 
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
#test<-numbersandweights.4vswskate()
