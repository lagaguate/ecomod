numbersandweights.4vnsentinel<-function(){
  #DFO/4VN Sentinel Survey Numbers and Weights
  #ported to ecomod Feb 21, 2015 (Mike McMahon)
  
   # CONCERNS ----------------------------------------------------------
  # This relies on some derived tables that were created for the VDC, rather than calculating
  # the information from the original data (eg isinf_4vn_sentinel, iscat_4vn_sentinel).  
  # This has the potential to be ugly as additional grants will be necessary so tha users can 
  # access everythingthat will be needed.

  #may need to tweak input parameters to narrow down selections available to user
  library(RODBC)
  chan<-odbcConnect(uid=oracle.vdc.user,pw=oracle.vdc.password,dsn=oracle.dsn,case='nochange',rows_at_time=1)
  
  selections<-populateselections()
  
  paramlist<-list()
  #settype needs to be a human readable (4=survey-fixed;5=survey-random;6=fishers choice;9=experiental;10=commercial index)
  paramlist[[":bind__SetType"]]<-select.list(as.character(c(4,5,6,9,10)),title='Choose a set type:',multiple=F,graphics=T, preselect=as.character(c(4)))
  paramlist[[":bind__Species"]]<-select.list(as.character(selections$the.species[[2]]),title='Choose a species:',multiple=F,graphics=T,preselect="White Hake")
  
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
                     WHERE common=:bind__Species) 
         GROUP BY year) c 
  WHERE i.year=c.year(+)
 ORDER by 1
"
  df<-sqlQuery(chan,binder(sselect, paramlist))
  close(chan)
  output<-list()
  output[["paramlist"]]<-paramlist
  output[["df"]]<-df
  return(output)
}

#loadfunctions("vdc.analytic.numbersandweights")
#test<-numbersandweights.4vnsentinel()
