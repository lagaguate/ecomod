PortSamples.get.data<-function(dsn, user, pw, last_n_days,startDate, endDate, vessel_list){
 options(stringsAsFactors=F)
 require(RODBC)
  channel<-connectAs(dsn, user,pw)  
  #capture last_n_days - if unspecified, default it to 1 day
  if(is.null(last_n_days)){
    last_n_days=1
  }else{
    last_n_days<-as.numeric(last_n_days)
  }
  
  #filter the data as requested
  if (!is.null(startDate) && !is.null(endDate)){
    #have start and end dates - get a range
    titlebit<- paste("between ",startDate," and ",endDate,sep="")
    where1=paste("and p.pdate BETWEEN to_date('",startDate,"','YYYY-MM-DD HH24:MI') AND to_date('",endDate,"','YYYY-MM-DD HH24:MI')
and v.start_date <= vdc.local_time(to_date('",endDate,"','YYYY-MM-DD HH24:MI'),'AST','ADT')
and v.end_date   >= vdc.local_time(to_date('",startDate,"','YYYY-MM-DD HH24:MI'),'AST','ADT')
and lc.start_date <= vdc.local_time(to_date('",endDate,"','YYYY-MM-DD HH24:MI'),'AST','ADT')
and lc.end_date   >= vdc.local_time(to_date('",startDate,"','YYYY-MM-DD HH24:MI'),'AST','ADT')",sep="")
  }else if (!is.null(startDate) || !is.null(endDate)){
    #have a single date - get last_n_days prior to this date
    thedate<-c(startDate,endDate)
    titlebit<- paste("for the ",last_n_days," day(s) prior to ", thedate,sep="")
  #MMM 2013
#Jerry's SQL didn't seem to have a way to get x days prior to a date, so I altered the date range code to meet the need
#--and p.pdate >= vdc.utc_time(to_date('",thedate,"','YYYY-MM-DD HH24:MI')-",last_n_days,"+1,'AST','ADT')
where1=paste("
and p.pdate BETWEEN vdc.utc_time(to_date('",thedate,"','YYYY-MM-DD HH24:MI'),'AST','ADT')-",last_n_days," AND vdc.utc_time(to_date('",thedate,"','YYYY-MM-DD HH24:MI'),'AST','ADT')
and v.end_date   >= to_date('",thedate,"','YYYY-MM-DD HH24:MI')-",last_n_days,"
and v.start_date <= to_date('",thedate,"','YYYY-MM-DD HH24:MI')
and lc.end_date   >= to_date('",thedate,"','YYYY-MM-DD HH24:MI')-",last_n_days,"
and lc.start_date <= to_date('",thedate,"','YYYY-MM-DD HH24:MI')",sep="")
  }else{
    #no dates, so just get last_n_days prior to now 
    titlebit<- paste("for the last ", last_n_days," day(s)",sep="")
    where1=paste("and p.pdate >= vdc.utc_time(trunc(sysdate-to_number(",last_n_days,")+1),'AST','ADT')
and v.start_date <= sysdate-(to_number(",last_n_days,")-to_number(",last_n_days,")) 
and v.end_date   >= trunc(sysdate-to_number(",last_n_days,")) 
and lc.start_date <= sysdate-(to_number(",last_n_days,")-to_number(",last_n_days,"))
and lc.end_date   >= trunc(sysdate-to_number(",last_n_days,"))",sep="")
  }
  
  #if a vrn has been provided, add it to the SQL, modify title
  if (!is.null(vessel_list)){
    vessel_list<-paste(vessel_list,collapse=", ")
    titlebit<- c(titlebit,paste(" (for VRNS:",vessel_list,")",sep=""))
    where1= paste(where1," and p.vrn IN (",vessel_list,")",sep="")
  }

  #stole SQL from VDC.MWQUERY.DATA (ID=4191) 
  #removed time, date and speed range options
  the.SQL <-paste("
SELECT /*+ USE_HASH(L,LD,HO) ORDERED */
  /* Select Groundfish ITQ MG < 65.  Created: 2006/06/22 J.Black */
  /* Modified 2011/01/19 J.Black - to remove the restriction to license year = 2006 */
  /* changed 9497 to 8497 ITQ MG ABORIGINAL 2012/01/17 */
  /* 2013/12/16 M.McMahon - modified to remove time, date, and speed options
      removed FOIP link, field with 'v' appended to vrn
     -- generally simplified query for use in kml routine*/
  rownum setid, 
  p.lon, 
  p.lat, 
  vn.vessel_name, 
  p.vrn, 
  p.pdate VMSDATE,
  to_char(p.pdate,'YYYY/MM/DD HH24:MI') vmstime,
  p.hailout, 
  initcap(c.desc_eng) fleet,
  p.speed_knots
FROM mfd_obfmi.vms_pos p,
  mfd_obfmi.marfis_licence_vessels_syn v,
  mfd_obfmi.marfis_condition_lic_assign lc,
  mfd_obfmi.marfis_vessels_syn vn,
  mfd_obfmi.marfis_conditions c,
  mfd_obfmi.marfis_pfisp_hail_outs ho,
  mfd_obfmi.marfis_pfisp_hail_out_lic_docs ld,
mfd_obfmi.marfis_licences_syn l
WHERE 
  p.vrn = v.vr_number
  and p.vrn = vn.vr_number
  and v.licence_id = lc.licence_id 
  and lc.condition_id = c.condition_id
  and p.hailauth = ho.conf_number
  and ho.hail_out_id = ld.hail_out_id
  and ld.licence_id = l.licence_id
  and l.species_code = c.species_code
  and lc.condition_id in (8495,8497) 
  /* Filters from above */
  ",where1,"
  and p.lon < 181 and p.lon > -75
  and p.lat < 91
  order by p.vrn, p.pdate
;",sep="")
  vertexFields = c("LON","LAT","VMSTIME","SPEED_KNOTS")
  #cat(the.SQL)
  df<-sqlQuery(channel,the.SQL)
  df<-list(df[with(df, order(VRN, VMSDATE)), ],vertexFields,the.SQL,last_n_days, vessel_list, titlebit)
  dfKeep<<-df
  odbcClose(channel)
  return(df)
}