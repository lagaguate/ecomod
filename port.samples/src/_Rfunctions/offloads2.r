#################################################
#Change the number below to the number of days you want to go back
ndays=10
handyFolder = "C:/Users/mcmahonm/Documents/GitHub/ecomod_data/port.samples/"
#################################################


library(RODBC)
library(xlsx)
connect.ptran = function(user=NULL, password=NULL, i=0){
  if (i<5){
    channel = tryCatch( 
      { 
        odbcConnect(uid = user, pw= password, dsn='PTRAN', case='nochange', rows_at_time=1)
        connected=T
        odbcConnect(uid = user, pw= password, dsn='PTRAN', case='nochange', rows_at_time=1);
      },
      warning=function(e) {
        message("Error - Unable to connect - please try again")
        user <- readline(prompt="Enter Username: ")
        password <- readline(prompt="Enter Password: ")
        i=i+1
        connect.ptran(user,password, i)
      }
    )
  }else{
    writeLines("Please check your credentials - you don't seem to be able to connect")
    channel = NULL
  }
  return(channel)
}

hail.in<-function(channel){
  writeLines("Running 'Hail Ins that haven't offloaded yet'")
  query=paste0("SELECT 
  DECODE(SIGN(hl.EST_OFFLOAD_DATE_TIME - SysDate), 1, 'Imminent', 
         DECODE(SIGN(TRUNC(hl.EST_OFFLOAD_DATE_TIME) - TRUNC(SysDate)), 0, 'Just Landed', 'Been here a while')) STATUS,
  c.community_name community,
  v.vessel_name || ' (' || hi.VR_NUMBER || ')' vessel,
  NA.AREA NAFO,
  regexp_replace(a.desc_eng, 'NAFO UNIT AREA - |NAFO DIVISION -','') area,
  s.desc_eng SPEC,
  ssf.specie_form_desc_eng form,
  O.OFFLOAD_WEIGHT || ' ' ||U.DESC_ENG EST_WGT,
  hl.MONITOR_REQ_FLAG MONITOR_REQ,
  hi.OBSERVER_FLAG OBSERVER,
  hi.conf_number ||'(' || to_char(ho.conf_issued_date_time,'YYYY-MM-DD HH24:MI')|| ')' conf_number_OUT,
  to_char(ho.SAILED_DATE_TIME, 'YYYY-MM-DD HH24:MI') sailed_time_OUT,
  to_char(ho.EST_LANDED_DATE, 'YYYY-MM-DD') landed_date_OUT,
  ho.conf_number ||'(' || to_char(hi.conf_issued_date_time,'YYYY-MM-DD HH24:MI')|| ')' conf_number_IN,
  to_char(hl.EST_LANDING_DATE_TIME, 'YYYY-MM-DD HH24:MI') landing_time_IN,
  to_char(hl.EST_OFFLOAD_DATE_TIME, 'YYYY-MM-DD HH24:MI') offload_time_IN,
  l.licence_id
  FROM MARFISSCI.HAIL_OUTS ho, MARFISSCI.HAIL_IN_LANDINGS hl, MARFISSCI.HAIL_IN_CALLS hi, MARFISSCI.VESSELS v, 
  MARFISSCI.licences l, MARFISSCI.hail_out_lic_docs ld, MARFISSCI.species s, MARFISSCI.areas a, MARFISSCI.communities c,
  MARFISSCI.HAIL_IN_ONBOARD O, MARFISSCI.NAFO_UNIT_AREAS na,MARFISSCI.SPECIE_SIZE_FORMS SSF, MARFISSCI.UNIT_OF_MEASURES U
  WHERE
  hi.HAIL_IN_CALL_ID  = hl.HAIL_IN_CALL_ID AND
  hi.vr_number        = v.vr_number(+) AND
  ho.hail_out_id      = hi.hail_out_id(+) AND
  hi.hail_in_call_id  = hl.hail_in_call_id(+) AND
  ho.hail_out_id         = ld.hail_out_id AND 
  ld.licence_id            = l.licence_id AND 
  l.species_code           = s.species_code AND
  o.fishing_area_id               = a.area_id AND
  ho.community_code        = c.community_code AND
  hl.HAIL_IN_LANDING_ID   = O.HAIL_IN_LANDING_ID AND
  O.SSF_SPECIES_CODE     = S.SPECIES_CODE AND
  O.UNIT_OF_MEASURE_ID   = U.UNIT_OF_MEASURE_ID AND
  O.SSF_LANDED_FORM_CODE = ssf.LANDED_FORM_CODE AND
  O.SSF_SPECIES_CODE     = ssf.SPECIES_CODE AND
  o.ssf_species_size_code     = ssf.SPECIES_SIZE_CODE AND
  O.NAFO_UNIT_AREA_ID    = NA.AREA_ID AND
  O.VR_NUMBER            = V.VR_NUMBER AND
  hl.est_offload_date_time > sysdate
  ORDER BY hl.EST_OFFLOAD_DATE_TIME DESC, hi.conf_number;")
  
  hail.in.data=sqlQuery(channel,query)
  return(hail.in.data)
}

hail.inout<-function(channel, ndays){
  writeLines(paste0("Running 'Hail In/Hail Out' Report for the last ",ndays," days"))
  query=paste0("SELECT 
  DECODE(SIGN(hl.EST_OFFLOAD_DATE_TIME - SysDate), 1, 'Imminent', 
               DECODE(SIGN(TRUNC(hl.EST_OFFLOAD_DATE_TIME) - TRUNC(SysDate)), 0, 'Just Landed', 'Been here a while')) STATUS,
               c.community_name community,
               v.vessel_name || ' (' || hi.VR_NUMBER || ')' vessel,
               NA.AREA NAFO,
               regexp_replace(a.desc_eng, 'NAFO UNIT AREA - |NAFO DIVISION -','') area,
               s.desc_eng SPEC,
               ssf.specie_form_desc_eng form,
               O.OFFLOAD_WEIGHT || ' ' ||U.DESC_ENG EST_WGT,
               hl.MONITOR_REQ_FLAG MONITOR_REQ,
               hi.OBSERVER_FLAG OBSERVER,
               hi.conf_number ||'(' || to_char(ho.conf_issued_date_time,'YYYY-MM-DD HH24:MI')|| ')' conf_number_OUT,
               to_char(ho.SAILED_DATE_TIME, 'YYYY-MM-DD HH24:MI') sailed_time_OUT,
               to_char(ho.EST_LANDED_DATE, 'YYYY-MM-DD') landed_date_OUT,
               ho.conf_number ||'(' || to_char(hi.conf_issued_date_time,'YYYY-MM-DD HH24:MI')|| ')' conf_number_IN,
               to_char(hl.EST_LANDING_DATE_TIME, 'YYYY-MM-DD HH24:MI') landing_time_IN,
               to_char(hl.EST_OFFLOAD_DATE_TIME, 'YYYY-MM-DD HH24:MI') offload_time_IN,
               l.licence_id
               FROM MARFISSCI.HAIL_OUTS ho, MARFISSCI.HAIL_IN_LANDINGS hl, MARFISSCI.HAIL_IN_CALLS hi, MARFISSCI.VESSELS v, 
               MARFISSCI.licences l, MARFISSCI.hail_out_lic_docs ld, MARFISSCI.species s, MARFISSCI.areas a, MARFISSCI.communities c,
               MARFISSCI.HAIL_IN_ONBOARD O, MARFISSCI.NAFO_UNIT_AREAS na,MARFISSCI.SPECIE_SIZE_FORMS SSF, MARFISSCI.UNIT_OF_MEASURES U
               WHERE
               hi.HAIL_IN_CALL_ID  = hl.HAIL_IN_CALL_ID AND
               hi.vr_number        = v.vr_number(+) AND
               ho.hail_out_id      = hi.hail_out_id(+) AND
               hi.hail_in_call_id  = hl.hail_in_call_id(+) AND
               ho.hail_out_id         = ld.hail_out_id AND 
               ld.licence_id            = l.licence_id AND 
               l.species_code           = s.species_code AND
               o.fishing_area_id               = a.area_id AND
               ho.community_code        = c.community_code AND
               hl.HAIL_IN_LANDING_ID   = O.HAIL_IN_LANDING_ID AND
               O.SSF_SPECIES_CODE     = S.SPECIES_CODE AND
               O.UNIT_OF_MEASURE_ID   = U.UNIT_OF_MEASURE_ID AND
               O.SSF_LANDED_FORM_CODE = ssf.LANDED_FORM_CODE AND
               O.SSF_SPECIES_CODE     = ssf.SPECIES_CODE AND
               o.ssf_species_size_code     = ssf.SPECIES_SIZE_CODE AND
               O.NAFO_UNIT_AREA_ID    = NA.AREA_ID AND
               O.VR_NUMBER            = V.VR_NUMBER AND
               ho.sailed_date_time >= TRUNC(sysdate-to_number(",ndays,")+1)
               ORDER BY TO_CHAR(hl.est_offload_date_time,'YYYY/MM/DD HH24:MI');")
  hail.inout.data=sqlQuery(channel,query)
  out=list(hail.inout.data,ndays)
  return(out)
}
dateString = strftime(Sys.time(),"%Y%m%d_%H%M%S")

hail.in.data = hail.in(connect.ptran(oracle.personal.username,oracle.personal.password))
hail.in.filename = paste0(handyFolder,"hailInsNotYetOffloaded_",dateString,".xlsx")
write.xlsx(hail.in.data, hail.in.filename)
writeLines(paste0("Wrote file to ", hail.in.filename))

hail.inout.data = hail.inout(connect.ptran(oracle.personal.username,oracle.personal.password), ndays)
hail.inout.filename = paste0(handyFolder,"hailInHailOutReport_",hail.inout.data[2],"days_",dateString,".xlsx")
write.xlsx(hail.inout.data[1], hail.inout.filename)
writeLines(paste0("Wrote file to ", hail.inout.filename))
