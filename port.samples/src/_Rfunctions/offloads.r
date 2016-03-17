offloads<-function(user=-1,password=-1){
#print("Hail Ins that haven't offloaded yet") #VDC ID= 4215
  #Map Tracks by vessel not yet implemented
  #links to FOIP all broken
  if(!require(RODBC)) { 
    install.packages('RODBC',repos="http://cran.r-project.org")
  }
  cancelmsg="Cancelled by user"
  #prompt user for credentials several times
  connect <- function(user, password){
    user=ifelse(exists("oracle.personal.username"), oracle.personal.username,-1)
    password=ifelse(exists("oracle.personal.password"), oracle.personal.password,-1)
    try=0
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
      channel<<-channel
      return(channel)
    }
  } 
  
  channel<-connect(user, password)
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
analytics = c("Hail Ins that haven't offloaded yet","Hail Outs, Last X Days")
 analytic =  select.list(analytics,
              multiple = F, graphics = T,
              title = "What do you want?")
 if (analytic==""){
   stop(return(print(cancelmsg)))
 }

 if (analytic == analytics[1]){
query=paste0("select 
             /* Generates a hail in / hail out report.  Last modified: 2006/06/20 J.Black */
               hi.hail_in_call_id, 
             min(to_char(ho.conf_number)) conf_no, 
             to_char(ho.conf_issued_date_time,'YYYY/MM/DD HH24:MI') conf_issued, 
             to_char(ho.sailed_date_time,'YYYY/MM/DD HH24:MI') sailed, 
             ho.vr_number vr_number,
             v.vessel_name, 
             c.community_name, 
             ld.licence_id, 
             s.desc_eng Species, 
             a.desc_eng area, 
             to_char(ho.est_landed_date,'YYYY/MM/DD') est_landed_date, 
             to_char(hl.est_landing_date_time,'YYYY/MM/DD HH24:MI') landing_date,
             decode(sign(hl.est_offload_date_time-sysdate),
                    1,to_char(hl.est_offload_date_time,'YYYY/MM/DD HH24:MI'), 
                    to_char(hl.est_offload_date_time,'YYYY/MM/DD HH24:MI')) offload_date, 
             hi.observer_flag obs
             from 
             mfd_obfmi.marfis_pfisp_hail_outs ho,
             mfd_obfmi.marfis_pfisp_hail_out_lic_docs ld,
             mfd_obfmi.marfis_licences_syn l,
             mfd_obfmi.marfis_communities_syn c,
             mfd_obfmi.marfis_vessels_syn v,
             mfd_obfmi.marfis_species_syn s,
             mfd_obfmi.marfis_licence_areas_syn la,
             mfd_obfmi.areas a,
             mfd_obfmi.marfis_pfisp_hail_in_calls hi,
             mfd_obfmi.marfis_pfisp_hail_in_landings hl
             where ho.hail_out_id = ld.hail_out_id
             and ld.licence_id = l.licence_id
             and ho.community_code = c.community_code
             and ho.vr_number = v.vr_number 
             and l.species_code = s.species_code
             and l.licence_id = la.licence_id
             and ho.area_id = a.area_id
             and ho.hail_out_id = hi.hail_out_id(+)
             and hi.hail_in_call_id = hl.hail_in_call_id(+)
             and hl.est_offload_date_time > sysdate
             group by
             to_char(ho.conf_issued_date_time,'YYYY/MM/DD HH24:MI'),
             to_char(ho.sailed_date_time,'YYYY/MM/DD HH24:MI'),
             ho.vr_number,
             v.vessel_name, 
             c.community_name, 
             ld.licence_id, 
             s.desc_eng, 
             a.desc_eng, 
             to_char(ho.est_landed_date,'YYYY/MM/DD'), 
             hl.hail_in_landing_id,
             hi.hail_in_call_id, 
             to_char(hl.est_landing_date_time,'YYYY/MM/DD HH24:MI'),
             decode(sign(hl.est_offload_date_time-sysdate),
                    1,to_char(hl.est_offload_date_time,'YYYY/MM/DD HH24:MI'), 
                    to_char(hl.est_offload_date_time,'YYYY/MM/DD HH24:MI')), 
             hi.observer_flag
             order by conf_no")
data=sqlQuery(channel,query)
print(data)
}else {

  ndays = select.list(as.character(seq(1,20,1)),
                            multiple = F, graphics = T,
                            title = "How many days of offloads do you want?", preselect="7")
  ndays = as.numeric(ndays)
  if (is.na(ndays)){
    stop(return(print(cancelmsg)))
  }
  query= paste0("SELECT
  /* hail in / hail out report.  2006/07/19 J.Black */
  -- MIN(TO_CHAR(ho.conf_number)) conf_no,
  TO_CHAR(ho.conf_issued_date_time,'YYYY/MM/DD HH24:MI') conf_issued,
  TO_CHAR(ho.sailed_date_time,'YYYY/MM/DD HH24:MI') sailed,
  ho.vr_number,
  v.vessel_name,
  c.community_name,
  ld.licence_id,
  s.desc_eng Species,
  a.desc_eng area,
  TO_CHAR(ho.est_landed_date,'YYYY/MM/DD') est_landed_date,
  hl.hail_in_landing_id,
  hi.hail_in_call_id,
  TO_CHAR(hl.est_landing_date_time,'YYYY/MM/DD HH24:MI') landing_date,
  TO_CHAR(hl.est_offload_date_time,'YYYY/MM/DD HH24:MI') offload_date,
  DECODE(SIGN(hl.est_offload_date_time-sysdate), 1, 'Imminent', DECODE(SIGN(TRUNC(hl.est_offload_date_time)-TRUNC(sysdate)), 0, 'Just Landed', '') ) offload_flag,  
hi.observer_flag obs
  FROM mfd_obfmi.marfis_pfisp_hail_outs ho,
  mfd_obfmi.marfis_pfisp_hail_out_lic_docs ld,
  mfd_obfmi.marfis_licences_syn l,
  mfd_obfmi.marfis_communities_syn c,
  mfd_obfmi.marfis_vessels_syn v,
  mfd_obfmi.marfis_species_syn s,
  mfd_obfmi.marfis_licence_areas_syn la,
  mfd_obfmi.areas a,
  mfd_obfmi.marfis_pfisp_hail_in_calls hi,
  mfd_obfmi.marfis_pfisp_hail_in_landings hl
  WHERE ho.hail_out_id     = ld.hail_out_id
  AND ld.licence_id        = l.licence_id
  AND ho.community_code    = c.community_code
  AND ho.vr_number         = v.vr_number
  AND l.species_code       = s.species_code
  AND l.licence_id         = la.licence_id(+)
  AND ho.area_id           = a.area_id
  AND ho.hail_out_id       = hi.hail_out_id(+)
  AND hi.hail_in_call_id   = hl.hail_in_call_id(+)
  AND ho.sailed_date_time >= TRUNC(sysdate-to_number(",ndays,")+1)
/* #   GROUP BY TO_CHAR(ho.sailed_date_time,'YYYY/MM/DD HH24:MI'),
#   TO_CHAR(ho.conf_issued_date_time,'YYYY/MM/DD HH24:MI'),
#   ho.vr_number,
#   v.vessel_name,
#   c.community_name,
#   ld.licence_id,
#   s.desc_eng,
#   a.desc_eng,
#   TO_CHAR(ho.est_landed_date,'YYYY/MM/DD'),
#   hl.hail_in_landing_id,
#   hi.hail_in_call_id,
#   TO_CHAR(hl.est_landing_date_time,'YYYY/MM/DD HH24:MI'),
#   TO_CHAR(hl.est_offload_date_time,'YYYY/MM/DD HH24:MI'),
#   DECODE(SIGN(hl.est_offload_date_time-sysdate), 1, 'Imminent', DECODE(SIGN(TRUNC(hl.est_offload_date_time)-TRUNC(sysdate)), 0, 'Just Landed', '') ),  
#   hi.observer_flag */
  ORDER BY 
  TO_CHAR(hl.est_offload_date_time,'YYYY/MM/DD HH24:MI')")
  data=sqlQuery(channel,query)
  print(data)
}

  the.hail.ins = paste0(data$VESSEL_NAME, " (",data$HAIL_IN_CALL_ID,")")
  get.hail.in = select.list(the.hail.ins,
                            multiple = F, graphics = T,
                            title = "Which hail-in do you want details about?")
  get.hail.in = as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', get.hail.in))
  if (is.na(get.hail.in)){
    stop(return(print(cancelmsg)))
  }
  det.query = paste0("SELECT 
                           L.HAIL_IN_CALL_ID,
                           C.CONF_NUMBER,
                           L.HAIL_IN_LANDING_ID,
                           O.HAIL_IN_ONBOARD_ID,
                           WHARVES.WHARF_NAME,
                           O.VR_NUMBER,
                           HAIL_IN_TYPES.DESC_ENG,
                           NAFO_UNIT_AREAS.AREA NAFO,
                           SPECIES.DESC_ENG,
                           SPECIE_SIZE_FORMS.DESC_ENG,
                           SPECIE_SIZE_FORMS.SPECIE_FORM_DESC_ENG FORM,
                           O.EST_ONBOARD_WEIGHT,
                           O.OFFLOAD_WEIGHT,
                           UNIT_OF_MEASURES.DESC_ENG
                     FROM 
                           MARFISSCI.HAIL_IN_LANDINGS L,
                           MARFISSCI.HAIL_IN_CALLS C,
                           MARFISSCI.HAIL_IN_ONBOARD O,
                           MARFISSCI.HAIL_IN_TYPES,
                           MARFISSCI.COMMUNITIES,
                           MARFISSCI.WHARVES,
                           MARFISSCI.SPECIES,
                           MARFISSCI.UNIT_OF_MEASURES,
                           MARFISSCI.SPECIE_SIZE_FORMS,
                           MARFISSCI.NAFO_UNIT_AREAS,
                           MARFISSCI.AREAS,
                           MARFISSCI.VESSELS
                     WHERE 
                           L.HAIL_IN_CALL_ID    = ",get.hail.in,"
                           AND L.HAIL_IN_CALL_ID      = C.HAIL_IN_CALL_ID
                           AND L.HAIL_IN_LANDING_ID   = O.HAIL_IN_LANDING_ID
                           AND C.HAIL_IN_TYPE_ID      = HAIL_IN_TYPES.HAIL_IN_TYPE_ID
                           AND L.COMMUNITY_CODE       = COMMUNITIES.COMMUNITY_CODE
                           AND O.SSF_SPECIES_CODE     = SPECIES.SPECIES_CODE
                           AND O.UNIT_OF_MEASURE_ID   = UNIT_OF_MEASURES.UNIT_OF_MEASURE_ID
                           AND L.WHARF_ID             = WHARVES.WHARF_ID
                           AND O.SSF_LANDED_FORM_CODE = SPECIE_SIZE_FORMS.LANDED_FORM_CODE
                           AND O.SSF_SPECIES_CODE     = SPECIE_SIZE_FORMS.SPECIES_CODE
                           AND O.NAFO_UNIT_AREA_ID    = NAFO_UNIT_AREAS.AREA_ID
                           AND O.FISHING_AREA_ID      = AREAS.AREA_ID
                           AND O.VR_NUMBER            = VESSELS.VR_NUMBER
                     ")
  det.data=sqlQuery(channel,det.query)
  print(det.data)
  # }
odbcClose(channel)
}