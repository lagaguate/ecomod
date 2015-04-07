
zoop.db = function( DS, p ) {
 # data view of zooplakton from biochem db
  
  biochem.dir = project.directory("biochem") 
  biochem.data.dir = file.path( biochem.dir, "data" ) 
  biochem.datadump.dir = file.path( biochem.dir, "data", "datadump" ) 
  
  if ( DS == "zoop.data.dump.odbc" ) {
    if (FALSE) {
      
      "
      select
  m.mission_seq
, m.name
      , m.descriptor mission
      , substr(m.leader, 1, 15) leader
      , m.start_date mission_start
      , m.end_date mission_end
      , substr(m.institute, 1,10) institute
      , m.data_center_code
      , substr(m.protocol, 1, 15) protocol
      , substr(m.platform, 1, 14) platform
      , m.geographic_region
      , e.event_seq             event_seq
      , e.start_date            event_start
      , e.start_time            event_start_time
      , e.end_date              event_end
      , e.end_time              event_end_time
      , e.collector_station_name collector_station_name
      , e.collector_event_id
      , e.utc_offset            utc_offset
      , e.min_lat               event_min_lat
      , e.min_lon               event_Min_lon
      , e.max_lat               event_max_lat
      , e.max_lon               event_max_lon
      , h.plankton_seq
      , h.start_date header_start
      , h.start_time header_start_time
      , h.end_date header_end
      , h.end_time header_end_time
      , h.time_qc_code
      , h.start_lat             header_start_lat
      , h.start_lon             header_start_lon
      , h.end_lat               header_end_lat
      , h.end_lon               header_end_lon
      , h.position_qc_code
      , h.start_depth header_start_depth
      , h.end_depth header_end_depth
      , h.sounding
      , h.collector_sample_id
      , h.collector
      , h.phase_of_daylight
      , h.volume
      , h.large_plankton_removed
      , h.mesh_size
      , h.meters_sqd_flag
      , h.collector_deployment_id
      , v.name volume_method
      , ge.type gear_type
      , ge.model gear_model
      , g.plankton_general_seq
      , g.national_taxonomic_seq
      , n.taxonomic_name biochem_taxonname
      , g.modifier biochem_modifier
      
      , w.taxonname worms_taxonname
      , w.authority worms_authority
      , w.subkingdom
      , w.phylum
      , w.class
      , w.ordo
      , w.family
      , w.genus
      , w.subgenus
      , w.species
      , w.subspecies
      
      , n.authority biochem_authority
      , n.aphiaid
      , n.best_NODC7
      , n.TSN
      , n.TSN_ITIS
      , hi.name life_history
      , hi.molt_number
      , t.name trophic_desc
      , g.min_sieve
      , g.max_sieve
      , g.split_fraction
      , s.name sex
      , g.counts
      , g.count_pct
      , g.wet_weight
      , g.dry_weight
      , g.bio_volume
      , g.presence
      
      from biochem.bcmissions m
      , biochem.bcevents e
      , biochem.bcvolumemethods v
      , biochem.bcgears ge
      , biochem.bcplanktnhedrs h
      , biochem.bcnatnltaxoncodes n
      , biochem.bcplanktngenerals g
      , biochem.bclifehistories hi
      , biochem.bctrophicdescriptors t
      , biochem.bcsexes s
      , canrms.worms_matrix w 
      
      where 
      m.mission_seq = e.mission_seq
      and e.event_seq = h.event_seq
      and h.gear_seq = ge.gear_seq
      and h.plankton_seq = g.plankton_seq
      and h.volume_method_seq = v.volume_method_seq
      and g.life_history_seq = hi.life_history_seq
      and g.trophic_seq = t.trophic_seq
      and g.sex_seq = s.sex_seq
      and g.national_taxonomic_seq = n.national_taxonomic_seq
      and n.aphiaid=w.id(+)
      and (h.start_lat between 37 and 48 and h.start_lon between -71 and -48)
      
      "
    }
    
    
    require(RODBC)
    dir.create( biochem.datadump.dir, recursive=TRUE, showWarnings=FALSE )
    con = odbcConnect( dsn=oracle.biochem.server, uid=oracle.biochem.user, pwd=oracle.biochem.password, believeNRows=F)
        

    for ( YR  in p$zoop.years ) {
      query = paste( 
"     select
  m.mission_seq
, m.name
, m.descriptor mission
, substr(m.leader, 1, 15) leader
, m.start_date mission_start
, m.end_date mission_end
, substr(m.institute, 1,10) institute
, m.data_center_code
, substr(m.protocol, 1, 15) protocol
, substr(m.platform, 1, 14) platform
, m.geographic_region
, e.event_seq             event_seq
, e.start_date            event_start
, e.start_time            event_start_time
, e.end_date              event_end
, e.end_time              event_end_time
, e.collector_station_name collector_station_name
, e.collector_event_id
, e.utc_offset            utc_offset
, e.min_lat               event_min_lat
, e.min_lon               event_Min_lon
, e.max_lat               event_max_lat
, e.max_lon               event_max_lon
, h.plankton_seq
, h.start_date header_start
, h.start_time header_start_time
, h.end_date header_end
, h.end_time header_end_time
, h.time_qc_code
, h.start_lat             header_start_lat
, h.start_lon             header_start_lon
, h.end_lat               header_end_lat
, h.end_lon               header_end_lon
, h.position_qc_code
, h.start_depth header_start_depth
, h.end_depth header_end_depth
, h.sounding
, h.collector_sample_id
, h.collector
, h.phase_of_daylight
, h.volume
, h.large_plankton_removed
, h.mesh_size
, h.meters_sqd_flag
, h.collector_deployment_id
, v.name volume_method
, ge.type gear_type
, ge.model gear_model
, g.plankton_general_seq
, g.national_taxonomic_seq
, n.taxonomic_name biochem_taxonname
, g.modifier biochem_modifier

, w.taxonname worms_taxonname
, w.authority worms_authority
, w.subkingdom
, w.phylum
, w.class
, w.ordo
, w.family
, w.genus
, w.subgenus
, w.species
, w.subspecies

, n.authority biochem_authority
, n.aphiaid
, n.best_NODC7
, n.TSN
, n.TSN_ITIS
, hi.name life_history
, hi.molt_number
, t.name trophic_desc
, g.min_sieve
, g.max_sieve
, g.split_fraction
, s.name sex
, g.counts
, g.count_pct
, g.wet_weight
, g.dry_weight
, g.bio_volume
, g.presence

from biochem.bcmissions m
, biochem.bcevents e
, biochem.bcvolumemethods v
, biochem.bcgears ge
, biochem.bcplanktnhedrs h
, biochem.bcnatnltaxoncodes n
, biochem.bcplanktngenerals g
, biochem.bclifehistories hi
, biochem.bctrophicdescriptors t
, biochem.bcsexes s
, canrms.worms_matrix w 

where 
m.mission_seq = e.mission_seq
and e.event_seq = h.event_seq
and h.gear_seq = ge.gear_seq
and h.plankton_seq = g.plankton_seq
and h.volume_method_seq = v.volume_method_seq
and g.life_history_seq = hi.life_history_seq
and g.trophic_seq = t.trophic_seq
and g.sex_seq = s.sex_seq
and g.national_taxonomic_seq = n.national_taxonomic_seq
and n.aphiaid=w.id(+)
and (h.start_lat between 37 and 48 and h.start_lon between -71 and -48)

/* and to_number(to_char (h.start_date, 'YYYY')) 
BETWEEN to_number(to_char (m.end_date,'YYYY'))
and to_number(to_char (m.start_date,'YYYY'))
and to_char(e.start_date, 'DD-MON-YYYY') = to_char(h.start_date, 'DD-MON-YYYY') 
 */

and to_number(to_char (m.start_date,'YYYY')) =",  YR, ";" )

      fn = file.path( biochem.datadump.dir, paste( "zoop.data.dump.odbc", YR, ".rdata", sep="")  )  
      res = NULL
      res = sqlQuery(con, query )
      save(res, file=fn, compress=TRUE )
      print(YR)
    }
    
  }

  
  if ( DS == "zoop.data.odbc.all.redo" ) {
    fn.all = file.path( biochem.datadump.dir, paste( "zoop.data.dump.odbc", "all_years", "rdata", sep=".")  )  
    out = NULL
    for ( YR  in p$zoop.years ) {
      fn = file.path( biochem.datadump.dir, paste( "zoop.data.dump.odbc", YR, ".rdata", sep="")  )  
      res = NULL
      if (file.exists(fn)) load( fn )
      out = rbind( out, res )
    }
    save(out, file=fn.all, compress=TRUE)
  }
  
  if ( DS == "zoop.data.odbc.all" ) {
    fn.all = file.path( biochem.datadump.dir, paste( "zoop.data.dump.odbc", "all_years", "rdata", sep=".")  )  
    out= NULL
    if (file.exists(fn.all)) load(fn.all) 
    return( out)
  }
  
  if ( DS %in% c("zoop.qa.qc", "zoop.qa.qc.redo")  ) {
    fn = file.path( biochem.data.dir, paste( "zoop.data.qa.qc", ".rdata", sep="") )  
    if ( DS == "zoop.qa.qc" ) {
      out = NULL
      if (file.exists(fn)) load(fn)
      return(out)
    }
    
    zoo = zoop.db( DS="zoop.data.odbc.all", p=p ) 
    
    
    # data filtering in filter_zoo_final.r
    
    
    save(out, file=fn, compress=TRUE )
    return( "Completed filtering step" )
  
  
} 