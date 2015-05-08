
zoop.db = function( DS, p ) {
 # Gordana Lazin, April 8, 2015
 # data view of zooplakton from biochem db
  
  biochem.dir = project.datadirectory("biochem") 
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
      ;
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
      df = NULL
      if (file.exists(fn)) load(fn)
      return(df)
    }
    
    zoo = zoop.db( DS="zoop.data.odbc.all", p=p ) 
    
    
    # data filtering in filter_zoo_final.r
    # Make headers lower case
    zoonames = tolower(names(zoo)) 
    names(zoo) = zoonames
    
    # ===== Step 1 =====
    # Data filtering and selection of data that follows AZMP protocol
    
    
    # Delete mesh size=0, that will remove phytoplankton records
    msz=which(zoo$mesh_size==0)
    if (length(msz)>0) {zoo=zoo[-msz,]}
    
    
    # Remove records with flagged position and time (qc_codes 2,3 and 4)
    qc=which(zoo$position_qc_code %in% c(2,3,4) | zoo$time_qc_code %in% c(2,3,4))
    if (length(qc)>0) {zoo=zoo[-qc,]}
    
    # Standardize station names: remove underscores and zeros (i.e. replace HL_02  with HL2)
    zoo$collector_station_name=gsub("_0",c(""), zoo$collector_station_name)
    
    # Remove "database artifacts" from biochem_taxonname field
    art=grep("database artifact",zoo$biochem_taxonname)
    if (length(art)>0) {zoo=zoo[-art,]}
    
    
    # Missions that follow AZMP zooplankton protocol:
    
    AZMPspring=c('18HU99005','18PZ00002','18HU01009','2002916','18HU03005',
                 '18HU04009','18NE05004','18HU06008','18HU07001','18HU08004',
                 '18HU09005','18HU10006','18HU11004','18HU13004','18HU14004')
    
    AZMPfall=c('18HU99054','18HU00050','18HU01061','18HU02064','18HU03067',
               '18HU04055','18HU05055','18HU06052','18HU07045','18HU08037',
               '18HU09048','18HU11043','18HU12042','18HU13037','18HU14030')
    
    Groundfish_winter=c('18NE00065','18NE00066','18NE01003','18NE01004','18NE02002',
                        '18NE02003','18NE03002','18NE03003','181C04004','18TL05545',
                        '18TL05546','18TL06614','18TL06615','181C07685','181C07686',
                        '181C08775','18TL08805','18NE09841','18NE09002','18NE10001',
                        '18NE10002','18NE11002','18NE12002','18NE13002','18NE14002',
                        '18NE14101')
    
    Groundfish_summer=c('18NE99025','18NE99029','18NE00026','18NE00031','18NE01032',
                        '18NE01037','18NE02037','18NE02040','18NE03036','18NE03042',
                        '18TL04529','18TL04530','18TL05605','18TL05633','18NE06030',
                        '18NE06036','18TL07745','181C08830','18NE09027','18NE10027',
                        '18NE11025','18NE12022','18NE13022','18NE14018')
    
    HL2=c('18VA99666','18VA00666','18VA01666','18VA02666','18VA03666','18VA04666',
          '18VA05666','18VA06666','18VA07666','18VA08666','18VA09666','18VA10666',  
          '18VA11666','18VA12666','18VA13666')
    
    
    Prince5=c('18VA99669','18VA00669','18VA01669','18VA02669','18VA03669',
              '18VA04669','18VA05669','18VA06669','18VA07669','18VA08669',
              '18VA09669','18VA10669','18VA11669','18VA12669','18VA13669')
    
    labSea=c("18HU00009","18HU01022","18HU02032","18HU02075","18HU03038",
             "18HU04016","18HU04019","18HU05016","18HU06019","18HU07011","18HU08009",
             "18HU09015", "18HU10014","18HU11009","18MF12001","18HU13008")
    
    IML=c('18HU10070')
    
    BOF=c("18HU13013")
    
    # On Lab Sea missions only stations on the Scotian Shelf were collected with AZMP protocol.
    # The rest of the samples were collected with AZOMP protocol. 
    # Lab see samples not tagged as AZMP protocols but with AZMP in "collector" field :
    # "18MF12001","18HU13008", "18HU06019", "18HU09015", "18HU10014", "18HU11009" 
    # Those are most likely AZMP protocol samples and are included in the dataset.
    # IML mission '18HU10070' collected data for maritimes just on HL line
    
    
    # Define appropriate gear model and mesh size
    goodGear=c("Ring net 0.75m xbow","Ring net 0.75m")
    
    extraGear=c("Ring net 0.75m o/c","Ring net 0.5m","Ring net 0.5m o/c",
                "BIONESS 0.5m","BIONESS 1.0m","HydroBIOS")
    meshSize=170:250
    
    
    # Find stations on Scotian Shelf collected on Lab Sea missions
    lh=which(zoo$mission %in% c(labSea,IML)
             & zoo$gear_model %in% goodGear
             & zoo$mesh_size %in% meshSize
             & zoo$header_start_lat>= 42 & zoo$header_start_lat<=46
             & zoo$header_start_lon>=-66 & zoo$header_start_lon<=-59 )
    
    
    AZMP_protocol_missions=c(AZMPspring,AZMPfall,
                             Groundfish_winter,Groundfish_summer,
                             HL2,Prince5,BOF)
    
    
    
    # Find records that follow AZMP zooplankton protocol and have appropriate gear model
    i=which(toupper(zoo$mission) %in% AZMP_protocol_missions 
            & zoo$gear_model %in% goodGear
            & zoo$mesh_size %in% meshSize)
    
    
    # Zooplankton data that follow AZMP protocol, includes Lab sea missions
    azmp=unique(c(i,lh))
    AZMPzoo=zoo[azmp,]
    
    
    # Benoit's Level1 processing: quality control checks
    df=AZMPzoo
    
    # Check if the samples have same start and end depths, volume=0, 
    # or missing split fraction
    same_depths=which(df$header_start_depth==df$header_end_depth)
    depth_diff=which((df$header_start_depth-df$header_end_depth)<5)
    zero_volume=which(df$volume==0)
    split_fraction=which(df$split_fraction==0 | is.na(df$split_fraction) | df$split_fraction>1)
    
    bqc=unique(c(same_depths,depth_diff,zero_volume,split_fraction))
    
    # Filtered data
    if (length(bqc)>0) {df=df[-bqc,]}
    
    # Find and remove suspect plankton sequences. This list was established after data qc (multiple values for same sample ID)
    suspects=c(20000001813260,20000001813261,20000001813262,20000001813374,20000001813375,20000001813376)
    sus=which(df$plankton_general_seq %in% suspects)
    df=df[-sus,]
    
    
    # Compute abundance from counts, and biomass from dry_weight, and wet_weight
    
    df$abund_counts=df$counts*abs(df$header_start_depth-df$header_end_depth)/df$split_fraction/df$volume
    df$biomass_dry=df$dry_weight*abs(df$header_start_depth-df$header_end_depth)/df$split_fraction/df$volume
    df$biomass_wet=df$wet_weight*abs(df$header_start_depth-df$header_end_depth)/df$split_fraction/df$volume
    
    # add year and month columns
    require(lubridate)
    df$month=month(df$header_start)
    df$year=year(df$header_start)
    
    # === End of Step 1. Filtered dataset that includes abundance is df ===
    
    
    
    save(df, file=fn, compress=TRUE )
    return( "Completed filtering step" )
  } 

  if ( DS %in% c("zoop.totalSamples", "zoop.speciesAbund",
                 "zoop.totalSamples.redo", "zoop.speciesAbund.redo")  ) {
    
    fn1 = file.path( biochem.data.dir, paste( "zoop.totalSamples", ".rdata", sep="") )  
    fn2 = file.path( biochem.data.dir, paste( "zoop.speciesAbund", ".rdata", sep="") )  
    
    if ( DS == "zoop.totalSamples" ) {
      ts = NULL
      if (file.exists(fn1)) load(fn1)
      return(ts)
    }
  
    
    if ( DS == "zoop.speciesAbund" ) {
      sa = NULL
      if (file.exists(fn2)) load(fn2)
      return(sa)
    }
    
    df = zoop.db( DS="zoop.qa.qc", p=p)
    
    
    # ===== Step 2 =====
    # Create final files:
    #a. totalSamples dataframe with weights and sum of the abundance for each sample
    #b. speciesAbund dataframe with abundance for each species
    
    # --- a. create dataframe for total samples ---
    
    # Find all unique collector_sampl_id, dates and position
    cols=c("collector_sample_id","mission","header_start","header_start_lat","header_start_lon")
    uid=df[!duplicated(df[,cols]),cols]
    
    # Sum counts from all the species
    counts0=aggregate(abund_counts ~ collector_sample_id, data = df, sum)
    names(counts0)=gsub("abund_counts","total_abund",names(counts0))
    
    # Merge counts with metadata
    joinID="collector_sample_id"
    counts=merge(uid,counts0, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    
    # Sum all Calanus species separately
    calSpec=c("Calanus finmarchicus","Calanus hyperboreus","Calanus glacialis")
    calF=aggregate(abund_counts ~ collector_sample_id, data = df[which(df$biochem_taxonname== calSpec[1]),], sum)
    names(calF)=gsub("abund_counts","calF_abund",names(calF))
    
    calH=aggregate(abund_counts ~ collector_sample_id, data = df[which(df$biochem_taxonname== calSpec[2]),], sum)
    names(calH)=gsub("abund_counts","calH_abund",names(calH))
    
    calG=aggregate(abund_counts ~ collector_sample_id, data = df[which(df$biochem_taxonname== calSpec[3]),], sum)
    names(calG)=gsub("abund_counts","calG_abund",names(calG))
    
    # Find wet weight for total samples
    ww=which(df$biochem_taxonname=="total sample" & df$min_sieve %in% c(0.202,0.20) & is.na(df$max_sieve))
    wwtotal=df[ww,c("collector_sample_id","biomass_wet")]
    names(wwtotal)=gsub("biomass_wet","total_biomass_wet",names(wwtotal))
    
    # Merge all columns with full outer join
    joinID="collector_sample_id"
    calFH=merge(calF, calH, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    calFHG=merge(calFH, calG, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    calww=merge(wwtotal, calFHG, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    
    # Dataframe with wet weights and sum of the counts for all species and 3 Calanus species
    totalSamples=merge(counts, calww, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    
    # Add small and large fractions, with dry weight and wet weight:
    smab=aggregate(abund_counts ~ collector_sample_id, data = df[which(df$min_sieve %in% c(0.2,0.202) & df$max_sieve==10),], sum)
    names(smab)=gsub("abund_counts","small_abund",names(smab))
    
    lgab=aggregate(abund_counts ~ collector_sample_id, data = df[which(df$min_sieve==10 & is.na(df$max_sieve)),], sum)
    names(lgab)=gsub("abund_counts","large_abund",names(lgab))
    
    # find dry and wet weight for small organisma
    swd=which(df$biochem_taxonname=="total sample" & df$min_sieve %in% c(0.202,0.20) & df$max_sieve==10)
    smwd=df[swd,c("collector_sample_id","biomass_wet","biomass_dry")]
    names(smwd)=gsub("biomass_wet","small_biomass_wet",names(smwd))
    names(smwd)=gsub("biomass_dry","small_biomass_dry",names(smwd))
    
    
    # find wet weight for large organisms
    lw=which(df$biochem_taxonname=="total sample" & df$min_sieve==10 & is.na(df$max_sieve))
    lgw=df[lw,c("collector_sample_id","biomass_wet")]
    names(lgw)=gsub("biomass_wet","large_biomass_wet",names(lgw))
    
    # Merge small and large organisms by collector sample id
    sss=merge(smwd[!is.na(smwd$small_biomass_wet),c(1,2)], smwd[!is.na(smwd$small_biomass_dry),c(1,3)], by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    m0=merge(smab, sss, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    m1=merge(m0, lgab, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE) 
    m2=merge(m1,lgw, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    
    # Merge with total sample
    ts=merge(totalSamples,m2, by=joinID,  all.x=TRUE, all.y=TRUE, sort=FALSE )
    
    # If counts for small are missing but there is small wet eight and counts for large, replace total counts with NA
    ww=which(is.na(ts$small_abund) & !is.na(ts$large_abund) & !is.na(ts$small_biomass_wet))
    ts$total_abund[ww]=NA
    
    # Only data with complete records for total counts (abundance) and wet weight
    cc=which(complete.cases(ts[,c("total_biomass_wet","total_abund")]))
    tsc=ts[cc,]
    
    # Add month and year
    require(lubridate)
    ts$month=month(ts$header_start)
    ts$year=year(ts$header_start)
    
    save( ts, file=fn1, compress=TRUE )
    # ---  a. completed. ts is data frame containing "total sample" data (abundance and biomass) ---
    
    
    
    # --- b. Create dataframe that has all species and abundance only ---
    
    sumsp=aggregate(cbind(counts,abund_counts) ~ biochem_taxonname+collector_sample_id,data=df, sum, na.rm=TRUE)
    
    joinID="collector_sample_id"
    sa=merge(uid,sumsp, by=joinID, all.y=TRUE, sort=FALSE )
    
    # Remove "total sample" from the file
    sa=sa[which(sa$biochem_taxonname != "total sample"),]
    
    #--- b. completed, sa dataframe contain species abundance ---
    
    save(sa, file=fn2, compress=TRUE)
  
  }
  
  if (DS=="plotdata") {
    # Make plots for data summary
    
    df = zoop.db( DS="zoop.qa.qc" )
    ts = zoop.db( DS="zoop.totalSamples" )
    sa = zoop.db( DS="zoop.speciesAbund" )


    # Extract only individual net deployments
    nets=df[ ,c("mission","header_start","collector_station_name",
                "collector_sample_id","header_start_lat","header_start_lon")]
    nets=nets[which(!duplicated(nets)),]
    
    require(lubridate)
    nets$month=month(nets$header_start)
    nets$year=year(nets$header_start)
    
    
    # Plot data with trellis (lattice) with coastline in the background
    # by Jae Feb 12, 2015
    
    library(maps)
    library(mapdata)
    library(lattice)
    trellis.par.set(col.whitebg())
    # par( mar=c(5, 4, 4, 2) )
    # aggregate by month
    plm = xyplot( header_start_lat ~ header_start_lon| factor(month,labels=month.name), 
                  data=nets,as.table=TRUE,
                  xlab="Longitude",ylab="Latitude",main="Zooplankton data, 1999-2014",      
                  panel = function(x, y, subscripts, ...) {
                    require(maps)
                    coast=  map( "worldHires", regions=c("Canada", "USA"), 
                                 xlim=c(-72, -56 ), ylim=c(42,49), fill=TRUE, resolution=0, plot=FALSE)
                    panel.lines( coast$x, coast$y, col = "grey25", lwd=0.5 )
                    panel.xyplot( x, y, pch=21, col="dodgerblue",cex=0.7, ...)
                  }
    )
    print(plm)
    dev.off()
    
    
    # plot histogram of monthly measurements
    plot( xtabs( ~ month, data=nets ), lwd=5, col="blue",xaxt="n", xlab="Month",ylab="Number of net deployments", main="Zooplankton data 1999-2014")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    
    # plot total biomass from wet weight monthly, all data
    par(mfrow=c(1,1)) 
    mtw=tapply(ts$total_biomass_wet,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$total_biomass_wet,col="dodgerblue",xaxt="n",xlab="Month",ylab="Total biomass [g/m2]", main="Total biomass")
    points(as.numeric(names(mtw)),mtw,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot total biomass from wet weight monthly, average only
    plot(as.numeric(names(mtw)),mtw,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Total biomass [g/m2]", main="Total biomass")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    

    # plot total abundance monthly, all data
    mta=tapply(ts$total_abund,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$total_abund,col="dodgerblue",xaxt="n",xlab="Month",ylab="Total abundance [individuals/m2]", main="Total abundance")
    points(as.numeric(names(mtw)),mta,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot total abundance monthly, average only
    plot(as.numeric(names(mtw)),mta,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Total abundance [individuals/m2]", main="Total abundance")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot ratio of the mean total biomass to mean total abundance
    plot(as.numeric(names(mtw)),mtw/mta,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Biomass/Abundance [g/individual]", main="Biomass/Abundance")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    
    # small fraction plots:
    # plot wet biomass for small fraction, all data
    par(mfrow=c(1,1)) 
    mtws=tapply(ts$small_biomass_wet,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$small_biomass_wet,col="dodgerblue",xaxt="n",xlab="Month",ylab="Small biomass [g/m2]", main="Small size biomass (0.2-10mm), wet")
    points(as.numeric(names(mtws)),mtws,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot wet biomass for small fraction, average only
    plot(as.numeric(names(mtws)),mtws,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Small biomass wet [g/m2]", main="Small size biomass (0.2-10mm), wet")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot dry biomass for small size, all data
    mtds=tapply(ts$small_biomass_dry,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$small_biomass_dry,col="dodgerblue",xaxt="n",xlab="Month",ylab="Small biomass [g/m2]", main="Small size biomass (0.2-10mm), dry")
    points(as.numeric(names(mtws)),mtds,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot dry biomass for small fraction, average only
    plot(as.numeric(names(mtds)),mtds,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Small biomass wet [g/m2]", main="Small size biomass (0.2-10mm), dry")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    
    # plot abundance for small size, all data
    msa=tapply(ts$small_abund,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$small_abund,col="dodgerblue",xaxt="n",xlab="Month",ylab="Small abundance [individuals/m2]", main="Small size abundance")
    points(as.numeric(names(msa)),msa,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot abundance for small size, average only
    plot(as.numeric(names(msa)),msa,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Small abundance [individuals/m2]", main="Small size abundance")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot of the ratio of the mean WET biomass to abundance for small size
    plot(as.numeric(names(mtw)),mtws/msa,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Biomass/Abundance [g/individual]", main="Small size Biomass/Abundance, wet")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # plot of the ratio of the mean DRY biomass to abundance for small size
    plot(as.numeric(names(mtw)),mtds/msa,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Biomass/Abundance [g/individual]", main="Small size Biomass/Abundance, dry")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    
    # plot wet vs dry biomass
    plot(ts$small_biomass_wet,ts$small_biomass_dry, col="dodgerblue", xlab='Wet biomass', ylab="Dry biomass", main="Small size 0.2-10mm")
    
    
    # plot of the ratio computed from individual samples -- not looking good
    ts$ma_ratio=ts$total_biomass_wet/ts$total_abund
    
    mtr=tapply(ts$ma_ratio,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$ma_ratio,col="dodgerblue",xaxt="n",xlab="Month",ylab="Weight/abundance [g/individual]")
    points(as.numeric(names(mtw)),mtr,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    plot(as.numeric(names(mtr)),mtr,type='b', col='red',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Biomass/Abundance [g/individual]", main="Mean total Biomass/Abundance, wet")
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    
    # Plot Calanus species
    # Calanus finmachicus
    mtcf=tapply(ts$calF_abund,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$calF_abund,col="dodgerblue",xaxt="n",xlab="Month",ylab="Abundance [individuals/m2]", main="Calanus F.")
    points(as.numeric(names(mtcf)),mtcf,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # Calanus hyperboreus
    mtch=tapply(ts$calH_abund,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$calH_abund,col="dodgerblue",xaxt="n",xlab="Month",ylab="Abundance [individuals/m2]", main="Calanus H.")
    points(as.numeric(names(mtch)),mtch,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # Calanus glacialis
    mtcg=tapply(ts$calG_abund,ts$month,mean,na.rm=TRUE)
    plot(month(ts$header_start),ts$calG_abund,col="dodgerblue",xaxt="n",xlab="Month",ylab="Abundance [individuals/m2]", main="Calanus G.")
    points(as.numeric(names(mtcg)),mtcg,type='b', col='red',pch=21,cex=2,lwd=2.5)
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    
    # Plot all 3 calanus species on the same plot
    plot(as.numeric(names(mtcf)),mtcf,type='b', col='dodgerblue',pch=21,cex=1.5,lwd=2.5,xaxt="n",xlab="Month",ylab="Abundance [individuals/m2]", ylim=c(0,37000))
    points(as.numeric(names(mtch)),mtch,type='b', col='green',pch=21,cex=1.5,lwd=2.5)
    points(as.numeric(names(mtcg)),mtcg,type='b', col='orange',pch=21,cex=1.5,lwd=2.5)
    legend("topright",bty="n",c("Calanus F.", "Calanus H.","Calanus G."), pch=c(21,21,21), col=c("dodgerblue","green","orange"),lwd=c(2.5,2.5,2.5),cex=c(1.5,1.5,1.5))
    axis(1, at=1:12,labels=substr(month.abb,1,1))
    #----- End of plots ----
    
    
    # Plot all the raw counts for Calanus F.
    species="Calanus finmarchicus"
    
    # i is index all data, 1914-2014, zoo dataframe
    i=which(zoo$biochem_taxonname %in% species & !is.na(zoo$counts))
    
    # j is only for filtered dataset 1999-2014, df dataframe
    j=which(df$biochem_taxonname %in% species & !is.na(df$counts))
    
    # s is for filtered dataset and stages
    stages=c("V ","VI")
    stages1=c("I","II","III","IV",stages, "I-V")
    
    s=which(df$biochem_taxonname %in% species & !is.na(df$counts) & df$molt_number %in% stages)
    s1=which(df$biochem_taxonname %in% species & !is.na(df$counts) & df$molt_number %in% stages1)
    #plot(zoo$counts[i] ~ as.Date(zoo$header_start[i]),xlab="Year",ylab="Counts",main=species)
    #points(df$counts[j]~ as.Date(df$header_start[j]), col="red")
    #plot(df$counts[j] ~ as.Date(df$header_start[j]),xlab="Year",ylab="Counts",main=species)
    as=c(s,s1) #all stages
    # Plot monthly averages for calanus F.
    # 1. Sum all the counts in each sample, then log them, then average
    calM=aggregate(abund_counts ~ header_start+collector_sample_id, data = df[as,], sum)
    calM$month=month(calM$header_start)
    calM$log_abund=log10(calM$abund_counts+1)
    cfm=aggregate(cbind(log_abund,abund_counts) ~ month, data=calM,mean)
    
    c1=aggregate(abund_counts ~ header_start+collector_sample_id, data = df[s1,], sum)
    c1$month=month(c1$header_start)
    c1$log_abund=log10(c1$abund_counts+1)
    c1234=aggregate(cbind(log_abund,abund_counts) ~ month, data=c1,mean)
    
    c5=aggregate(abund_counts ~ header_start+collector_sample_id, data = df[s,], sum)
    c5$month=month(c5$header_start)
    c5$log_abund=log10(c5$abund_counts+1)
    c56=aggregate(cbind(log_abund,abund_counts) ~ month, data=c5,mean)
    
    x=cfm
    x1=c1234
    x2=c56
    
    # plot log abundance from counts for all stages and separate
    plot(x$month,x$log_abund,type="o",col="black",pch=23,cex=1.2, bg="green",
         xlab="Month",ylab="log10(abundance+1) [individuals/m2]",ylim=c(2,5), 
         main="Calanus Finmarchicus")
    points(x1$month,x1$log_abund,type="o",col="black", pch=21,cex=1.2, bg="red")
    points(x2$month,x2$log_abund,type="o",col="black", pch=22,cex=1.2, bg="blue")
    legend("topright",legend=c("Stages I-IV","Stages V-VI","All stages"), col = c("blue","red","green"),pch=c(15,16,18),lty=c(1,1,1), cex=1.2)
    
    
    # plot all monthly data (all stages) with average on top
    plot(calM$month,calM$abund_counts,type="p",col="green",pch=21,cex=1,
         xlab="Month",ylab="Abundance [individuals/m2]",
         main="Calanus Finmarchicus",ylim=c(0,35000))
    points(x$month,x$abund_counts,type="b",col="red",pch=21,cex=1.5,lwd=2)
    
    points(x1$month,x1$abund_counts,type="o",col="black", pch=21,cex=1.2, bg="red")
    points(x2$month,x2$abund_counts,type="o",col="black", pch=22,cex=1.2, bg="blue")
    legend("topright",legend=c("Stages I-IV","Stages V-VI","All stages"), col = c("blue","red","green"),pch=c(15,16,18),lty=c(1,1,1), cex=1.2)
    
    
    
    # plot abundance from counts for all stages and separate
    plot(x$month,x$abund_counts,type="o",col="black",pch=23,cex=1.2, bg="green",
         xlab="Month",ylab="Abundance [individuals/m2]", ylim=c(0,30000),
         main="Calanus Finmarchicus")
    points(x1$month,x1$abund_counts,type="o",col="black", pch=21,cex=1.2, bg="red")
    points(x2$month,x2$abund_counts,type="o",col="black", pch=22,cex=1.2, bg="blue")
    legend("topright",legend=c("Stages I-IV","Stages V-VI","All stages"), col = c("blue","red","green"),pch=c(15,16,18),lty=c(1,1,1), cex=1.2)
    
    
    
    
    
    # Extract only individual net deployments
    nets=df[ ,c("mission","header_start","collector_station_name",
                "collector_sample_id","header_start_lat","header_start_lon")]
    nets=nets[which(!duplicated(nets)),]
    
    require(lubridate)
    nets$month=month(nets$header_start)
    nets$year=year(nets$header_start)
    nets$monthName=month.name[nets$month]
    
    # Order by date
    nets=nets[order(nets$header_start),]
    
    # Plot net deployments by month
    require(lattice)
    xyplot(header_start_lat ~ header_start_lon| factor(month,labels=month.name), 
           data=nets,as.table=TRUE,
           xlab="Longitude",ylab="Latitude",main="Zooplankton data, 1999-2014")
    dev.copy(png,"zooplankton_data_monthly.png")
    dev.off()
    
    xyplot(header_start_lat ~ header_start_lon| factor(year), 
           data=nets,as.table=TRUE,
           xlab="Longitude",ylab="Latitude",main="Zooplankton data, 1999-2014")
    
    
    # Plot data with trellis (lattice) with coastline in the background
    # by Jae Feb 12, 2015
    
    library(maps)
    library(mapdata)
    trellis.par.set(col.whitebg())
    # par( mar=c(5, 4, 4, 2) )
    # aggregate by month
    plm = xyplot( header_start_lat ~ header_start_lon| factor(month,labels=month.name), 
                  data=nets,as.table=TRUE,
                  xlab="Longitude",ylab="Latitude",main="Zooplankton data, 1999-2014",      
                  panel = function(x, y, subscripts, ...) {
                    require(maps)
                    coast=  map( "worldHires", regions=c("Canada", "USA"), 
                                 xlim=c(-72, -56 ), ylim=c(42,49), fill=TRUE, resolution=0, plot=FALSE)
                    panel.lines( coast$x, coast$y, col = "grey25", lwd=0.7 )
                    panel.xyplot( x, y, pch=21, cex=0.8, ...)
                  }
    )
    
    # aggregate by year
    pl = xyplot( header_start_lat ~ header_start_lon| factor(year), 
                 data=nets,as.table=TRUE,
                 xlab="Longitude",ylab="Latitude",main="Zooplankton data, 1999-2014",      
                 panel = function(x, y, subscripts, ...) {
                   require(maps)
                   coast=  map( "worldHires", regions=c("Canada", "USA"), 
                                xlim=c(-72, -56 ), ylim=c(42,49), fill=TRUE, resolution=0, plot=FALSE)
                   panel.lines( coast$x, coast$y, col = "grey25", lwd=0.7 )
                   panel.xyplot( x, y, pch=21, cex=0.8 , ...)
                 }
    )
    
    
    Cairo( file="zooplankton_data_yearly.pdf", type="pdf", bg="white",  units="in", width=6, height=8 )
    print(pl)
    dev.off()
    
    
    # Plot net deployments for each year by month
    yrs=unique(nets$year)
    
    for (i in 1:length(yrs) ) { 
      
      ind=which(nets$year==yrs[i])
      tdf=nets[ind,]
      lab=month.name[unique(tdf$month)]
      tit=as.character(yrs[i])
      
      print(xyplot(header_start_lat ~ header_start_lon| factor(month,labels=lab), 
                   data=tdf,as.table=TRUE, xlab="Longitude",ylab="Latitude",main=tit))
    }
    
    # --- Data  inventory -----
    # Find how many samples have counts, dry_weight and wet_weight
    samples_counts=unique(df$collector_sample_id[!is.na(df$counts)])
    samples_dry=unique(df$collector_sample_id[!is.na(df$dry_weight)])
    samples_wet=unique(df$collector_sample_id[!is.na(df$wet_weight)])
    
    samplesCombos=unique()
    
    counts_and_dry=intersect(samples_counts,samples_dry)
    counts_dry_wet=intersect(counts_and_dry,samples_wet)
    
    # Find how many samples have counts, dry weight, wet weight for sieves 202-10
    nc=which(!is.na(df$counts) & df$min_sieve %in% c(0.2,0.202) & df$max_sieve==10)
    nd=which(!is.na(df$dry_weight) & df$min_sieve %in% c(0.2,0.202) & df$max_sieve==10)
    nw=which(!is.na(df$wet_weight) & df$min_sieve %in% c(0.2,0.202) & df$max_sieve==10)
    
    sc=which(df$min_sieve %in% c(0.2,0.202) & df$max_sieve==10)
    scs=unique(df$collector_sample_id[sc])
    
    c202=unique(df$collector_sample_id[nc])
    d202=unique(df$collector_sample_id[nd])
    w202=unique(df$collector_sample_id[nw])
    
    cd=intersect(c202,d202)
    cw=intersect(c202,w202)
    cdw=intersect(cd,w202)
    
    # How many samples have large zooplankton
    nclg=which(!is.na(df$counts) & df$min_sieve==10 & is.na(df$max_sieve))
    nwlg=which(!is.na(df$wet_weight) & df$min_sieve==10 & is.na(df$max_sieve))
    
    # How many samples have large bugs
    sclg=which(df$min_sieve==10 & is.na(df$max_sieve))
    scslg=unique(df$collector_sample_id[sc])
    
    clg=unique(df$collector_sample_id[nclg])
    wlg=unique(df$collector_sample_id[nwlg])
    
    
    cwlg=intersect(clg,wlg)
    
    # How many samples have large and small bugs
    
    lsc=intersect(c202,clg)
    lsw=intersect(w202,wlg)
    lscw=intersect(cw,cwlg)
    lscwd=intersect(cdw,cwlg) 
    
    # Plot dry and wet weight monthly (nd are indices for dry and nw for wet weight)
    
    par(mfrow=c(2,2))
    plot(df$month[nd],df$biomass_dry[nd],xlab="Month", ylab="Biomass dry [g/m2]",main="Dry weight biomass", col="blue")
    plot(df$month[nw],df$biomass_wet[nw],xlab="Month", ylab="Biomass wet [g/m2]",main="Wet weight biomass", col="red")
    plot(df$month[nd],log10(df$biomass_dry[nd]),xlab="Month", ylab="log10 (Biomass dry) [g/m2]",main="Log 10 (dry weight biomass)", col="blue")
    plot(df$month[nw],log10(df$biomass_wet[nw]),xlab="Month", ylab="log10 (Biomass wet) [g/m2]",main="Log 10 (wet weight biomass)", col="red")
    
    # plot only data for HL2 and size 0.202- 10
    dfw=df[nw,]
    dfd=df[nd,]
    dfc=df[nc,]
    
    which()
    
    
    # ---- End of data inventory -----
    
    
    # How many dry_weight records there is per sample id?
    dwsid= aggregate(dry_weight ~ collector_sample_id, data=df, function(x) {sum(!is.na(x))}, na.action = NULL)
    
    # How many samples have 0, 1, 2 or 3 dry weights?
    tab=xtabs(~ dry_weight, data=dwsid)
    
    # What are all possible min and max sieve combinations?
    sieves=df[,c("min_sieve","max_sieve")]
    sieveCombos=sieves[!duplicated(sieves),]
    
    # How many sieve combinations for dry weight data?
    drySieves=sieves[which(!is.na(df$dry_weight)),]
    drySieves$min=factor(drySieves$min_sieve,exclude=NULL)
    drySieves$max=factor(drySieves$max_sieve,exclude=NULL)
    drySievesTable=xtabs(~(min+max),data=drySieves)
    
    drySievesCombos=drySieves[!duplicated(drySieves),]
    
    # How many sieve combinations for wet weight data?
    wetSieves=sieves[which(!is.na(df$wet_weight)),]
    wetSieves$min=factor(wetSieves$min_sieve,exclude=NULL)
    wetSieves$max=factor(wetSieves$max_sieve,exclude=NULL)
    wetSievesTable=xtabs(~(min+max),data=wetSieves)
    
    wetSievesCombos=wetSieves[!duplicated(wetSieves),]
    
    # plot the data for 2013 and HL2 station - reproduce Fig.18 from AZMP 2013 report
    require("lubridate")
    df$month=month(df$header_start)
    df$year=year(df$header_start)
    
    
    
    #exclude mission 18HU13008, just to reproduce plot
    df=df[which(df$mission != "18HU13008"),]
    
    hl2lat=44.27
    hl2lon=-63.32
    
    cf=which(df$biochem_taxonname %in% species & df$year==2013  
             & df$header_start_lat>= hl2lat-0.1 & df$header_start_lat<= hl2lat+0.1
             & df$header_start_lon>=hl2lon-0.1 & df$header_start_lon<=hl2lon+0.1 )
    
    m=tapply(df$abund_counts[cf],df$header_start[cf],sum)
    
    axlab=substr(month.abb, 1,1)
    
    plot(as.Date(names(m)),m,ylim=c(0,80000),xlab="Month",ylab="Abundance [/m^2]")
    axis(1,at=1:12,labels=axlab)
    
    df=out
    
    # Make time series plot of abundance for 1 species, all stages
    
    cc=which(df$biochem_taxonname %in% species)
    
    sum_calF=tapply(df$abund_counts[cc],df$header_start[cc],sum)
    
    plot(as.Date(names(sum_calF)),sum_calF,xlab="Year",ylab="Abundance [/m^2]",main="Calanus finmarchicus, all stages")
    
    # Convert sum_calF to dataframe
    calF=cbind(read.table(text = names(sum_calF)), sum_calF)
    calF$date=as.Date(calF$V1)
    calF$month=month(calF$date)
    
    # Compute mean monthly calanus F. abundance
    calF_monthly=tapply(calF$sum_calF,calF$month,mean)
    
    df=out
    
    #to order data frame by columns
    #dd[with(dd, order(-z, b)), ]
    
    var=c("mission","header_start","collector_station_name","collector_deployment_id",
          "collector_sample_id","header_start_depth","header_end_depth","volume",
          "split_fraction", "counts","abund_counts","biochem_taxonname", "molt_number","gear_model")
    
    dd=df[var]
    dd=dd[hl2c,]
    
    dd[with(dd,order("mission","header_start"))]
    
    write.table(dd, "C:/Gordana/HL2_2013.txt", sep="\t", col.names=TRUE)
    
    h0=grep("_0", df$collector_station_name )
    
    snowc=grep("hionocetes",df$biochem_taxonname)
    
  }  
    
  
} 