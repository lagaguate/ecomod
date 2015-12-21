--This is a modified version of code used by ISDB_HALIBUT to populate its' ISD_INF
--table
SELECT SUBSTR(v.vessel_name,1,15) vessel,
  t.tripcd_id,
  t.board_date,
  --TO_NUMBER(TO_CHAR(t.board_date,'YYYY')) YEAR,
  --DECODE(TO_CHAR(t.Board_Date,'MM'),12,'WINTER',1,'WINTER',2,'WINTER',3,'SPRING',4,'SPRING',5,'SPRING', 6,'SUMMER',7,'SUMMER',8,'SUMMER',9,'FALL',10,'FALL',11,'FALL','MISSING') season,
  --SUBSTR(TO_CHAR(t.board_date,'MON'),1,3) MONTH,
  v.cfv,
  t.trip,
  f.set_no,
  f.nafarea_id,
  --SUBSTR (isset.get_nafo_div(f.nafarea_id), 1, 2) nafdiv,
  f.stratum_id,
  to_number(f.station) station,
  NVL(f.haulccd_id,1) haulccd_id,
  p1.longitude*-1 p1long,
  p1.latitude p1lat,
  p2.longitude*-1 p2long,
  p2.latitude p2lat,
  p3.longitude*-1 p3long,
  p3.latitude p3lat,
  p4.longitude*-1 p4long,
  p4.latitude p4lat,
 -- NVL(p1.longitude, p2.longitude)*-1 slongitude,
 -- NVL(p1.latitude,p2.latitude) slatitude,
 -- NVL(p3.longitude, p4.longitude)*-1 elongitude,
 -- NVL(p3.latitude,p4.latitude) elatitude,
  p1.longddmm p1longddmm,
  p1.latddmm p1latddmm,
  p2.longddmm p2longddmm,
  p2.latddmm p2latddmm,
  p3.longddmm p3longddmm,
  p3.latddmm p3latddmm,
  p4.longddmm p4longddmm,
  p4.latddmm p4latddmm,
  --isset.day_and_time (p1.setdate,p1.settime) SDayTime,
  p1.setdate p1date,
  p1.settime p1time,
  p2.setdate p2date,
  p2.settime p2time,
  --isset.day_and_time (p4.setdate,p4.settime) EDayTime,
  p3.setdate p3date,
  p3.settime p3time,
  p4.setdate p4date,
  p4.settime p4time,
  --DECODE(NVL(p3.settime-p2.settime,-999999),-999999,NULL, 1440*(isset.day_and_time (p3.setdate,p3.settime)-isset.day_and_time (p2.setdate,p2.settime))) soakminp3p2,
  --DECODE(NVL(p4.settime-p1.settime,-999999),-999999,NULL, 1440*(isset.day_and_time (p4.setdate,p4.settime)-isset.day_and_time (p1.setdate,p1.settime))) duration,
  len_longline lenLLkm,
  ROUND((f.num_hook_haul/ 1000.), 2) nK_hooks,
  --(f.num_hook_haul      *gf105.feature_value) gear_len_m,
  p1.depth p1depth,
  p2.depth p2depth,
  p3.depth p3depth,
  p4.depth p4depth,
--  NVL(p1.depth, p2.depth) sdepth,
--  NVL(p3.depth, p4.depth) edepth,
  g.gearcd_id,
  g.hookcd_id,
  g.hooksize,
  f.num_hook_haul,
--  gf105.feature_value hookspacingm,
  f.specscd_id,
  t.vess_id,
  t.trip_id,
  f.fishset_id,
  f.setcd_id,
  a.set_type,
  p1.setprof_id p1setprof_id,
  p4.setprof_id p4setprof_id
FROM observer.isTrips t,
  observer.isvessels v,
--  (SELECT g.gear_id g_id,
--    v.*
--  FROM
--    (SELECT * FROM observer.isgearfeatures WHERE gearfcd_id = 105
--    ) v,
--    observer.isgears g
--  WHERE v.gear_id(+)=g.gear_id
--  ) gf105,
  observer.isgears g,
  (SELECT f.fishset_id f_id,
    p.*
  FROM
    (SELECT * FROM observer.issetprofile WHERE pntcd_id=1
    ) p,
    observer.isFishSets f
  WHERE p.fishset_id(+)=f.fishset_id
  ) p1,
  (SELECT f.fishset_id f_id,
    p.*
  FROM
    (SELECT * FROM observer.issetprofile WHERE pntcd_id=2
    ) p,
    observer.isFishSets f
  WHERE p.fishset_id(+)=f.fishset_id
  ) p2,
  (SELECT f.fishset_id f_id,
    p.*
  FROM
    (SELECT * FROM observer.issetprofile WHERE pntcd_id=3
    ) p,
    observer.isFishSets f
  WHERE p.fishset_id(+)=f.fishset_id
  ) p3,
  (SELECT f.fishset_id f_id,
    p.*
  FROM
    (SELECT * FROM observer.issetprofile WHERE pntcd_id=4
    ) p,
    observer.isFishSets f
  WHERE p.fishset_id(+)=f.fishset_id
  ) p4,
  observer.isFishSets f,
  observer.issettypecodes a
WHERE f.fishset_id=p1.f_id
AND f.fishset_id  =p2.f_id
AND f.fishset_id  =p3.f_id
AND f.fishset_id  =p4.f_id
AND t.vess_id     =v.vess_id
AND t.trip_id     =f.trip_id
AND g.gear_id     =f.gear_id
--AND g.gear_id     =gf105.g_id
AND f.setcd_id    = a.setcd_id
  --AND T.TripCd_Id in (7057,7058)
  --AND T.owner_group ='USER_HALIBUT'';
AND ROWNUM < 20;