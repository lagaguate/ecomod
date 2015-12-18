rem IOP.BAT

rem This script is designed to extract multiple species. Filtering adjustments can be effected
rem during creation of the first table (where speccd_id in ...), and via trip codes during 
rem creation of iop4. Rename the last table (e.g. iopce) as appropriate.

rem Run this script by typing '@d:\assess\Extract\iop\IOP.bat' at the SQL command line
rem 9430 TORTOISES AND SEA GOING TURTLES CHELONIA O.      
rem 9431 GREEN SEA TURTLE CHELONIA MYDAS
rem 9432 HAWKSBILL SEA TURTLE      ERETMOCHELYS IMBRICATA                                                           
rem 9433 KEMPS RIDLEY  SEA TURTLE    LEPODOCHELYS KEMPII                                                          
rem 9434 OLIVE RIDLEY  SEA TURTLE    LEPODOCHELYS OLIVACEA                                                         
rem 9435 LEATHERBACK SEA TURTLE     DEMOCHELYS CORIAVEA                                                            
rem 9436 LOGGERHEAD SEA TURTLE       CARETTA CARETTA                                                               
rem 9437 HARD SHELL SEA TURTLE NS   CHELONIDAE NS       
  
drop table iopfirst;
create table iopfirst as select
	c.fishset_id, c.set_no,
sum(c.est_combined_wt) TotalWt,
	nvl(sum(decode(c.speccd_id,'9435', c.est_combined_wt)),0) Leatherback,
  nvl(sum(decode(c.speccd_id,'9436', c.est_num_caught)),0) Loggerhead
from	observer.iscatches c
group by c.fishset_id, c.set_no;
drop table iopsp;
create table iopsp as select
	fishset_id, set_no, max(depth) towdep, max(botcd_id) bottom 
from	observer.issetprofile
group by fishset_id, set_no;
drop table iop2;
create table iop2 as select
	c.fishset_id, c.set_no, TotalWt,
Leatherback,Loggerhead,	towdep, bottom 
from	iopsp s, iopfirst c
where	c.fishset_id = s.fishset_id (+)
	and c.set_no = s.set_no (+);
drop table iopsp;
drop table iop3;
drop table iopfirst;
create table iop3 as select
	s.trip_id, c.fishset_id, c.set_no, s.setcd_id, s.gear_id, s.specscd_id, s.nafarea_id, s.num_hook_haul,
	s.stratum_id,s.haulccd_id,s.len_longline,
	c.towdep, c.bottom, TotalWt,
Leatherback, Loggerhead
from	iop2 c,
	observer.isfishsets s
where	c.fishset_id = s.fishset_id
	and c.set_no = s.set_no;
drop table iop4;
drop table iop2;
create table iop4 as select
	r.vess_id, r.license_no, r.tripcd_id, r.owner_group, r.trip, r.board_date,
	c.trip_id, c.fishset_id, c.set_no, c.setcd_id, c.gear_id, c.specscd_id, c.nafarea_id, c.num_hook_haul,
	c.stratum_id,c.haulccd_id,c.len_longline,
	c.towdep, c.bottom,TotalWt,
Leatherback,Loggerhead
from	iop3 c,
	observer.istrips r
where	c.trip_id = r.trip_id;
drop table iop5;
drop table iop3;
create table iop5 as select
	v.ctrycd_id, c.vess_id, v.cfv, c.license_no, v.tonccd_id, c.tripcd_id, c.owner_group, c.trip, c.board_date,
	c.trip_id, c.fishset_id, c.set_no, c.setcd_id, c.gear_id, c.specscd_id, c.nafarea_id, c.num_hook_haul,
	c.stratum_id,c.haulccd_id,c.len_longline,
	c.towdep, c.bottom, TotalWt,
Leatherback, Loggerhead 
from	iop4 c,
	observer.isvessels v
where	c.vess_id = v.vess_id
	and	c.license_no = v.license_no;
drop table iop6;
drop table iop4;
create table iop6 as select
	c.ctrycd_id, c.vess_id, c.cfv, c.license_no, c.tonccd_id, g.gearcd_id, c.tripcd_id, c.owner_group, c.trip, c.board_date,
	c.trip_id, c.fishset_id, c.set_no, c.setcd_id, c.gear_id, c.specscd_id, c.nafarea_id, c.num_hook_haul,
	c.stratum_id,c.haulccd_id,c.len_longline,g.wingspread,g.body_mesh_size,g.hookcd_id,g.hooksize,
	c.towdep, c.bottom, TotalWt,
Leatherback, Loggerhead
from	iop5 c,
	observer.isgears g
where	c.trip_id = g.trip_id
	and	c.gear_id = g.gear_id;
drop table MainSpec;
create table MainSpec 
AS select
       fishset_id
      ,MIN(speccd_id) MainSpec  
FROM observer.iscatches
WHERE 
     fishset_id||','||est_combined_wt IN
       (
        select fishset_id||','||MAX(est_combined_wt)
          FROM observer.iscatches
            GROUP BY fishset_id
        )
GROUP BY fishset_id;
drop table effs;
create table effs
AS select
	f.FishSet_Id,
	f.Set_No,
	f.Gear_Id,
	f.Trip_Id
    ,decode(g.PntCd_Id, 1, g.SetDate, 2, g.SetDate, null) StartDate
    ,decode(g.PntCd_Id, 1, g.SetTime, 2, g.SetTime, null) StartTime
    ,decode (g.PntCd_Id, 1, g.Latitude, 2, g.Latitude, null) StartLat
    ,(-1* (decode (g.PntCd_Id, 1, g.Longitude, 2, g.Longitude, null))) StartLong
FROM observer.ISFishSets f
    ,observer.ISSetProfile g
    ,observer.ISGears k
WHERE f.FishSet_Id = g.FishSet_Id
  and f.Set_No = g.Set_No
  and f.Gear_Id = k.Gear_Id
  and (
       (k.GearCd_Id < 32 OR k.Gearcd_Id IN (55, 71, 72, 73))
        and g.PntCd_Id = 2 /* select start point for mobile gear where avail. */
      OR
       (k.GearCd_Id BETWEEN 39 and 54 OR k.GearCd_Id BETWEEN 56 and 63 OR k.Gearcd_Id = 81)
        and g.Pntcd_Id = 1 /* select start point for fixed gear where avail. */
      );
drop table effe;
create table effe
as select
	f.FishSet_Id,
	f.Set_No,
	f.Gear_Id,
	f.Trip_Id
    ,decode(g.Pntcd_Id, 3, g.SetDate, 4, g.SetDate, null) EndDate
    ,decode(g.Pntcd_Id, 3, g.SetTime, 4, g.SetTime, null) EndTime
    ,decode (g.Pntcd_Id, 3, g.Latitude, 4, g.Latitude, null) EndLat
    ,(-1* (decode (g.Pntcd_Id, 3, g.Longitude, 4, g.Longitude, null))) EndLong
FROM observer.ISFishSets f
    ,observer.ISSetProfile g
    ,observer.ISGears k
WHERE f.FishSet_Id = g.FishSet_Id
  and f.Set_No = g.Set_No
  and f.Gear_Id = k.Gear_Id
  and (
       (k.Gearcd_Id < 32 OR k.Gearcd_Id IN (55, 71, 72, 73))
        and g.Pntcd_Id = 3 /* select end point for mobile gear where avail. */
      OR
       (k.Gearcd_Id BETWEEN 39 and 63 OR k.Gearcd_Id = 81)
        and g.Pntcd_Id = 4 /* select end point for fixed gear where avail. */
      );
drop table eff;
create table eff as select
	s.FishSet_Id,
	s.Set_No,
	s.Gear_Id,
	s.Trip_Id,
	s.StartDate,s.StartTime,s.StartLat,s.StartLong,e.EndDate,e.EndTime,e.EndLat,e.EndLong
from effs s, effe e
WHERE s.FishSet_Id = e.FishSet_Id
  and s.Set_No = e.Set_No
  and s.Gear_Id = e.Gear_Id
  and s.Trip_Id = e.Trip_Id;
drop table iop5;
drop table IOP;
create table IOP
as select
	c.ctrycd_id, nvl(c.owner_group,'0') owner_group, c.cfv, nvl(c.license_no,'0') license_no, c.gear_id, c.gearcd_id,
	nvl(c.tonccd_id,0) tonccd_id, nvl(c.tripcd_id,0) tripcd_id, c.trip_id, nvl(c.trip,0) trip, c.fishset_id,
	c.set_no, c.setcd_id, c.specscd_id, nvl(c.nafarea_id,'XXX') nafarea_id, c.towdep, nvl(c.bottom,'0') botdep,
	nvl(c.num_hook_haul,0) num_hook_haul,
	c.stratum_id,c.haulccd_id,c.len_longline,c.wingspread,c.body_mesh_size,c.hookcd_id,c.hooksize,
	nvl(m.MainSpec,0) mainspec,
	c.board_date,
	nvl(e.StartDate,'01-JAN-9999') sdate,nvl(e.StartTime,0) stime,nvl(e.StartLat,0) slat,nvl(e.StartLong,0) slong,
	nvl(e.EndDate,'01-JAN-9999') edate,nvl(e.EndTime,0) etime,nvl(e.EndLat,0) elat,nvl(e.EndLong,0) elong,
TotalWt,Leatherback, Loggerhead
FROM iop6 c
    ,eff e
    ,MainSpec m
WHERE c.FishSet_Id = e.FishSet_Id (+)
  and c.FishSet_Id = m.FishSet_Id (+)
  and c.Set_No = e.Set_No (+)
  and c.Gear_Id = e.Gear_Id (+)
  and c.Trip_Id = e.Trip_Id (+);
drop table iop6;
drop table effs;
drop table effe;
drop table eff;
drop table MainSpec;
drop table seabirdie;
CREATE TABLE SEABIRDIE AS
select ctrycd_id,
gearcd_id,
tonccd_id, 
setcd_id, specscd_id, nafarea_id, towdep, botdep,
num_hook_haul,
stratum_id,haulccd_id,len_longline,wingspread,body_mesh_size,hookcd_id,hooksize,
mainspec,
board_date,
sdate,stime,slat,slong,
edate,etime,elat,elong,
TotalWt,
Leatherback, Loggerhead
from IOP
where to_number(substr(to_char(board_date,'YYYY'),1,4))>=1998
order by sdate;



