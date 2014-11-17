

mysql -u user -p

pass 

create database ecnasap;
use ecnasap;

drop table spec;
create table spec (
 spid smallint unsigned not null,
 code varchar(8) not null,
 commonname varchar(35) not null,
 scientificname varchar(35) not null,
 primary key (spid)
);

load data
	infile "/home/jae/data/ecnasap/spec.out"
	into table spec
	fields terminated by '\;'
;

drop table spec_classified;
create table spec_classified (
 spid smallint unsigned not null,
 code varchar(8) not null,
 commonname varchar(35) not null,
 scientificname varchar(35) not null,
 pelagic tinyint unsigned,
 elasmo tinyint unsigned,
 mammal tinyint unsigned,
 invert tinyint unsigned,
 pred1 tinyint unsigned,
 pred2 tinyint unsigned,
 prey tinyint unsigned,
 primary key (spid)
);

load data
	infile "/home/jae/data/ecnasap/specieslist_ecnasap_classified.csv"
	into table spec_classified
	fields terminated by '\;'
;



drop table inf;
create table inf (
 setid int unsigned not null,
 source smallint unsigned not null,
 vess varchar(5) not null,
 cruise smallint unsigned,
 set_no smallint unsigned,
 strat varchar(5),
 yr smallint unsigned not null,
 month smallint unsigned,
 day smallint unsigned,
 time smallint unsigned,
 gear smallint unsigned,
 depth smallint unsigned,
 nrows tinyint unsigned,
 btemp decimal(5,2),
 stemp decimal(5,2),
 bsal decimal(5,2), 
 ssal decimal(5,2),
 lat decimal(5,2), 
 lon decimal(5,2), /home/jae/data/ecnasap/specieslist_ecnasap_classified.csv
 primary key (setid)
) ;

load data
	infile "/home/jae/data/ecnasap/inf.out"
	into table inf
	fields terminated by '\;'
;


drop table cat;
create table cat (
 setid int unsigned not null,
 spid  smallint unsigned not null,
 totno decimal (12, 2),
 index (setid, spid)
)
;

load data
	infile "/home/jae/data/ecnasap/cat.out"
	into table cat
	fields terminated by '\;'
;


