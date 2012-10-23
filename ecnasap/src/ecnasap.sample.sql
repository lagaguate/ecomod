
SELECT to_char(sysdate,'YYYY-MM-DD') DateLastModified,
       'ECNASAP' InstitiutionCode,
	   DECODE(i.source,1,'NMFS-NEFSC',2,'DFO-SF',3,'DFO-SG',4,'DFO-NG',5,'DFO-NFLD') CollectionCode,
	   'NW ATLANTIC' ContinentOcean,
	   i.setid||'.'||i.spid CatalogNumber,
	   i.setid FieldNumber,
	   1900+i.yr YearCollected,
	   i.month MonthCollected,
	   i.day DayCollected,
	   i.time TimeOfDay,
	   i.strat Locality,
	   i.code speciescode,
	   i.scientificname scientificname,
	   i.commonname commonname,
	   'O' BasisOfRecord,
	   i.lon Longitude,
	   i.lat Latitude,
	   i.btemp Temperature,
	   NVL(c.totno,0) ObservedIndividualCount,
	   NULL ObservedWeight
  FROM (SELECT *
          FROM ecnasap_inf,
	           ecnasap_spec) i,
		ecnasap_cat c
 WHERE i.lat is not null and i.lon is not null
   AND i.setid=c.setid(+)
   AND i.spid=c.spid(+)

  

SELECT collectioncode, yearcollected, locality, 
       fieldnumber setid, latitude lat, longitude lon, temperature, 
       scientificname, speciescode, observedindividualcount val,
       log(10,observedindividualcount+1) logval
  FROM nwagscol.obis_ecnasap_data
/* where */
WHERE collectioncode in ('DFO-SF')
AND speciescode in ('AESSHP','AFPOMP','ALEWIF')
AND yearcollected in ('1973','1974','1975','1976')



