Z„BUILD_DFO_QUERY mission;qry;Œelx;all_strat;sel_strat;invalid_strat

Œelx„ "ON_ERROR"

Z„0

© Initiate and make visible the Progress bar and label.
'Form.pbProgress' Œwi 'value' 0 180 20 0
'Form.lProgress' Œwi 'visible' 1
'Form.pbProgress' Œwi 'visible' 1
'Form.lProgress' Œwi 'caption' 'Building Queries'

infquery„catquery„lfquery„alkquery„alkmodquery„alkplusquery„0½0

© Create the infquery based on user selection for DFO data
infquery „ "SELECT STRAT, CONCAT(CONCAT(MISSION,'-'),SETNO) SETNO, DIST, SLAT, SLONG, AREA "
infquery „ infquery, 'FROM GROUNDFISH.GSINF '
infquery „ infquery, 'WHERE ((MISSION) in (', (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ')) '
:IF 0¬½'Form.lsUnitArea' Œwi 'value'
    infquery „ infquery, 'AND ((AREA) in (', (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',')  , ')) '
:ENDIF
infquery „ infquery, 'AND ((STRAT) in (', (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ')) '
infquery „ infquery, 'AND ((TYPE) in (', (¯1 ‡,(•œlist_exptype['Form.lsExptype' Œwi 'value';]),',') , '))'
infquery „ infquery, ' ORDER BY STRAT, SETNO'

© Create the catquery based on user selection for DFO data
catquery „ "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, SAMPWGT, TOTWGT, SIZE_CLASS "
catquery „ catquery, "FROM GROUNDFISH.GSINF, GROUNDFISH.GSCAT "
catquery „ catquery, "WHERE ((SPEC)= " , combo_species['Form.lsSpecies' Œwi 'value';1] , ") "
catquery „ catquery, "AND ((GROUNDFISH.GSCAT.MISSION) in (" ,  (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',')   , ")) "
:IF 0¬½'Form.lsUnitArea' Œwi 'value'
    catquery „ catquery, "AND ((AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',') , ")) "
:ENDIF
catquery „ catquery, "AND ((STRAT) in (" , (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ")) "
catquery „ catquery, 'AND ((TYPE) in (', (¯1 ‡,(•œlist_exptype['Form.lsExptype' Œwi 'value';]),',') , '))'        
catquery „ catquery, "AND GROUNDFISH.GSCAT.MISSION = GROUNDFISH.GSINF.MISSION "
catquery „ catquery, "AND GROUNDFISH.GSCAT.SETNO = GROUNDFISH.GSINF.SETNO "
catquery „ catquery, "ORDER BY GROUNDFISH.GSCAT.SETNO "

© Create the lfquery based on user selection for DFO data    
lfquery „ "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, FSEX, FLEN, CLEN, SIZE_CLASS "
lfquery „ lfquery, "FROM GROUNDFISH.GSDET, GROUNDFISH.GSINF "
lfquery „ lfquery, "WHERE ((SPEC) = " ,combo_species['Form.lsSpecies' Œwi 'value';1] , ") "
lfquery „ lfquery, "AND ((GROUNDFISH.GSDET.MISSION) in (" , (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ")) "
:IF 0¬½'Form.lsUnitArea' Œwi 'value'
    lfquery „ lfquery, "AND ((AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',') , ")) "
:ENDIF
lfquery „ lfquery, "AND ((STRAT) in (" , (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ")) "
lfquery „ lfquery, 'AND ((TYPE) in (', (¯1 ‡,(•œlist_exptype['Form.lsExptype' Œwi 'value';]),',') , '))'        
lfquery „ lfquery, "AND GROUNDFISH.GSDET.MISSION = GROUNDFISH.GSINF.MISSION "
lfquery „ lfquery, "AND GROUNDFISH.GSDET.SETNO = GROUNDFISH.GSINF.SETNO "
lfquery „ lfquery, "ORDER BY GROUNDFISH.GSDET.SETNO, Flen, Clen "    

© Create the alkquery based on user selection for DFO data
alkquery „ "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, FSEX, FLEN, AGE, CLEN, FWT "
alkquery „ alkquery, "FROM GROUNDFISH.GSDET, GROUNDFISH.GSINF "
alkquery „ alkquery, "WHERE ((GROUNDFISH.GSDET.SPEC) = " ,combo_species['Form.lsSpecies' Œwi 'value';1] , ") "
alkquery „ alkquery, "AND ((GROUNDFISH.GSDET.MISSION) in (" , (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ")) "
:IF 0¬½'Form.lsUnitArea' Œwi 'value'
    alkquery „ alkquery, "AND ((AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',') , ")) "
:ENDIF
alkquery „ alkquery, "AND ((STRAT) in (" , (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ")) "
alkquery „ alkquery, 'AND ((TYPE) in (', (¯1 ‡,(•œlist_exptype['Form.lsExptype' Œwi 'value';]),',') , '))'        
alkquery „ alkquery, "AND (FSHNO IS NOT NULL OR FWT IS NOT NULL) "
alkquery „ alkquery, "AND GROUNDFISH.GSDET.MISSION = GROUNDFISH.GSINF.MISSION "
alkquery „ alkquery, "AND GROUNDFISH.GSDET.SETNO = GROUNDFISH.GSINF.SETNO "
alkquery „ alkquery, "ORDER BY GROUNDFISH.GSDET.SETNO, Flen, Clen"    

© Create an alkplus query based on user selection from the main query 
© only if use all experiment types selected for alk modification.
:IF ('Form.fAlk.ckXtype' Œwi 'value') = 1
  alkplusquery „ "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, FSEX, FLEN, AGE, CLEN, FWT "
  alkplusquery „ alkplusquery, "FROM GROUNDFISH.GSDET, GROUNDFISH.GSINF "
  alkplusquery „ alkplusquery, "WHERE ((GROUNDFISH.GSDET.SPEC) = " ,combo_species['Form.lsSpecies' Œwi 'value';1] , ") "
  alkplusquery „ alkplusquery, "AND ((GROUNDFISH.GSDET.MISSION) in (" , (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ")) "
  :IF 0¬½'Form.lsUnitArea' Œwi 'value'
    alkplusquery „ alkplusquery, "AND ((AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',') , ")) "
  :ENDIF
  alkplusquery „ alkplusquery, "AND ((STRAT) in (" , (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ")) "
  alkplusquery „ alkplusquery, 'AND ((TYPE) NOT IN (', (¯1 ‡,(•œlist_exptype['Form.lsExptype' Œwi 'value';]),',') , '))'        
  alkplusquery „ alkplusquery, "AND (FSHNO IS NOT NULL OR FWT IS NOT NULL) "
  alkplusquery „ alkplusquery, "AND GROUNDFISH.GSDET.MISSION = GROUNDFISH.GSINF.MISSION "
  alkplusquery „ alkplusquery, "AND GROUNDFISH.GSDET.SETNO = GROUNDFISH.GSINF.SETNO "
  alkplusquery „ alkplusquery, "ORDER BY GROUNDFISH.GSDET.SETNO, Flen, Clen"    
:ENDIF

© Create optional alk query based on user selection for DFO Data
:IF (((('Form.fAlk.lsAlkStrata' Œwi 'value') ­ 0½0)¬1)Ÿ(((('Form.fAlk.lsAlkUnit' Œwi 'value') ­ 0½0)¬1)))     
  alkmodquery „ "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, FSEX, FLEN, AGE, CLEN, FWT "
  alkmodquery „ alkmodquery, "FROM GROUNDFISH.GSDET, GROUNDFISH.GSINF "
  alkmodquery „ alkmodquery, "WHERE ((SPEC) = " ,combo_species['Form.lsSpecies' Œwi 'value';1] , ") "
  alkmodquery „ alkmodquery, "AND ((GROUNDFISH.GSDET.MISSION) in (" , (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ")) "
  :IF 0¬½'Form.fAlk.lsAlkUnit' Œwi 'value'
    alkmodquery „ alkmodquery , "AND ((AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.fAlk.lsAlkUnit' Œwi 'value';1]),"'"),',') , ")) "
  :ENDIF
  alkmodquery „ alkmodquery, "AND ((STRAT) in (" , (¯1 ‡,("'",(œcombo_strata['Form.fAlk.lsAlkStrata' Œwi 'value';1]),"'"),',') , ")) "
  alkmodquery „ alkmodquery, 'AND ((TYPE) in (', (¯1 ‡,(•œlist_exptype['Form.lsExptype' Œwi 'value';]),',') , '))'
  alkmodquery „ alkmodquery, "AND (FSHNO IS NOT NULL OR FWT IS NOT NULL) "
  alkmodquery „ alkmodquery, "AND GROUNDFISH.GSDET.MISSION = GROUNDFISH.GSINF.MISSION "
  alkmodquery „ alkmodquery, "AND GROUNDFISH.GSDET.SETNO = GROUNDFISH.GSINF.SETNO "
  alkmodquery „ alkmodquery, "ORDER BY GROUNDFISH.GSDET.SETNO, Flen, Clen"    
:ENDIF
    
© Increase the progress bar
'Form.pbProgress' Œwi 'Stepit'

© The wing and distance values are text format in the form.
© By creating and then executing a string with the text from the form
© Converts the data into numeric, scalar data.

–'wing „ ',•('Form.cbSpread' Œwi 'text')
–'distance „ ', •('Form.edDistance' Œwi 'text')


© Use the Stratum Area Table to retrieve the necessary strata data.

Œelx„" POPUP 'Stratum Area Table not found or empty.' ,Œtcnl,'Analysis cannot continue.' ª …ERR2 "

© select all strata
qry „ "SELECT STRAT FROM "
qry „ qry, ('Form.cbStratumTable' Œwi 'text')
'Ado' Œwi 'xExecute > Ado.Rs' (•œqry)
:IF 1= 'Ado.Rs' Œwi 'EOF'
    ERR2: Z„1 ª …leave_function © no table or empty
:ELSE
    all_strat „ ('Ado.Rs' Œwi 'GetRows')[;1]
:ENDIF

sel_strat„combo_strata['Form.lsStrata' Œwi 'value';1] © selected strata

:IF 0¹(sel_strat¹all_strat)  © check for selected stratum not in all startum from area table
    invalid_strat „ (~(sel_strat¹all_strat))/sel_strat
    POPUP 'Selected Stratum not found in Stratum Area Table. ',Œtcnl,•›invalid_strat'Analysis cannot continue.' ª …ERR
:ENDIF

Œelx„" POPUP 'Stratum not found in Stratum Area Table' ª …ERR "

qry „ "SELECT STRAT, AREA FROM "
qry „ qry, ('Form.cbStratumTable' Œwi 'text')
qry „ qry, ' WHERE STRAT in (', (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ') '
qry „ qry, ' ORDER BY STRAT '
'Ado' Œwi 'xExecute > Ado.Rs' (•œqry)
:IF 1= 'Ado.Rs' Œwi 'EOF'
    ERR: Z„1 ª …leave_function © no vaild strata selected
:ELSE
    strata „ 'Ado.Rs' Œwi 'GetRows'
:ENDIF

leave_function:

Œelx„ "ON_ERROR"