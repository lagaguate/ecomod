Z�BUILD_DFO_QUERY mission;qry;�elx;all_strat;sel_strat;invalid_strat

�elx� "ON_ERROR"

Z�0

� Initiate and make visible the Progress bar and label.
'Form.pbProgress' �wi 'value' 0 180 20 0
'Form.lProgress' �wi 'visible' 1
'Form.pbProgress' �wi 'visible' 1
'Form.lProgress' �wi 'caption' 'Building Queries'

infquery�catquery�lfquery�alkquery�alkmodquery�alkplusquery�0�0

� Create the infquery based on user selection for DFO data
infquery � "SELECT STRAT, CONCAT(CONCAT(MISSION,'-'),SETNO) SETNO, DIST, SLAT, SLONG, AREA "
infquery � infquery, 'FROM GROUNDFISH.GSINF '
infquery � infquery, 'WHERE ((MISSION) in (', (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ')) '
:IF 0��'Form.lsUnitArea' �wi 'value'
    infquery � infquery, 'AND ((AREA) in (', (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',')  , ')) '
:ENDIF
infquery � infquery, 'AND ((STRAT) in (', (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ')) '
infquery � infquery, 'AND ((TYPE) in (', (�1 �,(��list_exptype['Form.lsExptype' �wi 'value';]),',') , '))'
infquery � infquery, ' ORDER BY STRAT, SETNO'

� Create the catquery based on user selection for DFO data
catquery � "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, SAMPWGT, TOTWGT, SIZE_CLASS "
catquery � catquery, "FROM GROUNDFISH.GSINF, GROUNDFISH.GSCAT "
catquery � catquery, "WHERE ((SPEC)= " , combo_species['Form.lsSpecies' �wi 'value';1] , ") "
catquery � catquery, "AND ((GROUNDFISH.GSCAT.MISSION) in (" ,  (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',')   , ")) "
:IF 0��'Form.lsUnitArea' �wi 'value'
    catquery � catquery, "AND ((AREA) in (" , (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',') , ")) "
:ENDIF
catquery � catquery, "AND ((STRAT) in (" , (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ")) "
catquery � catquery, 'AND ((TYPE) in (', (�1 �,(��list_exptype['Form.lsExptype' �wi 'value';]),',') , '))'        
catquery � catquery, "AND GROUNDFISH.GSCAT.MISSION = GROUNDFISH.GSINF.MISSION "
catquery � catquery, "AND GROUNDFISH.GSCAT.SETNO = GROUNDFISH.GSINF.SETNO "
catquery � catquery, "ORDER BY GROUNDFISH.GSCAT.SETNO "

� Create the lfquery based on user selection for DFO data    
lfquery � "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, FSEX, FLEN, CLEN, SIZE_CLASS "
lfquery � lfquery, "FROM GROUNDFISH.GSDET, GROUNDFISH.GSINF "
lfquery � lfquery, "WHERE ((SPEC) = " ,combo_species['Form.lsSpecies' �wi 'value';1] , ") "
lfquery � lfquery, "AND ((GROUNDFISH.GSDET.MISSION) in (" , (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ")) "
:IF 0��'Form.lsUnitArea' �wi 'value'
    lfquery � lfquery, "AND ((AREA) in (" , (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',') , ")) "
:ENDIF
lfquery � lfquery, "AND ((STRAT) in (" , (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ")) "
lfquery � lfquery, 'AND ((TYPE) in (', (�1 �,(��list_exptype['Form.lsExptype' �wi 'value';]),',') , '))'        
lfquery � lfquery, "AND GROUNDFISH.GSDET.MISSION = GROUNDFISH.GSINF.MISSION "
lfquery � lfquery, "AND GROUNDFISH.GSDET.SETNO = GROUNDFISH.GSINF.SETNO "
lfquery � lfquery, "ORDER BY GROUNDFISH.GSDET.SETNO, Flen, Clen "    

� Create the alkquery based on user selection for DFO data
alkquery � "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, FSEX, FLEN, AGE, CLEN, FWT "
alkquery � alkquery, "FROM GROUNDFISH.GSDET, GROUNDFISH.GSINF "
alkquery � alkquery, "WHERE ((GROUNDFISH.GSDET.SPEC) = " ,combo_species['Form.lsSpecies' �wi 'value';1] , ") "
alkquery � alkquery, "AND ((GROUNDFISH.GSDET.MISSION) in (" , (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ")) "
:IF 0��'Form.lsUnitArea' �wi 'value'
    alkquery � alkquery, "AND ((AREA) in (" , (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',') , ")) "
:ENDIF
alkquery � alkquery, "AND ((STRAT) in (" , (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ")) "
alkquery � alkquery, 'AND ((TYPE) in (', (�1 �,(��list_exptype['Form.lsExptype' �wi 'value';]),',') , '))'        
alkquery � alkquery, "AND (FSHNO IS NOT NULL OR FWT IS NOT NULL) "
alkquery � alkquery, "AND GROUNDFISH.GSDET.MISSION = GROUNDFISH.GSINF.MISSION "
alkquery � alkquery, "AND GROUNDFISH.GSDET.SETNO = GROUNDFISH.GSINF.SETNO "
alkquery � alkquery, "ORDER BY GROUNDFISH.GSDET.SETNO, Flen, Clen"    

� Create an alkplus query based on user selection from the main query 
� only if use all experiment types selected for alk modification.
:IF ('Form.fAlk.ckXtype' �wi 'value') = 1
  alkplusquery � "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, FSEX, FLEN, AGE, CLEN, FWT "
  alkplusquery � alkplusquery, "FROM GROUNDFISH.GSDET, GROUNDFISH.GSINF "
  alkplusquery � alkplusquery, "WHERE ((GROUNDFISH.GSDET.SPEC) = " ,combo_species['Form.lsSpecies' �wi 'value';1] , ") "
  alkplusquery � alkplusquery, "AND ((GROUNDFISH.GSDET.MISSION) in (" , (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ")) "
  :IF 0��'Form.lsUnitArea' �wi 'value'
    alkplusquery � alkplusquery, "AND ((AREA) in (" , (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',') , ")) "
  :ENDIF
  alkplusquery � alkplusquery, "AND ((STRAT) in (" , (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ")) "
  alkplusquery � alkplusquery, 'AND ((TYPE) NOT IN (', (�1 �,(��list_exptype['Form.lsExptype' �wi 'value';]),',') , '))'        
  alkplusquery � alkplusquery, "AND (FSHNO IS NOT NULL OR FWT IS NOT NULL) "
  alkplusquery � alkplusquery, "AND GROUNDFISH.GSDET.MISSION = GROUNDFISH.GSINF.MISSION "
  alkplusquery � alkplusquery, "AND GROUNDFISH.GSDET.SETNO = GROUNDFISH.GSINF.SETNO "
  alkplusquery � alkplusquery, "ORDER BY GROUNDFISH.GSDET.SETNO, Flen, Clen"    
:ENDIF

� Create optional alk query based on user selection for DFO Data
:IF (((('Form.fAlk.lsAlkStrata' �wi 'value') � 0�0)�1)�(((('Form.fAlk.lsAlkUnit' �wi 'value') � 0�0)�1)))     
  alkmodquery � "SELECT CONCAT(CONCAT(GROUNDFISH.GSINF.MISSION,'-'),GROUNDFISH.GSINF.SETNO) SETNO, FSEX, FLEN, AGE, CLEN, FWT "
  alkmodquery � alkmodquery, "FROM GROUNDFISH.GSDET, GROUNDFISH.GSINF "
  alkmodquery � alkmodquery, "WHERE ((SPEC) = " ,combo_species['Form.lsSpecies' �wi 'value';1] , ") "
  alkmodquery � alkmodquery, "AND ((GROUNDFISH.GSDET.MISSION) in (" , (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ")) "
  :IF 0��'Form.fAlk.lsAlkUnit' �wi 'value'
    alkmodquery � alkmodquery , "AND ((AREA) in (" , (�1 �,("'",(�combo_area['Form.fAlk.lsAlkUnit' �wi 'value';1]),"'"),',') , ")) "
  :ENDIF
  alkmodquery � alkmodquery, "AND ((STRAT) in (" , (�1 �,("'",(�combo_strata['Form.fAlk.lsAlkStrata' �wi 'value';1]),"'"),',') , ")) "
  alkmodquery � alkmodquery, 'AND ((TYPE) in (', (�1 �,(��list_exptype['Form.lsExptype' �wi 'value';]),',') , '))'
  alkmodquery � alkmodquery, "AND (FSHNO IS NOT NULL OR FWT IS NOT NULL) "
  alkmodquery � alkmodquery, "AND GROUNDFISH.GSDET.MISSION = GROUNDFISH.GSINF.MISSION "
  alkmodquery � alkmodquery, "AND GROUNDFISH.GSDET.SETNO = GROUNDFISH.GSINF.SETNO "
  alkmodquery � alkmodquery, "ORDER BY GROUNDFISH.GSDET.SETNO, Flen, Clen"    
:ENDIF
    
� Increase the progress bar
'Form.pbProgress' �wi 'Stepit'

� The wing and distance values are text format in the form.
� By creating and then executing a string with the text from the form
� Converts the data into numeric, scalar data.

�'wing � ',�('Form.cbSpread' �wi 'text')
�'distance � ', �('Form.edDistance' �wi 'text')


� Use the Stratum Area Table to retrieve the necessary strata data.

�elx�" POPUP 'Stratum Area Table not found or empty.' ,�tcnl,'Analysis cannot continue.' � �ERR2 "

� select all strata
qry � "SELECT STRAT FROM "
qry � qry, ('Form.cbStratumTable' �wi 'text')
'Ado' �wi 'xExecute > Ado.Rs' (��qry)
:IF 1= 'Ado.Rs' �wi 'EOF'
    ERR2: Z�1 � �leave_function � no table or empty
:ELSE
    all_strat � ('Ado.Rs' �wi 'GetRows')[;1]
:ENDIF

sel_strat�combo_strata['Form.lsStrata' �wi 'value';1] � selected strata

:IF 0�(sel_strat�all_strat)  � check for selected stratum not in all startum from area table
    invalid_strat � (~(sel_strat�all_strat))/sel_strat
    POPUP 'Selected Stratum not found in Stratum Area Table. ',�tcnl,��invalid_strat'Analysis cannot continue.' � �ERR
:ENDIF

�elx�" POPUP 'Stratum not found in Stratum Area Table' � �ERR "

qry � "SELECT STRAT, AREA FROM "
qry � qry, ('Form.cbStratumTable' �wi 'text')
qry � qry, ' WHERE STRAT in (', (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ') '
qry � qry, ' ORDER BY STRAT '
'Ado' �wi 'xExecute > Ado.Rs' (��qry)
:IF 1= 'Ado.Rs' �wi 'EOF'
    ERR: Z�1 � �leave_function � no vaild strata selected
:ELSE
    strata � 'Ado.Rs' �wi 'GetRows'
:ENDIF

leave_function:

�elx� "ON_ERROR"