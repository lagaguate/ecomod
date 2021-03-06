Z�BUILD_NMFS_QUERY mission;qry;�elx;all_strat;sel_strat;invalid_strat

�elx� "ON_ERROR"

Z�0

� Initiate and make visible the Progress bar and label.
'Form.pbProgress' �wi 'value' 0 180 20 0
'Form.lProgress' �wi 'visible' 1
'Form.pbProgress' �wi 'visible' 1
'Form.lProgress' �wi 'caption' 'Building Queries'

infquery�catquery�lfquery�alkquery�alkmodquery�0�0

� Create the infquery based on user selection for NMFS data
infquery � "SELECT S.STRATUM, CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, 1.75, S.BEGLAT, S.BEGLON, S.AREA "
infquery � infquery, 'FROM USNEFSC.USS_STATION S '
infquery � infquery, 'WHERE ((S.CRUISE6) in (', (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ')) '
:IF 0��'Form.lsUnitArea' �wi 'value'
    infquery � infquery, 'AND ((S.AREA) in (', (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',')  , ')) '
:ENDIF
infquery � infquery, 'AND ((S.STRATUM) in (', (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ')) '
infquery � infquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' �wi 'text'),"'",')) '
infquery � infquery, 'ORDER BY S.STRATUM, S.STATION'

� Create the catquery based on user selection for NMFS data
catquery � "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, C.EXPCATCHWT, C.EXPCATCHWT "
catquery � catquery , "FROM USNEFSC.USS_STATION S, USNEFSC.USS_CATCH C "
catquery � catquery , "WHERE ((C.SVSPP)= " ,("'",(�combo_species['Form.lsSpecies' �wi 'value';1]),"'"),")"
catquery � catquery , 'AND ((C.CRUISE6) in (',  (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ')) '
:IF 0��'Form.lsUnitArea' �wi 'value'
    catquery � catquery , "AND ((S.AREA) in (" , (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',') , ")) "
:ENDIF
catquery � catquery , "AND ((S.STRATUM) in (" , (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ")) "
catquery � catquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' �wi 'text'),"'",')) '
catquery � catquery , "AND C.CRUISE6 = S.CRUISE6 "
catquery � catquery , "AND C.STATION = S.STATION "
catquery � catquery , "ORDER BY C.STATION"

� Create the lfquery based on user selection for NFMS data
lfquery � "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, L.CATCHSEX, L.LENGTH, L.EXPNUMLEN "
lfquery � lfquery , "FROM USNEFSC.USS_LENGTHS L, USNEFSC.USS_STATION S "
lfquery � lfquery , "WHERE ((L.SVSPP) = " ,("'",(�combo_species['Form.lsSpecies' �wi 'value';1]),"'"),")"
lfquery � lfquery , 'AND ((L.CRUISE6) in (',  (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ')) '
:IF 0��'Form.lsUnitArea' �wi 'value'
    lfquery � lfquery , "AND ((S.AREA) in (" , (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',') , ")) "
:ENDIF
lfquery � lfquery , "AND ((S.STRATUM) in (" , (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ")) "
lfquery � lfquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' �wi 'text'),"'",')) '
lfquery � lfquery , "AND L.CRUISE6 = S.CRUISE6 "
lfquery � lfquery , "AND L.STATION = S.STATION "
lfquery � lfquery , "ORDER BY L.STATION, L.LENGTH, L.EXPNUMLEN"    

� Create the alkquery based on user selection for NMFS data
alkquery � "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, D.SEX, D.LENGTH, D.AGE, 1 , D.INDWT * 1000 "
alkquery � alkquery , "FROM USNEFSC.USS_DETAIL D, USNEFSC.USS_STATION S "
alkquery � alkquery , "WHERE ((D.SVSPP) =" ,("'",(�combo_species['Form.lsSpecies' �wi 'value';1]),"'"),")"
alkquery � alkquery , 'AND ((D.CRUISE6) in (',  (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ')) '
:IF 0��'Form.lsUnitArea' �wi 'value'
    alkquery � alkquery , "AND ((S.AREA) in (" , (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',') , ")) "
:ENDIF
alkquery � alkquery , "AND ((S.STRATUM) in (" , (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ")) "
alkquery � alkquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' �wi 'text'),"'",')) '
alkquery � alkquery , "AND D.CRUISE6 = S.CRUISE6 "
alkquery � alkquery , "AND D.STATION = S.STATION "
alkquery � alkquery , "ORDER BY D.STATION, D.LENGTH"    

� Create an alkplus query based on user selection from the main query 
� only if use all experiment types selected for alk modification.
:IF ('Form.fAlk.ckXtype' �wi 'value') = 1
    alkplusquery � "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, D.SEX, D.LENGTH, D.AGE, 1 , D.INDWT * 1000 "
    alkplusquery � alkplusquery , "FROM USNEFSC.USS_DETAIL D, USNEFSC.USS_STATION S "
    alkplusquery � alkplusquery , "WHERE ((D.SVSPP) =" ,("'",(�combo_species['Form.lsSpecies' �wi 'value';1]),"'"),")"
    alkplusquery � alkplusquery , 'AND ((D.CRUISE6) in (',  (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ')) '
    :IF 0��'Form.lsUnitArea' �wi 'value'
        alkplusquery � alkplusquery , "AND ((S.AREA) in (" , (�1 �,("'",(�combo_area['Form.lsUnitArea' �wi 'value';1]),"'"),',') , ")) "
    :ENDIF
    alkplusquery � alkplusquery , "AND ((S.STRATUM) in (" , (�1 �,("'",(�combo_strata['Form.lsStrata' �wi 'value';1]),"'"),',') , ")) "
    alkplusquery � alkplusquery , 'AND ((S.SHG)>(',"'",('Form.edExpTypeNMFS' �wi 'text'),"'",')) '
    alkplusquery � alkplusquery , "AND D.CRUISE6 = S.CRUISE6 "
    alkplusquery � alkplusquery , "AND D.STATION = S.STATION "
    alkplusquery � alkplusquery , "ORDER BY D.STATION, D.LENGTH"    
:ENDIF

� Create optional alk query based on user selection fo NMFS data.
:IF (((('Form.fAlk.lsAlkStrata' �wi 'value') � 0�0)�1)�(((('Form.fAlk.lsAlkUnit' �wi 'value') � 0�0)�1)))          
    alkmodquery � "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, D.SEX, D.LENGTH, D.AGE, 1 , D.INDWT * 1000 "
    alkmodquery � alkmodquery , "FROM USNEFSC.USS_DETAIL D, USNEFSC.USS_STATION S "
    alkmodquery � alkmodquery , "WHERE ((D.SVSPP) = " ,("'",(�combo_species['Form.lsSpecies' �wi 'value';1]),"'"),")"
    alkmodquery � alkmodquery , 'AND ((D.CRUISE6) in (',  (�1 �,("'",(�combo_mission[mission �wi 'value';1]),"'"),',') , ')) '
    :IF 0��'Form.fAlk.lsAlkUnit' �wi 'value'
        alkmodquery � alkmodquery , "AND ((S.AREA) in (" , (�1 �,("'",(�combo_area['Form.fAlk.lsAlkUnit' �wi 'value';1]),"'"),',') , ")) "
    :ENDIF
    alkmodquery � alkmodquery , "AND ((S.STRATUM) in (" , (�1 �,("'",(�combo_strata['Form.fAlk.lsAlkStrata' �wi 'value';1]),"'"),',') , ")) "
    alkmodquery � alkmodquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' �wi 'text'),"'",')) '
    alkmodquery � alkmodquery , "AND D.CRUISE6 = S.CRUISE6 "
    alkmodquery � alkmodquery , "AND D.STATION = S.STATION "
    alkmodquery � alkmodquery , "ORDER BY D.STATION, D.LENGTH"    
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