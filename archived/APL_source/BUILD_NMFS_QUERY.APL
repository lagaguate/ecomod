Z„BUILD_NMFS_QUERY mission;qry;Œelx;all_strat;sel_strat;invalid_strat

Œelx„ "ON_ERROR"

Z„0

© Initiate and make visible the Progress bar and label.
'Form.pbProgress' Œwi 'value' 0 180 20 0
'Form.lProgress' Œwi 'visible' 1
'Form.pbProgress' Œwi 'visible' 1
'Form.lProgress' Œwi 'caption' 'Building Queries'

infquery„catquery„lfquery„alkquery„alkmodquery„0½0

© Create the infquery based on user selection for NMFS data
infquery „ "SELECT S.STRATUM, CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, 1.75, S.BEGLAT, S.BEGLON, S.AREA "
infquery „ infquery, 'FROM USNEFSC.USS_STATION S '
infquery „ infquery, 'WHERE ((S.CRUISE6) in (', (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ')) '
:IF 0¬½'Form.lsUnitArea' Œwi 'value'
    infquery „ infquery, 'AND ((S.AREA) in (', (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',')  , ')) '
:ENDIF
infquery „ infquery, 'AND ((S.STRATUM) in (', (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ')) '
infquery „ infquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' Œwi 'text'),"'",')) '
infquery „ infquery, 'ORDER BY S.STRATUM, S.STATION'

© Create the catquery based on user selection for NMFS data
catquery „ "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, C.EXPCATCHWT, C.EXPCATCHWT "
catquery „ catquery , "FROM USNEFSC.USS_STATION S, USNEFSC.USS_CATCH C "
catquery „ catquery , "WHERE ((C.SVSPP)= " ,("'",(œcombo_species['Form.lsSpecies' Œwi 'value';1]),"'"),")"
catquery „ catquery , 'AND ((C.CRUISE6) in (',  (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ')) '
:IF 0¬½'Form.lsUnitArea' Œwi 'value'
    catquery „ catquery , "AND ((S.AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',') , ")) "
:ENDIF
catquery „ catquery , "AND ((S.STRATUM) in (" , (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ")) "
catquery „ catquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' Œwi 'text'),"'",')) '
catquery „ catquery , "AND C.CRUISE6 = S.CRUISE6 "
catquery „ catquery , "AND C.STATION = S.STATION "
catquery „ catquery , "ORDER BY C.STATION"

© Create the lfquery based on user selection for NFMS data
lfquery „ "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, L.CATCHSEX, L.LENGTH, L.EXPNUMLEN "
lfquery „ lfquery , "FROM USNEFSC.USS_LENGTHS L, USNEFSC.USS_STATION S "
lfquery „ lfquery , "WHERE ((L.SVSPP) = " ,("'",(œcombo_species['Form.lsSpecies' Œwi 'value';1]),"'"),")"
lfquery „ lfquery , 'AND ((L.CRUISE6) in (',  (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ')) '
:IF 0¬½'Form.lsUnitArea' Œwi 'value'
    lfquery „ lfquery , "AND ((S.AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',') , ")) "
:ENDIF
lfquery „ lfquery , "AND ((S.STRATUM) in (" , (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ")) "
lfquery „ lfquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' Œwi 'text'),"'",')) '
lfquery „ lfquery , "AND L.CRUISE6 = S.CRUISE6 "
lfquery „ lfquery , "AND L.STATION = S.STATION "
lfquery „ lfquery , "ORDER BY L.STATION, L.LENGTH, L.EXPNUMLEN"    

© Create the alkquery based on user selection for NMFS data
alkquery „ "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, D.SEX, D.LENGTH, D.AGE, 1 , D.INDWT * 1000 "
alkquery „ alkquery , "FROM USNEFSC.USS_DETAIL D, USNEFSC.USS_STATION S "
alkquery „ alkquery , "WHERE ((D.SVSPP) =" ,("'",(œcombo_species['Form.lsSpecies' Œwi 'value';1]),"'"),")"
alkquery „ alkquery , 'AND ((D.CRUISE6) in (',  (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ')) '
:IF 0¬½'Form.lsUnitArea' Œwi 'value'
    alkquery „ alkquery , "AND ((S.AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',') , ")) "
:ENDIF
alkquery „ alkquery , "AND ((S.STRATUM) in (" , (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ")) "
alkquery „ alkquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' Œwi 'text'),"'",')) '
alkquery „ alkquery , "AND D.CRUISE6 = S.CRUISE6 "
alkquery „ alkquery , "AND D.STATION = S.STATION "
alkquery „ alkquery , "ORDER BY D.STATION, D.LENGTH"    

© Create an alkplus query based on user selection from the main query 
© only if use all experiment types selected for alk modification.
:IF ('Form.fAlk.ckXtype' Œwi 'value') = 1
    alkplusquery „ "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, D.SEX, D.LENGTH, D.AGE, 1 , D.INDWT * 1000 "
    alkplusquery „ alkplusquery , "FROM USNEFSC.USS_DETAIL D, USNEFSC.USS_STATION S "
    alkplusquery „ alkplusquery , "WHERE ((D.SVSPP) =" ,("'",(œcombo_species['Form.lsSpecies' Œwi 'value';1]),"'"),")"
    alkplusquery „ alkplusquery , 'AND ((D.CRUISE6) in (',  (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ')) '
    :IF 0¬½'Form.lsUnitArea' Œwi 'value'
        alkplusquery „ alkplusquery , "AND ((S.AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.lsUnitArea' Œwi 'value';1]),"'"),',') , ")) "
    :ENDIF
    alkplusquery „ alkplusquery , "AND ((S.STRATUM) in (" , (¯1 ‡,("'",(œcombo_strata['Form.lsStrata' Œwi 'value';1]),"'"),',') , ")) "
    alkplusquery „ alkplusquery , 'AND ((S.SHG)>(',"'",('Form.edExpTypeNMFS' Œwi 'text'),"'",')) '
    alkplusquery „ alkplusquery , "AND D.CRUISE6 = S.CRUISE6 "
    alkplusquery „ alkplusquery , "AND D.STATION = S.STATION "
    alkplusquery „ alkplusquery , "ORDER BY D.STATION, D.LENGTH"    
:ENDIF

© Create optional alk query based on user selection fo NMFS data.
:IF (((('Form.fAlk.lsAlkStrata' Œwi 'value') ­ 0½0)¬1)Ÿ(((('Form.fAlk.lsAlkUnit' Œwi 'value') ­ 0½0)¬1)))          
    alkmodquery „ "SELECT CONCAT(CONCAT(CONCAT(CONCAT(S.SVVESSEL,'-'),S.CRUISE6),'-'),S.STATION) SETNO, D.SEX, D.LENGTH, D.AGE, 1 , D.INDWT * 1000 "
    alkmodquery „ alkmodquery , "FROM USNEFSC.USS_DETAIL D, USNEFSC.USS_STATION S "
    alkmodquery „ alkmodquery , "WHERE ((D.SVSPP) = " ,("'",(œcombo_species['Form.lsSpecies' Œwi 'value';1]),"'"),")"
    alkmodquery „ alkmodquery , 'AND ((D.CRUISE6) in (',  (¯1 ‡,("'",(œcombo_mission[mission Œwi 'value';1]),"'"),',') , ')) '
    :IF 0¬½'Form.fAlk.lsAlkUnit' Œwi 'value'
        alkmodquery „ alkmodquery , "AND ((S.AREA) in (" , (¯1 ‡,("'",(œcombo_area['Form.fAlk.lsAlkUnit' Œwi 'value';1]),"'"),',') , ")) "
    :ENDIF
    alkmodquery „ alkmodquery , "AND ((S.STRATUM) in (" , (¯1 ‡,("'",(œcombo_strata['Form.fAlk.lsAlkStrata' Œwi 'value';1]),"'"),',') , ")) "
    alkmodquery „ alkmodquery , 'AND ((S.SHG)<=(',"'",('Form.edExpTypeNMFS' Œwi 'text'),"'",')) '
    alkmodquery „ alkmodquery , "AND D.CRUISE6 = S.CRUISE6 "
    alkmodquery „ alkmodquery , "AND D.STATION = S.STATION "
    alkmodquery „ alkmodquery , "ORDER BY D.STATION, D.LENGTH"    
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