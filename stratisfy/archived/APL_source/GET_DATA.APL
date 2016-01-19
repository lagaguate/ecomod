Z„GET_DATA queries;var;I;J;qry;Œelx
©
© Loop through the four main queries then process the alk modification queries and place the results in variables.
© The progress bar and label will be updated as each query is processed.
© queries„(infquery) (catquery) (lfquery) (alkquery)   © the names of the queries to be used

Œelx„ "ON_ERROR"

var„'inf' 'cat' 'lf' 'det'  © the names of the variables that will store the queried information.

:FOR I :IN ¼4
    'Form.lProgress' Œwi 'caption' ('Querying Datasource: ', Iœvar) © Progress label updated.
    'Ado' Œwi 'xExecute > Ado.Rs' (•Iœqueries)
    :IF 1= 'Ado.Rs' Œwi 'EOF'
        –   (Iœvar), ' „ Ð'
        :if 4=I ª – (Iœvar), ' „ 0 6½0' ª :endif
    :ELSE
        – (Iœvar), ' „''Ado.Rs'' Œwi ''GetRows'' '
    :ENDIF
    'Form.pbProgress' Œwi 'Stepit' © Progress bar stepped.
:ENDFOR

detplus„0 6½0 © extra detail resulting from ALK modification queries

alk_mod_user „ alk_mod_plus „ alk_mod_sel „ 'N' © indicates yes or No for detail selected for ALK modifications

© User supplied ALK modifications
:IF 0¬½'Form.fAlk.edAlkTable' Œwi 'text'
    Œelx„ "POPUP 'ALK Modification table not found or invalid. Unable to continue Analysis.' ª Z„1 ª … NOMODTABLE"
    'Form.lProgress' Œwi 'caption' ('Querying Datasource: ', ('Form.fAlk.edAlkTable' Œwi 'text')) © Progress label updated.
    qry „ "SELECT 1 SETNO,FSEX, FLEN, AGE, CLEN, NULL FWT FROM "
    qry „ qry, ('Form.fAlk.edAlkTable' Œwi 'text')
    qry „ qry, ' WHERE SURVEY in (',  (¯1 ‡,("'",(œcombo_mission[mission_field Œwi 'value';1]),"'"),',') , ') '
    qry „ qry, ' ORDER BY FLEN, CLEN '
    'Ado' Œwi 'xExecute > Ado.Rs' (•œqry)
    :IF 1¬ 'Ado.Rs' Œwi 'EOF'
         detplus „ detplus® ('Ado.Rs' Œwi 'GetRows')
         alk_mod_user „ 'Y'
    :ENDIF
    'Form.pbProgress' Œwi 'Stepit'
:ELSE
    'Form.pbProgress' Œwi 'Stepit'
:ENDIF

Œelx„ "ON_ERROR"

© Select extra detail for alk modifications if use all experiment types checkbox is selected. 
:IF ('Form.fAlk.ckXtype' Œwi 'value') = 1
    'Form.lProgress' Œwi 'caption' ('Querying Datasource: alkmoddet') © Progress label updated.
    'Ado' Œwi 'xExecute > Ado.Rs' (•œalkplusquery)
    :IF 1¬ 'Ado.Rs' Œwi 'EOF'
         detplus „ detplus® ('Ado.Rs' Œwi 'GetRows')
         alk_mod_plus „ 'Y'
    :ENDIF
    'Form.pbProgress' Œwi 'Stepit'
:ELSE
    'Form.pbProgress' Œwi 'Stepit'
:ENDIF

© Database selected ALK modifications
:IF ((~Ð­'Form.fAlk.lsAlkStrata' Œwi 'value') Ÿ (~Ð­'Form.fAlk.lsAlkUnit' Œwi 'value'))
    'Form.lProgress' Œwi 'caption' ('Querying Datasource: alkmoddet') © Progress label updated.
    'Ado' Œwi 'xExecute > Ado.Rs' (•œalkmodquery)
    :IF 1¬ 'Ado.Rs' Œwi 'EOF'
        detplus „ detplus® ('Ado.Rs' Œwi 'GetRows')
        ©(alkmoddet)„ 'Ado.Rs' Œwi 'GetRows'© Query results returned to variable.
         alk_mod_sel „ 'Y'
    :ENDIF
    'Form.pbProgress' Œwi 'Stepit'
:ELSE
  'Form.pbProgress' Œwi 'Stepit'ª  'Form.pbProgress' Œwi 'Stepit'ª  'Form.pbProgress' Œwi 'Stepit'
:ENDIF

detplus„((detplus[;1]¹det[;1])=0)š[1]detplus © remove any data from age modification array already in main data array

:if 1='Form.opDFO'  Œwi 'value' © for DFO data
  :if (combo_species['Form.lsSpecies' Œwi 'value';1])=60 © for Herring only
    lf[;3]„(1.0866×lf[;3])+0.95632  © Fork length converted to total length.
    det[;3]„(1.0866×det[;3])+0.95632  © Fork length converted to total length.
    detplus[;3]„(1.0866×detplus[;3])+0.95632  © Fork length converted to total length.
  :endif
:endif


 
– (0<1†½lf)/"lf[(0¬(­¨lf[;2]))/¼½lf[;2];2]„0"      © Change NULL(VT codes) fish sex to 0 for length frequency results
– (0<1†½cat)/"cat[(0¬(­¨cat[;2]))/¼½cat[;2];2]„0"  © Change NULL(VT codes) sample weight 0 for catch results 
– (0<1†½cat)/"cat[(0¬(­¨cat[;3]))/¼½cat[;3];3]„0"  © Change NULL(VT codes) total weight 0 for catch results 
– (0<1†½det)/"det[(0¬(­¨det[;2]))/¼½det[;2];2]„0"  © Change NULL(VT codes) fish sex to 0 for detail results 
– (0<1†½det)/"det[(0¬(­¨det[;4]))/¼½det[;4];4]„›Ð"  © Change NULL(VT codes) fish age to APL empty for detail results 
– (0<1†½det)/"det[(0¬(­¨det[;6]))/¼½det[;6];6]„›Ð"  © Change NULL(VT codes) fish weight to APL empty for detail results 
– (0<1†½detplus)/"detplus[(0¬(­¨detplus[;2]))/¼½detplus[;2];2]„0"  © Change NULL(VT codes) fish sex to 0 for detail results 
– (0<1†½detplus)/"detplus[(0¬(­¨detplus[;4]))/¼½detplus[;4];4]„›Ð"  © Change NULL(VT codes) fish age to APL empty for detail results 
– (0<1†½detplus)/"detplus[(0¬(­¨detplus[;6]))/¼½detplus[;6];6]„›Ð"  © Change NULL(VT codes) fish weight to APL empty for detail results 

© Apply vessel net conversions to the count at length of the length frequency
lfconv„0 3½0 © conversion values: vessel, length, vessel_conversion
lfconv_applied „ 'N' © indicates conversion was applied to at least 1 number at length
© Apply vessel net conversions to the count at length of the length frequency if selected
:IF 1='Form.opNMFS' Œwi 'value' © only applies to US data
  :IF ('Form.ckVesselNetConversion' Œwi 'value') = 1
    © Get conversion values for species
    Œelx„ "POPUP 'Vessel Conversion table not found or invalid. Unable to continue Analysis.' ª Z„1 ª … NOMODTABLE"
    qry „ "SELECT SVVESSEL, LENGTH, VESSEL_CONVERSION FROM USNEFSC.US_VESSEL_NET_CONVERSIONS "
    qry „ qry, ' WHERE SVSPP = ',  (¯1 ‡,("'",(œcombo_species['Form.lsSpecies' Œwi 'value';1]),"'"),',') , ' '
    qry „ qry, ' AND SVVESSEL = ',"'",(2†œlf[I;1]),"'",' '
    qry „ qry, ' ORDER by LENGTH '
    'Ado' Œwi 'xExecute > Ado.Rs' (•œqry)
    :IF 1¬ 'Ado.Rs' Œwi 'EOF'
      lfconv „ lfconv® ('Ado.Rs' Œwi 'GetRows')
    :ENDIF
    Œelx„ "ON_ERROR"
    :IF 0¬1†½lfconv © no conversions found for this species and vessel 
      lfconv_max_idx„lfconv[;2]¼(—/lfconv[;2]) © get index for conversion value of the max length
      :FOR I :in ¼1†½lf © for each length
        lfconv_idx„lfconv[;2]¼(lf[I;3]) © find index of conversion value in conversion array using length
        lfconv_applied „'Y' © conversion applied
        :IF lfconv_idx > 1†½lfconv © if matching length not found
          lf[I;4] „ lf[I;4]÷lfconv[lfconv_max_idx;3] © divide number_at_length by conversion at max length
        :ELSE
          lf[I;4] „ lf[I;4]÷lfconv[lfconv_idx;3] © divide number_at_length by conversion at matching length
        :ENDIF
      :ENDFOR
      
      ©:FOR I :in ¼1†½det © for each detail weight
       © lfconv_idx„lfconv[;2]¼(det[I;3]) © find index of conversion value in conversion array using length
       © lfconv_applied „'Y' © conversion applied
       © :IF lfconv_idx > 1†½lfconv © if matching length not found
       ©   det[I;6] „ det[I;6]÷lfconv[lfconv_max_idx;3] © divide number_at_length by conversion at max length
       © :ELSE
       ©   det[I;6] „ det[I;6]÷lfconv[lfconv_idx;3] © divide number_at_length by conversion at matching length
       © :ENDIF
      ©:ENDFOR
      ©:FOR I :in ¼1†½detplus © for each detail weight
       © lfconv_idx„lfconv[;2]¼(detplus[I;3]) © find index of conversion value in conversion array using length
       © lfconv_applied „'Y' © conversion applied
       © :IF lfconv_idx > 1†½lfconv © if matching length not found
       ©   detplus[I;6] „ detplus[I;6]÷lfconv[lfconv_max_idx;3] © divide number_at_length by conversion at max length
       © :ELSE
       ©   detplus[I;6] „ detplus[I;6]÷lfconv[lfconv_idx;3] © divide number_at_length by conversion at matching length
       © :ENDIF
      ©:ENDFOR
      ©:FOR I :in ¼1†½cat © for each sample weight
      ©    cat[I;2] „ cat[I;2]÷lfconv[lfconv_max_idx;3] © divide number_at_length by conversion at max length
      ©    cat[I;3] „ cat[I;3]÷lfconv[lfconv_max_idx;3] © divide number_at_length by conversion at max length
      ©:ENDFOR      
    :ENDIF
  :ENDIF 
:ENDIF  
    
'Form.lProgress' Œwi 'caption' 'Processing Data' © Progress label updated.

Z„0
NOMODTABLE:
Œelx„ "ON_ERROR"