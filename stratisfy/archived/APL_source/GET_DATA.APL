Z�GET_DATA queries;var;I;J;qry;�elx
�
� Loop through the four main queries then process the alk modification queries and place the results in variables.
� The progress bar and label will be updated as each query is processed.
� queries�(infquery) (catquery) (lfquery) (alkquery)   � the names of the queries to be used

�elx� "ON_ERROR"

var�'inf' 'cat' 'lf' 'det'  � the names of the variables that will store the queried information.

:FOR I :IN �4
    'Form.lProgress' �wi 'caption' ('Querying Datasource: ', I�var) � Progress label updated.
    'Ado' �wi 'xExecute > Ado.Rs' (�I�queries)
    :IF 1= 'Ado.Rs' �wi 'EOF'
        �   (I�var), ' � �'
        :if 4=I � � (I�var), ' � 0 6�0' � :endif
    :ELSE
        � (I�var), ' �''Ado.Rs'' �wi ''GetRows'' '
    :ENDIF
    'Form.pbProgress' �wi 'Stepit' � Progress bar stepped.
:ENDFOR

detplus�0 6�0 � extra detail resulting from ALK modification queries

alk_mod_user � alk_mod_plus � alk_mod_sel � 'N' � indicates yes or No for detail selected for ALK modifications

� User supplied ALK modifications
:IF 0��'Form.fAlk.edAlkTable' �wi 'text'
    �elx� "POPUP 'ALK Modification table not found or invalid. Unable to continue Analysis.' � Z�1 � � NOMODTABLE"
    'Form.lProgress' �wi 'caption' ('Querying Datasource: ', ('Form.fAlk.edAlkTable' �wi 'text')) � Progress label updated.
    qry � "SELECT 1 SETNO,FSEX, FLEN, AGE, CLEN, NULL FWT FROM "
    qry � qry, ('Form.fAlk.edAlkTable' �wi 'text')
    qry � qry, ' WHERE SURVEY in (',  (�1 �,("'",(�combo_mission[mission_field �wi 'value';1]),"'"),',') , ') '
    qry � qry, ' ORDER BY FLEN, CLEN '
    'Ado' �wi 'xExecute > Ado.Rs' (��qry)
    :IF 1� 'Ado.Rs' �wi 'EOF'
         detplus � detplus� ('Ado.Rs' �wi 'GetRows')
         alk_mod_user � 'Y'
    :ENDIF
    'Form.pbProgress' �wi 'Stepit'
:ELSE
    'Form.pbProgress' �wi 'Stepit'
:ENDIF

�elx� "ON_ERROR"

� Select extra detail for alk modifications if use all experiment types checkbox is selected. 
:IF ('Form.fAlk.ckXtype' �wi 'value') = 1
    'Form.lProgress' �wi 'caption' ('Querying Datasource: alkmoddet') � Progress label updated.
    'Ado' �wi 'xExecute > Ado.Rs' (��alkplusquery)
    :IF 1� 'Ado.Rs' �wi 'EOF'
         detplus � detplus� ('Ado.Rs' �wi 'GetRows')
         alk_mod_plus � 'Y'
    :ENDIF
    'Form.pbProgress' �wi 'Stepit'
:ELSE
    'Form.pbProgress' �wi 'Stepit'
:ENDIF

� Database selected ALK modifications
:IF ((~Э'Form.fAlk.lsAlkStrata' �wi 'value') � (~Э'Form.fAlk.lsAlkUnit' �wi 'value'))
    'Form.lProgress' �wi 'caption' ('Querying Datasource: alkmoddet') � Progress label updated.
    'Ado' �wi 'xExecute > Ado.Rs' (��alkmodquery)
    :IF 1� 'Ado.Rs' �wi 'EOF'
        detplus � detplus� ('Ado.Rs' �wi 'GetRows')
        �(alkmoddet)� 'Ado.Rs' �wi 'GetRows'� Query results returned to variable.
         alk_mod_sel � 'Y'
    :ENDIF
    'Form.pbProgress' �wi 'Stepit'
:ELSE
  'Form.pbProgress' �wi 'Stepit'�  'Form.pbProgress' �wi 'Stepit'�  'Form.pbProgress' �wi 'Stepit'
:ENDIF

detplus�((detplus[;1]�det[;1])=0)�[1]detplus � remove any data from age modification array already in main data array

:if 1='Form.opDFO'  �wi 'value' � for DFO data
  :if (combo_species['Form.lsSpecies' �wi 'value';1])=60 � for Herring only
    lf[;3]�(1.0866�lf[;3])+0.95632  � Fork length converted to total length.
    det[;3]�(1.0866�det[;3])+0.95632  � Fork length converted to total length.
    detplus[;3]�(1.0866�detplus[;3])+0.95632  � Fork length converted to total length.
  :endif
:endif


 
� (0<1��lf)/"lf[(0�(��lf[;2]))/��lf[;2];2]�0"      � Change NULL(VT codes) fish sex to 0 for length frequency results
� (0<1��cat)/"cat[(0�(��cat[;2]))/��cat[;2];2]�0"  � Change NULL(VT codes) sample weight 0 for catch results 
� (0<1��cat)/"cat[(0�(��cat[;3]))/��cat[;3];3]�0"  � Change NULL(VT codes) total weight 0 for catch results 
� (0<1��det)/"det[(0�(��det[;2]))/��det[;2];2]�0"  � Change NULL(VT codes) fish sex to 0 for detail results 
� (0<1��det)/"det[(0�(��det[;4]))/��det[;4];4]���"  � Change NULL(VT codes) fish age to APL empty for detail results 
� (0<1��det)/"det[(0�(��det[;6]))/��det[;6];6]���"  � Change NULL(VT codes) fish weight to APL empty for detail results 
� (0<1��detplus)/"detplus[(0�(��detplus[;2]))/��detplus[;2];2]�0"  � Change NULL(VT codes) fish sex to 0 for detail results 
� (0<1��detplus)/"detplus[(0�(��detplus[;4]))/��detplus[;4];4]���"  � Change NULL(VT codes) fish age to APL empty for detail results 
� (0<1��detplus)/"detplus[(0�(��detplus[;6]))/��detplus[;6];6]���"  � Change NULL(VT codes) fish weight to APL empty for detail results 

� Apply vessel net conversions to the count at length of the length frequency
lfconv�0 3�0 � conversion values: vessel, length, vessel_conversion
lfconv_applied � 'N' � indicates conversion was applied to at least 1 number at length
� Apply vessel net conversions to the count at length of the length frequency if selected
:IF 1='Form.opNMFS' �wi 'value' � only applies to US data
  :IF ('Form.ckVesselNetConversion' �wi 'value') = 1
    � Get conversion values for species
    �elx� "POPUP 'Vessel Conversion table not found or invalid. Unable to continue Analysis.' � Z�1 � � NOMODTABLE"
    qry � "SELECT SVVESSEL, LENGTH, VESSEL_CONVERSION FROM USNEFSC.US_VESSEL_NET_CONVERSIONS "
    qry � qry, ' WHERE SVSPP = ',  (�1 �,("'",(�combo_species['Form.lsSpecies' �wi 'value';1]),"'"),',') , ' '
    qry � qry, ' AND SVVESSEL = ',"'",(2��lf[I;1]),"'",' '
    qry � qry, ' ORDER by LENGTH '
    'Ado' �wi 'xExecute > Ado.Rs' (��qry)
    :IF 1� 'Ado.Rs' �wi 'EOF'
      lfconv � lfconv� ('Ado.Rs' �wi 'GetRows')
    :ENDIF
    �elx� "ON_ERROR"
    :IF 0�1��lfconv � no conversions found for this species and vessel 
      lfconv_max_idx�lfconv[;2]�(�/lfconv[;2]) � get index for conversion value of the max length
      :FOR I :in �1��lf � for each length
        lfconv_idx�lfconv[;2]�(lf[I;3]) � find index of conversion value in conversion array using length
        lfconv_applied �'Y' � conversion applied
        :IF lfconv_idx > 1��lfconv � if matching length not found
          lf[I;4] � lf[I;4]�lfconv[lfconv_max_idx;3] � divide number_at_length by conversion at max length
        :ELSE
          lf[I;4] � lf[I;4]�lfconv[lfconv_idx;3] � divide number_at_length by conversion at matching length
        :ENDIF
      :ENDFOR
      
      �:FOR I :in �1��det � for each detail weight
       � lfconv_idx�lfconv[;2]�(det[I;3]) � find index of conversion value in conversion array using length
       � lfconv_applied �'Y' � conversion applied
       � :IF lfconv_idx > 1��lfconv � if matching length not found
       �   det[I;6] � det[I;6]�lfconv[lfconv_max_idx;3] � divide number_at_length by conversion at max length
       � :ELSE
       �   det[I;6] � det[I;6]�lfconv[lfconv_idx;3] � divide number_at_length by conversion at matching length
       � :ENDIF
      �:ENDFOR
      �:FOR I :in �1��detplus � for each detail weight
       � lfconv_idx�lfconv[;2]�(detplus[I;3]) � find index of conversion value in conversion array using length
       � lfconv_applied �'Y' � conversion applied
       � :IF lfconv_idx > 1��lfconv � if matching length not found
       �   detplus[I;6] � detplus[I;6]�lfconv[lfconv_max_idx;3] � divide number_at_length by conversion at max length
       � :ELSE
       �   detplus[I;6] � detplus[I;6]�lfconv[lfconv_idx;3] � divide number_at_length by conversion at matching length
       � :ENDIF
      �:ENDFOR
      �:FOR I :in �1��cat � for each sample weight
      �    cat[I;2] � cat[I;2]�lfconv[lfconv_max_idx;3] � divide number_at_length by conversion at max length
      �    cat[I;3] � cat[I;3]�lfconv[lfconv_max_idx;3] � divide number_at_length by conversion at max length
      �:ENDFOR      
    :ENDIF
  :ENDIF 
:ENDIF  
    
'Form.lProgress' �wi 'caption' 'Processing Data' � Progress label updated.

Z�0
NOMODTABLE:
�elx� "ON_ERROR"