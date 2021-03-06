OPEN_NMFS;tmp;I;uid;pwd;dsn;r
� Setup selection lists.
� NFMS Data Only. 

'Form.lProgress' �wi 'visible' 1
'Form.lProgress' �wi 'caption' 'Creating Selection Lists'

'Form' �wi 'pointer' 11 � Hourglass pointer. 

:if 1�(tmp��'Form.s1' �wi 'children')
    :for I :in �1�1+�tmp
        ('Form.s1.pg',(�I)) �wi 'Delete'
    :endfor
:endif
'Form.s1' �wi 'value' 1

'Form.cbSpread' �wi 'value' 2
'Form.lExptype' �wi 'visible' 0
'Form.lsExptype' �wi 'visible' 0
'Form.lExpTypeNMFS' �wi 'visible' 1
'Form.llessorequal' �wi 'visible' 1
'Form.edExpTypeNMFS' �wi 'visible' 1
'Form.ckVesselNetConversion' �wi 'visible' 1

� Set style for mission list box.
'Form.s1.pg1.lsMission' �wi 'style' (0 16 128)

� Retrieve the data for the Mission combo box.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT CRUISE6 FROM USNEFSC.STRANL_CRUISE ORDER BY CRUISE6'

combo_mission � 'Ado.Rs' �wi 'GetRows'
combo_mission�combo_mission[(1=��combo_mission[;1])/��combo_mission[;1];] � Remove null values
'Form.s1.pg1.lsMission' �wi 'list' (combo_mission[;1])

� Retrieve the data for the Area combo box.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT AREA FROM USNEFSC.STRANL_AREA ORDER BY AREA'
combo_area � 'Ado.Rs' �wi 'GetRows'
combo_area�combo_area[(1=��combo_area[;1])/��combo_area[;1];] � Remove null values
'Form.lsUnitArea' �wi 'list' (combo_area[;1])
'Form.fAlk.lsAlkUnit' �wi 'list' (combo_area[;1])

� Retrieve the data for the Strata combo box.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT DISTINCT(STRATUM) FROM USNEFSC.STRANL_STRATUM ORDER BY STRATUM'
combo_strata � 'Ado.Rs' �wi 'GetRows'
combo_strata�combo_strata[(1=��combo_strata[;1])/��combo_strata[;1];]
'Form.lsStrata' �wi 'list' (combo_strata[;1])
'Form.fAlk.lsAlkStrata' �wi 'list' (combo_strata[;1])

� Retrieve the data for the Species combo box.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT DISTINCT(SPEC), (CNAME), (LGRP), (LFSEXED) FROM USNEFSC.USSPEC ORDER BY CNAME'
combo_species � 'Ado.Rs' �wi 'GetRows'
combo_species � combo_species[(~(�'9999')��combo_species[;1])/��combo_species[;1];] � get rid of 'allspecies'
'Form.lsSpecies' �wi 'list' (combo_species[;2])
'Form' �wi 'pointer' 0 � 

'Form' �wi 'pointer' 0 � Switch back to regular pointer.

'Form.lProgress' �wi 'visible' 0
'Form.lProgress' �wi 'caption' ''