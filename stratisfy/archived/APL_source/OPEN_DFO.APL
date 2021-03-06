OPEN_DFO;tmp;I;uid;pwd;dsn;r
� Setup selection lists.
� DFO Data Only. 

'Form.lProgress' �wi 'visible' 1
'Form.lProgress' �wi 'caption' 'Creating Selection Lists'

'Form' �wi 'pointer' 11 � Hourglass pointer. 

:if 1�(tmp��'Form.s1' �wi 'children')
    :for I :in �1�1+�tmp
        ('Form.s1.pg',(�I)) �wi 'Delete'
    :endfor
:endif
'Form.s1' �wi 'value' 1

'Form.cbSpread' �wi 'value' 1
'Form.lExptype' �wi 'visible' 1
'Form.lsExptype' �wi 'visible' 1
'Form.lExpTypeNMFS' �wi 'visible' 0
'Form.llessorequal' �wi 'visible' 0
'Form.edExpTypeNMFS' �wi 'visible' 0
'Form.ckVesselNetConversion' �wi 'visible' 0

� Set style for mission list box.
'Form.s1.pg1.lsMission' �wi 'style' (1 16 128)

� Retrieve the data for the Mission combo box.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT DISTINCT(MISSION) FROM GROUNDFISH.GSINF ORDER BY MISSION'

combo_mission � 'Ado.Rs' �wi 'GetRows'
'Form.s1.pg1.lsMission' �wi 'list' (combo_mission[;1])

� Retrieve the data for the Area combo box.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT DISTINCT(AREA) FROM GROUNDFISH.GSINF ORDER BY AREA'
combo_area � 'Ado.Rs' �wi 'GetRows'
combo_area�combo_area[(1���combo_area[;1])/��combo_area[;1];] � remove nested VT
'Form.lsUnitArea' �wi 'list' (combo_area[;1])
'Form.fAlk.lsAlkUnit' �wi 'list' (combo_area[;1])

� Retrieve the data for the Strata combo box.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT DISTINCT(STRAT) FROM GROUNDFISH.GSINF ORDER BY STRAT'
combo_strata � 'Ado.Rs' �wi 'GetRows'
combo_strata�combo_strata[(1���combo_strata[;1])/��combo_strata[;1];] � remove nested VT
'Form.lsStrata' �wi 'list' (combo_strata[;1])
'Form.fAlk.lsAlkStrata' �wi 'list' (combo_strata[;1])

� Retrieve the data for the Species combo box.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT DISTINCT(SPEC), (CNAME), (LGRP), (LFSEXED) FROM GROUNDFISH.GSSPEC ORDER BY CNAME'
combo_species � 'Ado.Rs' �wi 'GetRows'
combo_species � combo_species[(9999�combo_species[;1])/��combo_species[;1];] � get rid of 'allspecies'
'Form.lsSpecies' �wi 'list' (combo_species[;2])

� Retrieve the data for the Exptype list.
'Ado' �wi 'xExecute > Ado.Rs' 'SELECT DISTINCT(TYPE) FROM GROUNDFISH.GSINF ORDER BY TYPE'
list_exptype � 'Ado.Rs' �wi 'GetRows'
list_exptype�list_exptype[(1���list_exptype[;1])/��list_exptype[;1];] � remove nested VT
'Form.lsExptype' �wi 'list' (�list_exptype)
'Form.lsExptype' �wi 'value' ((1=list_exptype[;1])/��list_exptype[;1])

'Form' �wi 'pointer' 0 � Switch back to regular pointer.

'Form.lProgress' �wi 'visible' 0
'Form.lProgress' �wi 'caption' ''