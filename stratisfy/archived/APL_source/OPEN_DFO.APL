OPEN_DFO;tmp;I;uid;pwd;dsn;r
© Setup selection lists.
© DFO Data Only. 

'Form.lProgress' Œwi 'visible' 1
'Form.lProgress' Œwi 'caption' 'Creating Selection Lists'

'Form' Œwi 'pointer' 11 © Hourglass pointer. 

:if 1¬(tmp„½'Form.s1' Œwi 'children')
    :for I :in ¯1‡1+¼tmp
        ('Form.s1.pg',(•I)) Œwi 'Delete'
    :endfor
:endif
'Form.s1' Œwi 'value' 1

'Form.cbSpread' Œwi 'value' 1
'Form.lExptype' Œwi 'visible' 1
'Form.lsExptype' Œwi 'visible' 1
'Form.lExpTypeNMFS' Œwi 'visible' 0
'Form.llessorequal' Œwi 'visible' 0
'Form.edExpTypeNMFS' Œwi 'visible' 0
'Form.ckVesselNetConversion' Œwi 'visible' 0

© Set style for mission list box.
'Form.s1.pg1.lsMission' Œwi 'style' (1 16 128)

© Retrieve the data for the Mission combo box.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT DISTINCT(MISSION) FROM GROUNDFISH.GSINF ORDER BY MISSION'

combo_mission „ 'Ado.Rs' Œwi 'GetRows'
'Form.s1.pg1.lsMission' Œwi 'list' (combo_mission[;1])

© Retrieve the data for the Area combo box.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT DISTINCT(AREA) FROM GROUNDFISH.GSINF ORDER BY AREA'
combo_area „ 'Ado.Rs' Œwi 'GetRows'
combo_area„combo_area[(1‰­¨combo_area[;1])/¼½combo_area[;1];] © remove nested VT
'Form.lsUnitArea' Œwi 'list' (combo_area[;1])
'Form.fAlk.lsAlkUnit' Œwi 'list' (combo_area[;1])

© Retrieve the data for the Strata combo box.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT DISTINCT(STRAT) FROM GROUNDFISH.GSINF ORDER BY STRAT'
combo_strata „ 'Ado.Rs' Œwi 'GetRows'
combo_strata„combo_strata[(1‰­¨combo_strata[;1])/¼½combo_strata[;1];] © remove nested VT
'Form.lsStrata' Œwi 'list' (combo_strata[;1])
'Form.fAlk.lsAlkStrata' Œwi 'list' (combo_strata[;1])

© Retrieve the data for the Species combo box.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT DISTINCT(SPEC), (CNAME), (LGRP), (LFSEXED) FROM GROUNDFISH.GSSPEC ORDER BY CNAME'
combo_species „ 'Ado.Rs' Œwi 'GetRows'
combo_species „ combo_species[(9999¬combo_species[;1])/¼½combo_species[;1];] © get rid of 'allspecies'
'Form.lsSpecies' Œwi 'list' (combo_species[;2])

© Retrieve the data for the Exptype list.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT DISTINCT(TYPE) FROM GROUNDFISH.GSINF ORDER BY TYPE'
list_exptype „ 'Ado.Rs' Œwi 'GetRows'
list_exptype„list_exptype[(1‰­¨list_exptype[;1])/¼½list_exptype[;1];] © remove nested VT
'Form.lsExptype' Œwi 'list' (•list_exptype)
'Form.lsExptype' Œwi 'value' ((1=list_exptype[;1])/¼½list_exptype[;1])

'Form' Œwi 'pointer' 0 © Switch back to regular pointer.

'Form.lProgress' Œwi 'visible' 0
'Form.lProgress' Œwi 'caption' ''