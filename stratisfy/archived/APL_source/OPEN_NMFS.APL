OPEN_NMFS;tmp;I;uid;pwd;dsn;r
© Setup selection lists.
© NFMS Data Only. 

'Form.lProgress' Œwi 'visible' 1
'Form.lProgress' Œwi 'caption' 'Creating Selection Lists'

'Form' Œwi 'pointer' 11 © Hourglass pointer. 

:if 1¬(tmp„½'Form.s1' Œwi 'children')
    :for I :in ¯1‡1+¼tmp
        ('Form.s1.pg',(•I)) Œwi 'Delete'
    :endfor
:endif
'Form.s1' Œwi 'value' 1

'Form.cbSpread' Œwi 'value' 2
'Form.lExptype' Œwi 'visible' 0
'Form.lsExptype' Œwi 'visible' 0
'Form.lExpTypeNMFS' Œwi 'visible' 1
'Form.llessorequal' Œwi 'visible' 1
'Form.edExpTypeNMFS' Œwi 'visible' 1
'Form.ckVesselNetConversion' Œwi 'visible' 1

© Set style for mission list box.
'Form.s1.pg1.lsMission' Œwi 'style' (0 16 128)

© Retrieve the data for the Mission combo box.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT CRUISE6 FROM USNEFSC.STRANL_CRUISE ORDER BY CRUISE6'

combo_mission „ 'Ado.Rs' Œwi 'GetRows'
combo_mission„combo_mission[(1=­¨combo_mission[;1])/¼½combo_mission[;1];] © Remove null values
'Form.s1.pg1.lsMission' Œwi 'list' (combo_mission[;1])

© Retrieve the data for the Area combo box.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT AREA FROM USNEFSC.STRANL_AREA ORDER BY AREA'
combo_area „ 'Ado.Rs' Œwi 'GetRows'
combo_area„combo_area[(1=­¨combo_area[;1])/¼½combo_area[;1];] © Remove null values
'Form.lsUnitArea' Œwi 'list' (combo_area[;1])
'Form.fAlk.lsAlkUnit' Œwi 'list' (combo_area[;1])

© Retrieve the data for the Strata combo box.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT DISTINCT(STRATUM) FROM USNEFSC.STRANL_STRATUM ORDER BY STRATUM'
combo_strata „ 'Ado.Rs' Œwi 'GetRows'
combo_strata„combo_strata[(1=­¨combo_strata[;1])/¼½combo_strata[;1];]
'Form.lsStrata' Œwi 'list' (combo_strata[;1])
'Form.fAlk.lsAlkStrata' Œwi 'list' (combo_strata[;1])

© Retrieve the data for the Species combo box.
'Ado' Œwi 'xExecute > Ado.Rs' 'SELECT DISTINCT(SPEC), (CNAME), (LGRP), (LFSEXED) FROM USNEFSC.USSPEC ORDER BY CNAME'
combo_species „ 'Ado.Rs' Œwi 'GetRows'
combo_species „ combo_species[(~(›'9999')­¨combo_species[;1])/¼½combo_species[;1];] © get rid of 'allspecies'
'Form.lsSpecies' Œwi 'list' (combo_species[;2])
'Form' Œwi 'pointer' 0 © 

'Form' Œwi 'pointer' 0 © Switch back to regular pointer.

'Form.lProgress' Œwi 'visible' 0
'Form.lProgress' Œwi 'caption' ''