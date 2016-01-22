Z„ENTRY_CHECK mission;list;tmp
©’ Ensures that all required fields on the Form are filled out. 

list„0½0

:IF Ð­ mission Œwi 'value'
    list„ list, Œtcnl, 'Mission not selected for:,'•mission
:ENDIF

:IF Ð­ 'Form.lsStrata'  Œwi 'value'
    list„ list, Œtcnl, 'Strata not selected.'
:ENDIF

:IF Ð­ 'Form.lsSpecies' Œwi 'value'
    list„ list, Œtcnl, 'Species not selected.'
:ENDIF

:IF    0='Form.opDFO'  Œwi 'value'
:ANDIF 0='Form.opNMFS' Œwi 'value'
    list„ list, Œtcnl, 'Select DFO or NMFS.'
:ENDIF

:IF 0=½'Form.edDistance' Œwi 'text'
    list„list, Œtcnl, 'Distance not set.'
:ENDIF

:IF 0=½'Form.cbSpread' Œwi 'text'
    list„list, Œtcnl, 'Spread not set'.
:ENDIF

:IF 0=½'Form.cbStratumTable' Œwi 'text'
    list„list, Œtcnl, 'Stratum table not set.'
:ENDIF

:if Ð­'Form.fAlk.lsAlkStrata' Œwi 'value'
:andif 0<½'Form.fAlk.lsAlkUnit' Œwi 'value'
    list„list, Œtcnl, 'Area, but not Strata set for ALK modifications.'
:endif

:IF Ð­list
Z„0
…0
:ENDIF

Z„1
POPUP list