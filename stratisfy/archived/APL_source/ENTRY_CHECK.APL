Z�ENTRY_CHECK mission;list;tmp
�� Ensures that all required fields on the Form are filled out. 

list�0�0

:IF Э mission �wi 'value'
    list� list, �tcnl, 'Mission not selected for:,'�mission
:ENDIF

:IF Э 'Form.lsStrata'  �wi 'value'
    list� list, �tcnl, 'Strata not selected.'
:ENDIF

:IF Э 'Form.lsSpecies' �wi 'value'
    list� list, �tcnl, 'Species not selected.'
:ENDIF

:IF    0='Form.opDFO'  �wi 'value'
:ANDIF 0='Form.opNMFS' �wi 'value'
    list� list, �tcnl, 'Select DFO or NMFS.'
:ENDIF

:IF 0=�'Form.edDistance' �wi 'text'
    list�list, �tcnl, 'Distance not set.'
:ENDIF

:IF 0=�'Form.cbSpread' �wi 'text'
    list�list, �tcnl, 'Spread not set'.
:ENDIF

:IF 0=�'Form.cbStratumTable' �wi 'text'
    list�list, �tcnl, 'Stratum table not set.'
:ENDIF

:if Э'Form.fAlk.lsAlkStrata' �wi 'value'
:andif 0<�'Form.fAlk.lsAlkUnit' �wi 'value'
    list�list, �tcnl, 'Area, but not Strata set for ALK modifications.'
:endif

:IF Эlist
Z�0
�0
:ENDIF

Z�1
POPUP list