Z凟NTRY_CHECK mission;list;tmp
⿸ Ensures that all required fields on the Form are filled out. 

listﭬ

:IF 协 mission 寃i 'value'
    list� list, 宼cnl, 'Mission not selected for:,'昺ission
:ENDIF

:IF 协 'Form.lsStrata'  寃i 'value'
    list� list, 宼cnl, 'Strata not selected.'
:ENDIF

:IF 协 'Form.lsSpecies' 寃i 'value'
    list� list, 宼cnl, 'Species not selected.'
:ENDIF

:IF    0='Form.opDFO'  寃i 'value'
:ANDIF 0='Form.opNMFS' 寃i 'value'
    list� list, 宼cnl, 'Select DFO or NMFS.'
:ENDIF

:IF 0=�'Form.edDistance' 寃i 'text'
    list刲ist, 宼cnl, 'Distance not set.'
:ENDIF

:IF 0=�'Form.cbSpread' 寃i 'text'
    list刲ist, 宼cnl, 'Spread not set'.
:ENDIF

:IF 0=�'Form.cbStratumTable' 寃i 'text'
    list刲ist, 宼cnl, 'Stratum table not set.'
:ENDIF

:if 协'Form.fAlk.lsAlkStrata' 寃i 'value'
:andif 0<�'Form.fAlk.lsAlkUnit' 寃i 'value'
    list刲ist, 宼cnl, 'Area, but not Strata set for ALK modifications.'
:endif

:IF 协list
Z�0
�0
:ENDIF

Z�1
POPUP list