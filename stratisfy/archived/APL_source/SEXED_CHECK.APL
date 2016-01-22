SEXED_CHECK
:IF –• 'Y' = combo_species['Form.lsSpecies' Œwi 'value';4]
    'Form.ckSexed' Œwi ('enabled' 1) 
    'Form.ckSexed' Œwi ('value'   1)
:ELSE
    'Form.ckSexed' Œwi ('value'   0) 
    'Form.ckSexed' Œwi ('enabled' 0)    
:ENDIF    