SEXED_CHECK
:IF �� 'Y' = combo_species['Form.lsSpecies' �wi 'value';4]
    'Form.ckSexed' �wi ('enabled' 1) 
    'Form.ckSexed' �wi ('value'   1)
:ELSE
    'Form.ckSexed' �wi ('value'   0) 
    'Form.ckSexed' �wi ('enabled' 0)    
:ENDIF    