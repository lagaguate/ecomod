Qlx;r;�elx;combo_species;combo_mission;combo_strata;combo_area;list_exptype
� Startup function

�elx� "ON_ERROR"
startdir�(�CHDIR '') � save start up directory

�
� Database Logon form
�
:IF 0��'fmLogon' �wi 'self' � If logon form exists delete it.
   r�'fmLogon' �wi 'Delete'
:ENDIF  
r�'fmLogon' �wi 'New' fmLogon_def 
r�'fmLogon' �wi 'visible' 1
r�'fmLogon' �wi 'Wait'