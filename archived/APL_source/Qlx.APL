Qlx;r;Œelx;combo_species;combo_mission;combo_strata;combo_area;list_exptype
© Startup function

Œelx„ "ON_ERROR"
startdir„(ŒCHDIR '') © save start up directory

©
© Database Logon form
©
:IF 0¬½'fmLogon' Œwi 'self' © If logon form exists delete it.
   r„'fmLogon' Œwi 'Delete'
:ENDIF  
r„'fmLogon' Œwi 'New' fmLogon_def 
r„'fmLogon' Œwi 'visible' 1
r„'fmLogon' Œwi 'Wait'