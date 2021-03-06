DATA;r;uid;pwd;dsn;�elx

�
� Check the logon then create a database connection and intiate selection form.
�

�elx� " POPUP 'Unable to access the Oracle database.',�tcnl,�tcnl,�DM � � CLEANUP"

uid � 'fmLogon.edUserName' �wi 'text' � get username 
pwd � 'fmLogon.edPassword' �wi 'text' � get password
dsn � 'fmLogon.edDatabase' �wi 'text' � get database

:IF (0�(�'Ado' �wi 'self'))     � Determine if a connection object exists
  r� 'Ado' �wi 'Delete'     � remove connection ActiveObject
:ENDIF   

:IF (0=(�'Ado' �wi 'self'))     � Determine if connection already exists
  r� 'Ado' �wi 'New' 'ADODB.Connection'     � Create a database connection
  �'Ado' �wi 'xConnectionString' ('Provider=MSDAORA;Data Source=',dsn,';User Id=',uid,';Password=',pwd,';') � using OLEDB MDAC
  'Ado' �wi 'xConnectionString' ('Provider=OraOLEDB.Oracle;Data Source=',dsn,';User Id=',uid,';Password=',pwd,';') � using OLEDB ODAC
  'Ado' �wi 'xOpen'   � Open connection.
:ENDIF

�elx� "ON_ERROR"

:IF 0��'fmLogon' �wi 'self' � If logon form exists delete it.
   r�'fmLogon' �wi 'Delete'
:ENDIF

�
� Initiate the main form
�
:IF 0=�'Form' �wi 'self' � determine if selection form exists
   r�'Form' �wi 'Delete'
:ENDIF
r�'Form' �wi 'New' Form_def 
r�'Form' �wi 'caption' ('STRANAL ',version)
r�'Form.opDFO' �wi 'value' 1     
r�'Form' �wi 'visible' 1
r�'Form' �wi 'Wait'

CLEANUP:
� cleanup
:IF (0�(�'Ado' �wi 'self'))     � Determine if a connection object exists
  r� 'Ado' �wi 'Delete'     � remove connection ActiveObject
:ENDIF
