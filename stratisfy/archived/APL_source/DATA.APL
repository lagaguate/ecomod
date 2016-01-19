DATA;r;uid;pwd;dsn;Œelx

©
© Check the logon then create a database connection and intiate selection form.
©

Œelx„ " POPUP 'Unable to access the Oracle database.',Œtcnl,Œtcnl,ŒDM ª … CLEANUP"

uid „ 'fmLogon.edUserName' Œwi 'text' © get username 
pwd „ 'fmLogon.edPassword' Œwi 'text' © get password
dsn „ 'fmLogon.edDatabase' Œwi 'text' © get database

:IF (0¬(½'Ado' Œwi 'self'))     © Determine if a connection object exists
  r„ 'Ado' Œwi 'Delete'     © remove connection ActiveObject
:ENDIF   

:IF (0=(½'Ado' Œwi 'self'))     © Determine if connection already exists
  r„ 'Ado' Œwi 'New' 'ADODB.Connection'     © Create a database connection
  ©'Ado' Œwi 'xConnectionString' ('Provider=MSDAORA;Data Source=',dsn,';User Id=',uid,';Password=',pwd,';') © using OLEDB MDAC
  'Ado' Œwi 'xConnectionString' ('Provider=OraOLEDB.Oracle;Data Source=',dsn,';User Id=',uid,';Password=',pwd,';') © using OLEDB ODAC
  'Ado' Œwi 'xOpen'   © Open connection.
:ENDIF

Œelx„ "ON_ERROR"

:IF 0¬½'fmLogon' Œwi 'self' © If logon form exists delete it.
   r„'fmLogon' Œwi 'Delete'
:ENDIF

©
© Initiate the main form
©
:IF 0=½'Form' Œwi 'self' © determine if selection form exists
   r„'Form' Œwi 'Delete'
:ENDIF
r„'Form' Œwi 'New' Form_def 
r„'Form' Œwi 'caption' ('STRANAL ',version)
r„'Form.opDFO' Œwi 'value' 1     
r„'Form' Œwi 'visible' 1
r„'Form' Œwi 'Wait'

CLEANUP:
© cleanup
:IF (0¬(½'Ado' Œwi 'self'))     © Determine if a connection object exists
  r„ 'Ado' Œwi 'Delete'     © remove connection ActiveObject
:ENDIF
