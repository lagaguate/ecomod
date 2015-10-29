XL_INIT output;I;data;tmpx

'Form.lProgress' åwi 'caption' ('Creating Excel Workbook') © Progress label updated.

© Initialize APL to Excel COM support
© Creates objects: "xl" & "xl.wkbooks"
© Connect to Excel Application

tmpxÑ'xl' åwi 'Delete'
:IF 0=(0¨Ω'xl' åwi 'self')
    tmpxÑ'xl' åwi 'New' 'Excel.Application'  © Application Object
:ENDIF

tmpxÑ'xl' åwi 'SheetsInNewWorkbook' (Ωoutput)
tmpxÑ'xl' åwi 'xWorkbooks > xl.wkbooks'  © Workbook Object
tmpxÑ'xl.wkbooks' åwi 'XAdd>xl.wkbk'   

©Loop through each page and insert data

:FOR I :IN ≤ºΩoutput
    dataÑ2úIúoutput

    tmpxÑXL_ACTIVATEWORKSHEET I
    © Assign name to sheet
    'xl.wksht' åwi 'xName' (ú1úIúoutput)

    © Set range for the data to be entered into.
    tmpxÑ'xl.wksht' åwi 'xRange > xl.wksht.r' (RCèA1 1 1,(≥Ωdata))
    © Enter data into range.
    'xl.wksht.r' åwi 'value2' (≥data)  © changed from value to value2 to work with WinXP
:ENDFOR