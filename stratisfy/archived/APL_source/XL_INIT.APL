XL_INIT output;I;data;tmpx

'Form.lProgress' �wi 'caption' ('Creating Excel Workbook') � Progress label updated.

� Initialize APL to Excel COM support
� Creates objects: "xl" & "xl.wkbooks"
� Connect to Excel Application

tmpx�'xl' �wi 'Delete'
:IF 0=(0��'xl' �wi 'self')
    tmpx�'xl' �wi 'New' 'Excel.Application'  � Application Object
:ENDIF

tmpx�'xl' �wi 'SheetsInNewWorkbook' (�output)
tmpx�'xl' �wi 'xWorkbooks > xl.wkbooks'  � Workbook Object
tmpx�'xl.wkbooks' �wi 'XAdd>xl.wkbk'   

�Loop through each page and insert data

:FOR I :IN ���output
    data�2�I�output

    tmpx�XL_ACTIVATEWORKSHEET I
    � Assign name to sheet
    'xl.wksht' �wi 'xName' (�1�I�output)

    � Set range for the data to be entered into.
    tmpx�'xl.wksht' �wi 'xRange > xl.wksht.r' (RC�A1 1 1,(��data))
    � Enter data into range.
    'xl.wksht.r' �wi 'value2' (�data)  � changed from value to value2 to work with WinXP
:ENDFOR