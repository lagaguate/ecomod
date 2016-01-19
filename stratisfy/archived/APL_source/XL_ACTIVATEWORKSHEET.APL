rÑXL_ACTIVATEWORKSHEET wksht;åelx;tmpx

© Activate specific sheet in active workbook
© eg.   XL_Activate 3 or XL_Activate 'Sheet2'
© creates <xl.wksht> if successful
© rÑ0 if success 1 otherwise
  
rÑ1 ™ åelxÑ'Öèexit'
tmpxÑ('xl' åwi 'ActiveWorkbook') åwi 'Worksheets>xl.wkshts'
tmpxÑ'xl.wkshts' åwi 'Item>xl.wksht' wksht
    'xl.wksht'  åwi 'Activate'
rÑ0
èexit: 'xl.wkshts' åwi 'Delete'