r�XL_ACTIVATEWORKSHEET wksht;�elx;tmpx

� Activate specific sheet in active workbook
� eg.   XL_Activate 3 or XL_Activate 'Sheet2'
� creates <xl.wksht> if successful
� r�0 if success 1 otherwise
  
r�1 � �elx�'��exit'
tmpx�('xl' �wi 'ActiveWorkbook') �wi 'Worksheets>xl.wkshts'
tmpx�'xl.wkshts' �wi 'Item>xl.wksht' wksht
    'xl.wksht'  �wi 'Activate'
r�0
�exit: 'xl.wkshts' �wi 'Delete'