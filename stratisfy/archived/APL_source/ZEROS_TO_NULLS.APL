R„ZEROS_TO_NULLS data;I

© Change zeros to VT NULLS
:for I :in ¼1†½data © for each row
   data[I;(data[I;]=0)/¼½data[I;]]„('#' Œwi 'VT' 0 1) © change zeros to NULLS
:endfor
R„data