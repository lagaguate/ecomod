R�ZEROS_TO_NULLS data;I

� Change zeros to VT NULLS
:for I :in �1��data � for each row
   data[I;(data[I;]=0)/��data[I;]]�('#' �wi 'VT' 0 1) � change zeros to NULLS
:endfor
R�data