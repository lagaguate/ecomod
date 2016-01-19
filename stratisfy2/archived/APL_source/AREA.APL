R„AREA ;MAT;XX;SS;tmp;tot;proportion;var_proportion;total_area;var_area
© Author:Travis Shepherd
© Creation Date: Mar. 20, 2003
MAT„data[;3]
XX„6 EXTRACT_COL ¨MAT © extract total wgt from data as nested array
tmp„data[;1]
tmp„tmp, œnmsq AREA_STRATUM ¨XX  © get stratum proportion present and variances
total_area„+/¹tmp[;2] © calculate total area
var_area„+/¹tmp[;3]   © calculate total variance
proportion„total_area÷+/nmsq © calculate stratified proportion present
var_proportion„var_area÷((+/nmsq)*2) © calculate variance of proortion present
R„(,total_area) (,var_area) (,proportion) (,var_proportion) (tmp) © put data in returned variable